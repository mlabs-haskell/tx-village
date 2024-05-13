module Ledger.Sim (LedgerSim, LedgerState (..), LedgerConfig (..), LedgerValidatorError (..), runLedgerSim, runLedgerSimWithState, submitTx) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State.Strict (StateT (runStateT), get, gets, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as S
import Data.String (fromString)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.Common.Versions qualified as Plutus
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
    Address (Address),
    Credential (PubKeyCredential, ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    Datum,
    DatumHash,
    EvaluationError,
    ExBudget,
    LogOutput,
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    POSIXTime (getPOSIXTime),
    POSIXTimeRange,
    PubKeyHash,
    Redeemer,
    ScriptForEvaluation,
    ScriptHash (ScriptHash),
    ScriptPurpose (Minting, Spending),
    TxId (TxId),
    TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
    TxInfo (
        TxInfo,
        txInfoData,
        txInfoId,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoRedeemers,
        txInfoReferenceInputs,
        txInfoSignatories,
        txInfoValidRange
    ),
    TxOut (txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
    TxOutRef (TxOutRef),
    Value,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusMap

import Codec.Serialise (serialise)
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (find, fold)
import Data.Functor (void)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Set (Set)
import Data.Traversable (for)
import Ledger.Sim.Types.Config (
    LedgerConfig (lc'evaluationContext, lc'slotConfig),
 )
import Ledger.Sim.Types.Slot (Slot (Slot, getSlot), SlotConfig (SlotConfig, sc'slotLength, sc'slotZeroTime), slotToPOSIXTimeRange)
import Ledger.Sim.Types.TxInfo (TxInfoWithScripts (TxInfoWithScripts, txInfoRaw, txInfoScripts))
import PlutusTx.Builtins qualified as PlutusTx

data LedgerState = LedgerState
    { ls'currentSlot :: !Slot
    , ls'utxos :: !(Map TxOutRef TxOut)
    , ls'existingScripts :: !(Map ScriptHash ScriptForEvaluation)
    -- ^ Scripts which are stored on the chain by a reference script utxo.
    }

type LedgerSim = StateT LedgerState LedgerValidator

type LedgerValidator = ReaderT LedgerConfig (Except LedgerValidatorError)

data LedgerValidatorError
    = -- | The transaction is outside its validity range.
      TxInvalidRange {lve'txSlot :: !Slot, lve'txValidityRange :: POSIXTimeRange}
    | -- | The specified input in the transaction does not exist in the ledger utxo set.
      TxNonExistentInput {lve'txOutRef :: !TxOutRef}
    | -- | The owner of a utxo being spent has not signed this transaction.
      TxMissingOwnerSignature {lve'pubKeyHash :: !PubKeyHash}
    | -- | Transaction is not balanced.
      TxUnbalanced {lve'txInputValue :: !Value, lve'txMint :: !Value, lve'txOutputValue :: !Value}
    | -- | Datum for an input being spent is missing.
      TxMissingDatum {lve'txDatumHash :: !DatumHash}
    | -- | The policy for a token in the mint field was not invoked.
      TxMissingPolicyInvocation {lve'policyId :: !CurrencySymbol}
    | -- | The validator for an utxo being spent was not invoked.
      TxMissingValidatorInvocation {lve'utxoRef :: !TxOutRef}
    | -- | A script being invoked was not present in the witnesses.
      TxMissingScript {lve'scriptHash :: !ScriptHash}
    | -- | A script being invoked is present on the chain, but the associated utxo was not referenced.
      TxMissingReference {lve'scriptHash :: !ScriptHash}
    | -- | A script for a new reference script output was not provided.
      TxMissingReferenceScript {lve'scriptHash :: !ScriptHash}
    | -- | The input is at a script address but no datum has been provided during its creation.
      TxUnspendableInput {lve'utxoRef :: !TxOutRef}
    | TxScriptFailure {lve'scriptLogs :: LogOutput, lve'evalError :: EvaluationError}
    | -- | Stake/Reward based script purposes are not currently supported.
      TxUnsupportedScriptPurpose
    | -- | Some absurd TxInfo construction.
      TxInfoAbsurd {lve'absurd :: !String}

runLedgerSim :: LedgerConfig -> Map TxOutRef TxOut -> LedgerSim a -> Either LedgerValidatorError a
runLedgerSim ledgerCfg existingUtxos =
    runLedgerSimWithState
        ledgerCfg
        LedgerState
            { ls'currentSlot = Slot . sc'slotZeroTime $ lc'slotConfig ledgerCfg
            , ls'utxos = existingUtxos
            , ls'existingScripts = M.empty
            }

runLedgerSimWithState :: LedgerConfig -> LedgerState -> LedgerSim a -> Either LedgerValidatorError a
runLedgerSimWithState ledgerCfg ledgerState =
    fmap fst
        . runIdentity
        . runExceptT
        . flip runReaderT ledgerCfg
        . flip runStateT ledgerState

{- Known shortcomings:
- Tx id must be generated and is not generated by hashing the tx body (how it works on the chain).
  This is because we don't have a real Cardano.Api TxBody available at hand and creating one from PLA would
  require a lot of effort.
  As a stopgap, Tx id is generated from current POSIX time. This may violate the assumptions of any scripts that
  use hashing inside them. ex: script that checks `txId txInfo != blake2b_224 slot` where slot is some slot from validity range.
- See: 'checkTx'
-}
submitTx :: TxInfoWithScripts -> LedgerSim ()
submitTx TxInfoWithScripts{txInfoRaw, txInfoScripts} = do
    currentSlot <- gets ls'currentSlot
    let txInfo = txInfoRaw{txInfoId = genTxId currentSlot}
    ledgerState <- get
    -- TODO(chase): Find a way to utilize tx script budget.
    _txBudget <- lift . checkTx ledgerState $ TxInfoWithScripts txInfo txInfoScripts
    updateUtxos txInfo
    saveReferenceScripts $ TxInfoWithScripts txInfo txInfoScripts
    incrementSlot

{- Note on missing validations - Chase

- TX size (size of cbor serialized txbody)
- Budget and size within limits
-}
checkTx :: LedgerState -> TxInfoWithScripts -> LedgerValidator ExBudget
checkTx
    LedgerState
        { ls'currentSlot
        , ls'utxos
        , ls'existingScripts
        }
    TxInfoWithScripts
        { txInfoRaw =
            txInfo@TxInfo
                { txInfoValidRange
                , txInfoReferenceInputs
                , txInfoRedeemers
                , txInfoOutputs
                , txInfoMint
                , txInfoInputs
                , txInfoData
                , txInfoSignatories
                }
        , txInfoScripts
        } = do
        checkValidity
        checkUtxosExist
        checkSpendable
        checkNewReferenceScripts
        checkBalance
        checkScriptsInvoked

        fmap fold . traverse evalScript $ PlutusMap.toList txInfoRedeemers
      where
        datumMap = M.fromList $ PlutusMap.toList txInfoData

        checkValidity :: LedgerValidator ()
        checkValidity = do
            slotConfig <- asks lc'slotConfig
            unless (txInfoValidRange `contains` slotToPOSIXTimeRange slotConfig ls'currentSlot)
                . throwLVE
                $ TxInvalidRange ls'currentSlot txInfoValidRange

        checkUtxosExist :: LedgerValidator ()
        checkUtxosExist = do
            let inps = txInfoInputs <> txInfoReferenceInputs
            unlessExists TxNonExistentInput (M.keysSet ls'utxos) $ txInInfoOutRef <$> inps

        checkSpendable :: LedgerValidator ()
        checkSpendable = do
            let keys = S.fromList txInfoSignatories
            unlessExists TxMissingOwnerSignature keys $ mapMaybe (pubKeyHashFromAddress . txOutAddress . txInInfoResolved) txInfoInputs

        checkNewReferenceScripts :: LedgerValidator ()
        checkNewReferenceScripts = do
            unlessExists TxMissingReferenceScript (M.keysSet txInfoScripts) $ mapMaybe txOutReferenceScript txInfoOutputs

        checkBalance :: LedgerValidator ()
        checkBalance = do
            let inpValue = foldMap (txOutValue . txInInfoResolved) txInfoInputs
                outValue = foldMap txOutValue txInfoOutputs
            unless (inpValue <> txInfoMint == outValue)
                . throwLVE
                $ TxUnbalanced inpValue txInfoMint outValue

        checkScriptsInvoked :: LedgerValidator ()
        checkScriptsInvoked = do
            let invokedScripts = PlutusMap.keys txInfoRedeemers
                invokedPolicies = S.fromList . flip mapMaybe invokedScripts $ \case
                    Minting cs -> Just cs
                    _ -> Nothing
                invokedValidatorRefs = S.fromList . flip mapMaybe invokedScripts $ \case
                    Spending ref -> Just ref
                    _ -> Nothing
            unlessExists TxMissingPolicyInvocation invokedPolicies . filter (/= fromString "") $ Value.symbols txInfoMint
            unlessExists TxMissingValidatorInvocation invokedValidatorRefs . map txInInfoOutRef $
                filter (isScriptAddress . txOutAddress . txInInfoResolved) txInfoInputs

        getScript :: ScriptPurpose -> LedgerValidator (ScriptForEvaluation, Maybe Datum)
        getScript (Minting (CurrencySymbol (ScriptHash -> sh))) = do
            script <- lookupScript sh
            pure (script, Nothing)
        getScript (Spending ref) = do
            (txInInfoResolved -> utxo) <-
                maybe (throwLVE $ TxInfoAbsurd "Spending an utxo that is not an input") pure $
                    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ref) txInfoInputs
            sh <- case txOutAddress utxo of
                Address (ScriptCredential sh) _ -> pure sh
                _ -> throwLVE $ TxInfoAbsurd "absurd: TxInfo is spending a script input from a pub key address"
            script <- lookupScript sh
            datm <- case txOutDatum utxo of
                OutputDatumHash dh -> maybe (throwLVE $ TxMissingDatum dh) pure $ dh `M.lookup` datumMap
                OutputDatum datm -> pure datm
                NoOutputDatum -> throwLVE $ TxUnspendableInput ref
            pure (script, Just datm)
        getScript _ = throwLVE TxUnsupportedScriptPurpose

        evalScript :: (ScriptPurpose, Redeemer) -> LedgerValidator ExBudget
        evalScript (purpose, rdmr) = do
            (script, mDatm) <- getScript purpose
            evalCtx <- asks lc'evaluationContext
            let (logs, res) =
                    Plutus.evaluateScriptCounting
                        Plutus.PlutusV2
                        Plutus.vasilPV
                        Plutus.Verbose
                        evalCtx
                        script
                        $ maybeToList (PlutusTx.toData <$> mDatm) <> [PlutusTx.toData rdmr, PlutusTx.toData txInfo]
            case res of
                Left err -> throwLVE $ TxScriptFailure logs err
                Right budget -> pure budget

        lookupScript :: ScriptHash -> LedgerValidator ScriptForEvaluation
        lookupScript sh = do
            let scriptFromWitness = sh `M.lookup` txInfoScripts
                scriptFromReference = sh `M.lookup` ls'existingScripts
            case (scriptFromWitness, scriptFromReference) of
                -- If we find the script in the witnesses, we are done.
                (Just script, _) -> pure script
                -- If we find the script in the chain, we must make sure the reference utxo for it is also in the tx.
                (_, Just script) ->
                    if any ((== Just sh) . txOutReferenceScript . txInInfoResolved) txInfoReferenceInputs
                        then pure script
                        else throwLVE $ TxMissingReference sh
                _ -> throwLVE $ TxMissingScript sh

        isScriptAddress (Address (ScriptCredential _) _) = True
        isScriptAddress _ = False
        pubKeyHashFromAddress (Address (PubKeyCredential pkh) _) = Just pkh
        pubKeyHashFromAddress _ = Nothing

updateUtxos :: TxInfo -> LedgerSim ()
updateUtxos TxInfo{txInfoId, txInfoInputs, txInfoOutputs} = do
    -- Remove spend utxos.
    modify' $ \st -> st{ls'utxos = M.withoutKeys (ls'utxos st) . S.fromList $ map txInInfoOutRef txInfoInputs}
    -- Add newly created utxos.
    modify' $ \st ->
        st
            { ls'utxos =
                M.fromList (zipWith (\ix utxo -> (TxOutRef txInfoId ix, utxo)) [0 ..] txInfoOutputs) <> ls'utxos st
            }

saveReferenceScripts :: TxInfoWithScripts -> LedgerSim ()
saveReferenceScripts TxInfoWithScripts{txInfoRaw = TxInfo{txInfoOutputs}, txInfoScripts} = do
    modify' $ \st ->
        st
            { ls'existingScripts =
                M.restrictKeys txInfoScripts (S.fromList $ mapMaybe txOutReferenceScript txInfoOutputs)
                    <> ls'existingScripts st
            }

incrementSlot :: LedgerSim ()
incrementSlot = do
    SlotConfig{sc'slotLength} <- lift $ asks lc'slotConfig
    modify' $ \st -> st{ls'currentSlot = Slot $ getSlot (ls'currentSlot st) + sc'slotLength}

-- | Make sure every `k` in `f k` exists in the given set.
unlessExists :: (Traversable f, Ord k) => (k -> LedgerValidatorError) -> Set k -> f k -> LedgerValidator ()
unlessExists errF m l = void . for l $ \k -> unless (k `S.member` m) . throwLVE $ errF k

throwLVE :: LedgerValidatorError -> LedgerValidator a
throwLVE = lift . throwE

{- | Generate tx id from slot. NOTE: This is simply a stopgap measure used in the simulator. In reality,
tx ids are hashes of a transaction body.
-}
genTxId :: Slot -> TxId
genTxId =
    TxId
        . PlutusTx.toBuiltin
        . convert @_ @ByteString
        . hashWith Blake2b_224
        . LBS.toStrict
        . serialise
        . getPOSIXTime
        . getSlot
