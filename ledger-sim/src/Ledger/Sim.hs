module Ledger.Sim (
    LedgerSim,
    LedgerState (..),
    LedgerConfig (..),
    LedgerValidatorError (..),
    ledgerStateWithUtxos,
    emptyLedgerState,
    runLedgerSim,
    submitTx,
    incrementSlot,
    lookupUTxO,
    throwLedgerError,
) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State.Strict (StateT (runStateT), get, gets, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Set qualified as S
import Data.String (fromString)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.Common.Versions qualified as Plutus
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
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
    ScriptContext (ScriptContext),
    ScriptForEvaluation,
    ScriptHash (ScriptHash),
    ScriptPurpose (Minting, Spending),
    TxId (TxId),
    TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
    TxInfo (
        TxInfo,
        txInfoData,
        txInfoFee,
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
    adaSymbol,
    adaToken,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusMap

import Codec.Serialise (serialise)
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Data.Bifunctor (second)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (find, fold, for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (sort, sortOn)
import Data.Set (Set)
import Data.Traversable (for)
import Ledger.Sim.Types.Config (
    LedgerConfig (lc'evaluationContext, lc'scriptStorage),
 )
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V1.Interval qualified as IV
import PlutusTx.Builtins qualified as PlutusTx

data LedgerState = LedgerState
    { ls'currentTime :: !POSIXTime
    , ls'utxos :: !(Map TxOutRef TxOut)
    }

ledgerStateWithUtxos :: Map TxOutRef TxOut -> LedgerState
ledgerStateWithUtxos existingUtxos =
    LedgerState
        { ls'currentTime = 1
        , ls'utxos = existingUtxos
        }

emptyLedgerState :: LedgerState
emptyLedgerState = ledgerStateWithUtxos mempty

type LedgerSim e = StateT LedgerState (LedgerValidator e)

type LedgerValidator e = ReaderT LedgerConfig (Except (LedgerValidatorError e))

data LedgerValidatorError e
    = -- | The transaction is outside its validity range.
      TxInvalidRange {lve'txTime :: !POSIXTime, lve'txValidityRange :: POSIXTimeRange}
    | -- | The specified input in the transaction does not exist in the ledger utxo set.
      TxNonExistentInput {lve'txOutRef :: !TxOutRef}
    | -- | The owner of a utxo being spent has not signed this transaction.
      TxMissingOwnerSignature {lve'pubKeyHash :: !PubKeyHash}
    | -- | Transaction is not balanced.
      TxUnbalanced {lve'txInputValue :: !Value, lve'txMint :: !Value, lve'txOutputValue :: !Value}
    | -- | Spending an utxo that is missing from the inputs.
      TxMissingInput {lve'txOutRef :: !TxOutRef}
    | -- | Datum for an input being spent is missing.
      TxMissingDatum {lve'txDatumHash :: !DatumHash}
    | -- | The policy for a token in the mint field was not invoked.
      TxMissingPolicyInvocation {lve'policyId :: !CurrencySymbol}
    | -- | The validator for an utxo being spent was not invoked.
      TxMissingValidatorInvocation {lve'utxoRef :: !TxOutRef}
    | -- | A script being invoked was not present in the witnesses.
      TxMissingScript {lve'scriptHash :: !ScriptHash}
    | -- | A script for a new reference script output was not provided.
      TxMissingReferenceScript {lve'scriptHash :: !ScriptHash}
    | -- | The input is at a script address but no datum has been provided during its creation.
      TxUnspendableInput {lve'utxoRef :: !TxOutRef}
    | TxScriptFailure {lve'scriptLogs :: LogOutput, lve'evalError :: EvaluationError}
    | -- | Stake/Reward based script purposes are not currently supported.
      TxUnsupportedScriptPurpose
    | TxNonNormalInputs {lve'inputs :: ![TxInInfo]}
    | TxNonNormalReferenceInputs {lve'inputs :: ![TxInInfo]}
    | TxNonNormalOutput {lve'output :: !TxOut}
    | TxNonNormalMint {lve'mint :: !Value}
    | TxNonNormalFee {lve'fee :: !Value}
    | TxNonNormalSignatories {lve'signatories :: ![PubKeyHash]}
    | TxNonNormalRedeemers {lve'redeemers :: !(PlutusMap.Map ScriptPurpose Redeemer)}
    | TxNonNormalData {lve'data :: !(PlutusMap.Map DatumHash Datum)}
    | -- | Some absurd TxInfo construction.
      TxInfoAbsurd {lve'absurd :: !String}
    | -- | Custom Application Errors.
      TxApplicationError {lve'appError :: !e}
    deriving stock (Eq, Show)

runLedgerSim :: LedgerConfig -> LedgerState -> LedgerSim e a -> Either (LedgerValidatorError e) a
runLedgerSim ledgerCfg ledgerState =
    fmap fst
        . runIdentity
        . runExceptT
        . flip runReaderT ledgerCfg
        . flip runStateT ledgerState

lookupUTxO :: TxOutRef -> LedgerSim e (Maybe TxOut)
lookupUTxO ref = M.lookup ref <$> gets ls'utxos

{- Known shortcomings:
- Tx id must be generated and is not generated by hashing the tx body (how it works on the chain).
  This is because we don't have a real Cardano.Api TxBody available at hand and creating one from PLA would
  require a lot of effort.
  As a stopgap, Tx id is generated from current POSIX time. This may violate the assumptions of any scripts that
  use hashing inside them. ex: script that checks `txId txInfo != blake2b_224 time` where time is some time from validity range.
- See: 'checkTx'
-}
submitTx :: TxInfo -> LedgerSim e ()
submitTx txInfoRaw = do
    currentTime <- gets ls'currentTime
    let txInfo = txInfoRaw{txInfoId = genTxId currentTime}
    ledgerState <- get
    -- TODO(chase): Find a way to utilize tx script budget.
    _txBudget <- lift $ checkTx ledgerState txInfo
    updateUtxos txInfo
    incrementSlot

checkNormality :: TxInfo -> LedgerValidator e ()
checkNormality
    TxInfo
        { txInfoReferenceInputs
        , txInfoRedeemers
        , txInfoSignatories
        , txInfoOutputs
        , txInfoMint
        , txInfoInputs
        , txInfoFee
        , txInfoData
        } = do
        unless (normalizeInputs txInfoInputs == txInfoInputs)
            . throwLVE
            $ TxNonNormalInputs txInfoInputs
        unless (normalizeInputs txInfoReferenceInputs == txInfoReferenceInputs)
            . throwLVE
            $ TxNonNormalReferenceInputs txInfoReferenceInputs
        unless (PlutusMap.fromListSafe (PlutusMap.toList txInfoRedeemers) == txInfoRedeemers)
            . throwLVE
            $ TxNonNormalRedeemers txInfoRedeemers
        unless (normalizeList txInfoSignatories == txInfoSignatories)
            . throwLVE
            $ TxNonNormalSignatories txInfoSignatories
        for_ txInfoOutputs $ \txOut -> do
            unless (normalizeTxOut txOut == txOut)
                . throwLVE
                $ TxNonNormalSignatories txInfoSignatories
        unless (normalizeValue txInfoMint == deconstructValue txInfoMint)
            . throwLVE
            $ TxNonNormalMint txInfoMint
        unless ([(adaSymbol, [(adaToken, Value.getLovelace $ Value.lovelaceValueOf txInfoFee)])] == deconstructValue txInfoFee)
            . throwLVE
            $ TxNonNormalFee txInfoFee
        unless (normalizeMap txInfoData == PlutusMap.toList txInfoData)
            . throwLVE
            $ TxNonNormalData txInfoData
      where
        normalizeInputs x = (\inp -> inp{txInInfoResolved = normalizeTxOut $ txInInfoResolved inp}) <$> sortOn txInInfoOutRef x
        normalizeTxOut x =
            x
                { txOutValue =
                    Value.Value
                        . PlutusMap.fromList
                        . fmap (second PlutusMap.fromList)
                        . normalizeValue
                        $ txOutValue x
                }
        normalizeList = S.toAscList . S.fromList
        normalizeMap :: (Ord k, Ord v) => PlutusMap.Map k v -> [(k, v)]
        normalizeMap = sort . PlutusMap.toList
        normalizeValue = padAda . normalizeMap . PlutusMap.mapWithKey (const normalizeMap) . Value.getValue
        deconstructValue = PlutusMap.toList . PlutusMap.mapWithKey (const PlutusMap.toList) . Value.getValue
        padAda ((cs, [(tk, amt)]) : rest)
            | cs == adaSymbol && tk == adaToken = (cs, [(tk, amt)]) : rest
        padAda rest = (adaSymbol, [(adaToken, 0)]) : rest

{- Note on missing validations - Chase

- TX size (size of cbor serialized txbody)
- Budget and size within limits
-}
checkTx :: LedgerState -> TxInfo -> LedgerValidator e ExBudget
checkTx
    LedgerState
        { ls'currentTime
        , ls'utxos
        }
    txInfo@TxInfo
        { txInfoValidRange
        , txInfoReferenceInputs
        , txInfoRedeemers
        , txInfoOutputs
        , txInfoMint
        , txInfoInputs
        , txInfoData
        , txInfoSignatories
        } = do
        checkNormality txInfo

        checkValidity
        checkUtxosExist
        checkSpendable
        checkNewReferenceScripts
        checkBalance
        checkScriptsInvoked

        fmap fold . traverse evalScript $ PlutusMap.toList txInfoRedeemers
      where
        datumMap = M.fromList $ PlutusMap.toList txInfoData

        checkValidity :: LedgerValidator e ()
        checkValidity = do
            unless (ls'currentTime `IV.member` txInfoValidRange)
                . throwLVE
                $ TxInvalidRange ls'currentTime txInfoValidRange

        checkUtxosExist :: LedgerValidator e ()
        checkUtxosExist = do
            let inps = txInfoInputs <> txInfoReferenceInputs
            unlessExists TxNonExistentInput (M.keysSet ls'utxos) $ txInInfoOutRef <$> inps

        checkSpendable :: LedgerValidator e ()
        checkSpendable = do
            let keys = S.fromList txInfoSignatories
            unlessExists TxMissingOwnerSignature keys $ mapMaybe (toPubKeyHash . txOutAddress . txInInfoResolved) txInfoInputs

        checkNewReferenceScripts :: LedgerValidator e ()
        checkNewReferenceScripts = do
            scriptStorage <- asks lc'scriptStorage
            unlessExists TxMissingReferenceScript (M.keysSet scriptStorage) $ mapMaybe txOutReferenceScript txInfoOutputs

        checkBalance :: LedgerValidator e ()
        checkBalance = do
            let inpValue = foldMap (txOutValue . txInInfoResolved) txInfoInputs
                outValue = foldMap txOutValue txInfoOutputs
            unless (inpValue <> txInfoMint == outValue)
                . throwLVE
                $ TxUnbalanced inpValue txInfoMint outValue

        checkScriptsInvoked :: LedgerValidator e ()
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
                filter (isJust . toScriptHash . txOutAddress . txInInfoResolved) txInfoInputs

        getScript :: ScriptPurpose -> LedgerValidator e (ScriptForEvaluation, Maybe Datum)
        getScript (Minting (CurrencySymbol (ScriptHash -> sh))) = do
            script <- lookupScript sh
            pure (script, Nothing)
        getScript (Spending ref) = do
            (txInInfoResolved -> utxo) <-
                maybe (throwLVE $ TxMissingInput ref) pure $
                    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ref) txInfoInputs
            sh <-
                maybe
                    (throwLVE $ TxInfoAbsurd "absurd: TxInfo is spending a script input from a pub key address")
                    pure
                    . toScriptHash
                    $ txOutAddress utxo
            script <- lookupScript sh
            datm <- case txOutDatum utxo of
                OutputDatumHash dh -> maybe (throwLVE $ TxMissingDatum dh) pure $ dh `M.lookup` datumMap
                OutputDatum datm -> pure datm
                NoOutputDatum -> throwLVE $ TxUnspendableInput ref
            pure (script, Just datm)
        getScript _ = throwLVE TxUnsupportedScriptPurpose

        evalScript :: (ScriptPurpose, Redeemer) -> LedgerValidator e ExBudget
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
                        $ maybeToList (PlutusTx.toData <$> mDatm)
                            <> [PlutusTx.toData rdmr, PlutusTx.toData $ ScriptContext txInfo purpose]
            case res of
                Left err -> throwLVE $ TxScriptFailure logs err
                Right budget -> pure budget

        lookupScript :: ScriptHash -> LedgerValidator e ScriptForEvaluation
        lookupScript sh = do
            scriptStorage <- asks lc'scriptStorage
            maybe (throwLVE $ TxMissingScript sh) pure $ sh `M.lookup` scriptStorage

updateUtxos :: TxInfo -> LedgerSim e ()
updateUtxos TxInfo{txInfoId, txInfoInputs, txInfoOutputs} = do
    -- Remove spend utxos.
    modify' $ \st -> st{ls'utxos = M.withoutKeys (ls'utxos st) . S.fromList $ map txInInfoOutRef txInfoInputs}
    -- Add newly created utxos.
    modify' $ \st ->
        st
            { ls'utxos =
                M.fromList (zipWith (\ix utxo -> (TxOutRef txInfoId ix, utxo)) [0 ..] txInfoOutputs) <> ls'utxos st
            }

incrementSlot :: LedgerSim e ()
incrementSlot = do
    modify' $ \st -> st{ls'currentTime = ls'currentTime st + 1}

-- | Make sure every `k` in `f k` exists in the given set.
unlessExists :: (Traversable f, Ord k) => (k -> LedgerValidatorError e) -> Set k -> f k -> LedgerValidator e ()
unlessExists errF m l = void . for l $ \k -> unless (k `S.member` m) . throwLVE $ errF k

throwLVE :: LedgerValidatorError e -> LedgerValidator e a
throwLVE = lift . throwE

-- | Throw custom application error.
throwLedgerError :: e -> LedgerSim e ()
throwLedgerError = lift . lift . throwE . TxApplicationError

{- | Generate tx id from time. NOTE: This is simply a stopgap measure used in the simulator. In reality,
tx ids are hashes of a transaction body.
-}
genTxId :: POSIXTime -> TxId
genTxId =
    TxId
        . PlutusTx.toBuiltin
        . convert @_ @ByteString
        . hashWith Blake2b_224
        . LBS.toStrict
        . serialise
        . getPOSIXTime
