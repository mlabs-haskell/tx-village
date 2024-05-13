module Ledger.Sim (LedgerState (..), LedgerConfig (..), LedgerValidator, LedgerSim, LedgerValidatorError (..), checkTx) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.State.Strict (StateT)
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
    Address (Address, addressCredential),
    Credential (ScriptCredential),
    CurrencySymbol (CurrencySymbol),
    Datum,
    DatumHash,
    EvaluationError,
    ExBudget,
    LogOutput,
    OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
    POSIXTimeRange,
    Redeemer,
    ScriptForEvaluation,
    ScriptHash (ScriptHash),
    ScriptPurpose (Minting, Spending),
    TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
    TxInfo (
        TxInfo,
        txInfoData,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoRedeemers,
        txInfoReferenceInputs,
        txInfoValidRange
    ),
    TxOut (txOutAddress, txOutDatum, txOutValue),
    TxOutRef,
    Value,
 )
import PlutusTx.AssocMap qualified as PlutusMap
import PlutusTx.IsData qualified as PlutusTx

import Data.Foldable (find, fold)
import Data.Functor (void)
import Data.Set (Set)
import Data.Traversable (for)
import Ledger.Sim.Types.Config (
    LedgerConfig (lc'evaluationContext, lc'slotConfig),
 )
import Ledger.Sim.Types.Slot (Slot, slotToPOSIXTimeRange)

data LedgerState = LedgerState
    { ls'currentSlot :: !Slot
    , ls'utxos :: !(Map TxOutRef TxOut)
    , ls'witnessScripts :: !(Map ScriptHash ScriptForEvaluation)
    -- ^ Scripts which were either referenced (by an input) or explicitly attached in the tx.
    -- NOTE: This is NOT a property of the ledger. It's supposed to be a property of the transaction.
    -- Putting this field in 'LedgerState' is therefore a misnomer. It should ideally be in the transaction.
    }

type LedgerSim = StateT LedgerState LedgerValidator

type LedgerValidator = ReaderT LedgerConfig (Except LedgerValidatorError)

data LedgerValidatorError
    = -- | The transaction is outside its validity range.
      TxInvalidRange {lve'txSlot :: !Slot, lve'txValidityRange :: POSIXTimeRange}
    | -- | The specified input in the transaction does not exist in the ledger utxo set.
      TxNonExistentInput {lve'txOutRef :: !TxOutRef}
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
    | -- | The input is at a script address but no datum has been provided during its creation.
      TxUnspendableInput {lve'utxoRef :: !TxOutRef}
    | TxScriptFailure {lve'scriptLogs :: LogOutput, lve'evalError :: EvaluationError}
    | -- | Stake/Reward based script purposes are not currently supported.
      TxUnsupportedScriptPurpose
    | -- | Some absurd TxInfo construction.
      TxInfoAbsurd {lve'absurd :: !String}

{- Note on missing validations - Chase

- TX size (size of cbor serialized txbody)
- Budget and size within limits
- Verify that if output has script hash, the reference script exists in witnesses (but these witnesses don't exist in txinfo)
-}
checkTx :: LedgerState -> TxInfo -> LedgerValidator ExBudget
checkTx
    LedgerState
        { ls'currentSlot
        , ls'utxos
        , ls'witnessScripts
        }
    txInfo@TxInfo
        { txInfoValidRange
        , txInfoReferenceInputs
        , txInfoRedeemers
        , txInfoOutputs
        , txInfoMint
        , txInfoInputs
        , txInfoData
        } = do
        checkValidity
        checkUtxosExist
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
            script <- maybe (throwLVE $ TxMissingScript sh) pure $ sh `M.lookup` ls'witnessScripts
            pure (script, Nothing)
        getScript (Spending ref) = do
            (txInInfoResolved -> utxo) <-
                maybe (throwLVE $ TxInfoAbsurd "Spending an utxo that is not an input") pure $
                    find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ref) txInfoInputs
            sh <- case txOutAddress utxo of
                Address (ScriptCredential sh) _ -> pure sh
                _ -> throwLVE $ TxInfoAbsurd "absurd: TxInfo is spending a script input from a pub key address"
            script <- maybe (throwLVE $ TxMissingScript sh) pure $ sh `M.lookup` ls'witnessScripts
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

        isScriptAddress (Address{addressCredential = ScriptCredential _}) = True
        isScriptAddress _ = False

unlessExists :: (Traversable f, Ord k) => (k -> LedgerValidatorError) -> Set k -> f k -> LedgerValidator ()
unlessExists errF m l = void . for l $ \k -> unless (k `S.member` m) . throwLVE $ errF k

throwLVE :: LedgerValidatorError -> LedgerValidator a
throwLVE = lift . throwE
