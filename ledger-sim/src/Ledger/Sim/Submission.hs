module Ledger.Sim.Submission (submit) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Reader (MonadTrans (lift), asks)
import Control.Monad.State (MonadState (get), modify')
import Control.Monad.Writer (MonadWriter (tell), WriterT, execWriterT)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Traversable (for)
import Ledger.Sim.Types.EvaluationResult (
  EvaluationOutcome (EvaluationOutcome'Failure, EvaluationOutcome'Success),
  EvaluationResult (EvaluationResult, evaluationResult'outcome),
 )
import Ledger.Sim.Types.LedgerConfig (LedgerConfig (lc'evaluationContext, lc'maxTxExBudget, lc'scriptStorage))
import Ledger.Sim.Types.LedgerState (LedgerState (ls'currentTime, ls'utxos))
import Ledger.Sim.Types.Submission (
  Submission,
  SubmissionEnv (submissionEnv'config, submissionEnv'txInfo),
  SubmissionError (SubmissionError'Evaluation, SubmissionError'MaxTxExBudgetExceeded, SubmissionError'Validation),
  SubmissionResult (SubmissionResult, submissionResult'EvaluationResults, submissionResult'TotalTxExBudget, submissionResult'TxId),
 )
import Ledger.Sim.Utils.Hashing (hashScriptV2)
import Ledger.Sim.Validation (runTxInfoValidation)
import Ledger.Sim.Validation.Validator (Validity (Invalid, Valid))
import PlutusLedgerApi.Common.Versions (vasilPV)
import PlutusLedgerApi.V2 (
  Address (addressCredential),
  Credential (ScriptCredential),
  CurrencySymbol (unCurrencySymbol),
  Datum,
  DatumHash,
  ExBudget (exBudgetCPU, exBudgetMemory),
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  Redeemer,
  ScriptContext (ScriptContext),
  ScriptForEvaluation,
  ScriptHash (ScriptHash),
  ScriptPurpose (Minting, Spending),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoData, txInfoId, txInfoInputs, txInfoOutputs, txInfoRedeemers),
  TxOut (txOutAddress, txOutDatum),
  TxOutRef (TxOutRef),
  VerboseMode (Verbose),
  evaluateScriptCounting,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

asksTxInfo :: (Monad m) => (TxInfo -> a) -> Submission ctx st e m a
asksTxInfo f = asks $ f . submissionEnv'txInfo

asksConfig :: (Monad m) => (LedgerConfig ctx -> a) -> Submission ctx st e m a
asksConfig f = asks $ f . submissionEnv'config

submit :: (Monad m) => Submission ctx st e m SubmissionResult
submit = do
  txId <- asksTxInfo txInfoId
  validate
  evalResults <- evaluate
  totalExBudget <- checkEvaluationResults evalResults
  updateLedgerState
  pure $
    SubmissionResult
      { submissionResult'TxId = txId
      , submissionResult'EvaluationResults = evalResults
      , submissionResult'TotalTxExBudget = totalExBudget
      }

validate :: (Monad m) => Submission ctx st e m ()
validate = do
  state <- get
  config <- asks submissionEnv'config
  txInfo <- asks submissionEnv'txInfo

  case runTxInfoValidation config state txInfo of
    Valid -> pure ()
    Invalid reasons ->
      throwError $
        SubmissionError'Validation reasons

evaluate :: (Monad m) => Submission ctx st e m [EvaluationResult]
evaluate = do
  redeemers <- asksTxInfo $ AssocMap.toList . txInfoRedeemers
  execWriterT $ traverse (uncurry evaluateRedeemer) redeemers

checkEvaluationResults :: (Monad m) => [EvaluationResult] -> Submission ctx st e m ExBudget
checkEvaluationResults evalResults = do
  maxTxExBudget <- asksConfig lc'maxTxExBudget

  exBudgets <-
    for evalResults $ \result -> case evaluationResult'outcome result of
      EvaluationOutcome'Failure _ -> throwError $ SubmissionError'Evaluation evalResults
      EvaluationOutcome'Success exBudget -> pure exBudget

  let totalExBudget = mconcat exBudgets

  case maxTxExBudget of
    Just maxTxExBudget' ->
      let maxCPU = exBudgetCPU maxTxExBudget'
          maxMemory = exBudgetMemory maxTxExBudget'

          cpu = exBudgetCPU totalExBudget
          memory = exBudgetMemory totalExBudget
       in when (cpu > maxCPU || memory > maxMemory) $
            throwError $
              SubmissionError'MaxTxExBudgetExceeded maxTxExBudget' totalExBudget evalResults
    Nothing -> pure ()

  pure totalExBudget

evaluateRedeemer ::
  (Monad m) =>
  ScriptPurpose ->
  Redeemer ->
  WriterT [EvaluationResult] (Submission ctx st e m) ()
evaluateRedeemer p red = do
  result <- lift $ case p of
    Minting cs -> evaluateMintingRedeemer cs red
    Spending ref -> evaluateSpendingRedeemer ref red
    _ -> shouldHaveBeenCaughtByValidation "unsupported script purpose"

  tell [result]

evaluateMintingRedeemer :: (Monad m) => CurrencySymbol -> Redeemer -> Submission ctx st e m EvaluationResult
evaluateMintingRedeemer cs red = do
  script <- mustResolveMintingPolicy cs
  evaluatePlutusScript Nothing (Minting cs) red script

evaluateSpendingRedeemer :: (Monad m) => TxOutRef -> Redeemer -> Submission ctx st e m EvaluationResult
evaluateSpendingRedeemer txOutRef red = do
  (script, datum) <- mustResolveValidatorAndDatum txOutRef
  evaluatePlutusScript (Just datum) (Spending txOutRef) red script

mustResolveMintingPolicy :: (Monad m) => CurrencySymbol -> Submission ctx st e m ScriptForEvaluation
mustResolveMintingPolicy cs = mustFindScriptByHash $ ScriptHash $ unCurrencySymbol cs

mustResolveValidatorAndDatum :: (Monad m) => TxOutRef -> Submission ctx st e m (ScriptForEvaluation, Datum)
mustResolveValidatorAndDatum txOutRef = do
  inputs <- asksTxInfo txInfoInputs

  txOut <- case L.find ((== txOutRef) . txInInfoOutRef) inputs of
    Just i -> pure $ txInInfoResolved i
    Nothing -> shouldHaveBeenCaughtByValidation "input with the given reference not found"

  datum <- case txOutDatum txOut of
    OutputDatum d -> pure d
    OutputDatumHash h -> mustFindDatumByHash h
    NoOutputDatum -> shouldHaveBeenCaughtByValidation "cannot spend script input without datum"

  scriptHash <- case addressCredential $ txOutAddress txOut of
    ScriptCredential h -> pure h
    _ -> shouldHaveBeenCaughtByValidation "expected script credential, got pub key"

  script <- mustFindScriptByHash scriptHash

  pure (script, datum)

mustFindDatumByHash :: (Monad m) => DatumHash -> Submission ctx st e m Datum
mustFindDatumByHash h = do
  dat <- asksTxInfo txInfoData

  case AssocMap.lookup h dat of
    Just d -> pure d
    Nothing -> shouldHaveBeenCaughtByValidation "datum with the given hash not found"

mustFindScriptByHash :: (Monad m) => ScriptHash -> Submission ctx st e m ScriptForEvaluation
mustFindScriptByHash h = do
  scriptStorage <- asks $ lc'scriptStorage . submissionEnv'config

  case M.lookup h scriptStorage of
    Nothing -> shouldHaveBeenCaughtByValidation "script with the given hash not found in script storage"
    Just s -> pure $ snd s

shouldHaveBeenCaughtByValidation :: String -> Submission ctx st e m a
shouldHaveBeenCaughtByValidation msg =
  error $
    "ledger-sim: " <> msg <> " This is a bug; it should have been caught during validation"

evaluatePlutusScript ::
  (Monad m) =>
  Maybe Datum ->
  ScriptPurpose ->
  Redeemer ->
  ScriptForEvaluation ->
  Submission ctx st e m EvaluationResult
evaluatePlutusScript datum purpose redeemer script = do
  txInfo <- asks submissionEnv'txInfo
  evalCtx <- asksConfig lc'evaluationContext

  let args =
        catMaybes
          [ PlutusTx.toData <$> datum
          , Just $ PlutusTx.toData redeemer
          , Just $ PlutusTx.toData scriptContext
          ]

      scriptContext = ScriptContext txInfo purpose

      (logOutput, evalResultRaw) = evaluateScriptCounting vasilPV Verbose evalCtx script args

      outcome = case evalResultRaw of
        Left err -> EvaluationOutcome'Failure err
        Right exBudget -> EvaluationOutcome'Success exBudget

      scriptHash = hashScriptV2 script

      evalResult = EvaluationResult scriptHash purpose logOutput outcome

  pure evalResult

-- TODO(chfanghr): Use lens please
modifyUtxos :: (Monad m) => (Map TxOutRef TxOut -> Map TxOutRef TxOut) -> Submission ctx st e m ()
modifyUtxos f = modify' $ \st -> st {ls'utxos = f $ ls'utxos st}

updateUtxos :: (Monad m) => Submission ctx st e m ()
updateUtxos = do
  TxInfo {txInfoId, txInfoInputs, txInfoOutputs} <- asks submissionEnv'txInfo
  -- Remove spent utxos.
  modifyUtxos $
    flip M.withoutKeys $
      S.fromList $
        map txInInfoOutRef txInfoInputs

  -- Add newly created utxos.
  modifyUtxos $
    mappend $
      M.fromList $
        zipWith
          (\ix utxo -> (TxOutRef txInfoId ix, utxo))
          [0 ..]
          txInfoOutputs

incrementSlot :: (Monad m) => Submission ctx st e m ()
incrementSlot =
  modify' $ \st -> st {ls'currentTime = ls'currentTime st + 1}

updateLedgerState :: (Monad m) => Submission ctx st e m ()
updateLedgerState = updateUtxos >> incrementSlot
