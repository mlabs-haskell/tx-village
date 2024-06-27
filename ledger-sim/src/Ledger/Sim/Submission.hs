module Ledger.Sim.Submission (
  EvaluationResult (..),
  EvaluationOutcome (..),
  SubmissionEnv (..),
  SubmissionError (..),
  Submission,
  submit,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (Except)
import Control.Monad.Reader (MonadTrans (lift), ReaderT, asks)
import Control.Monad.State (MonadState (get), StateT, modify')
import Control.Monad.Writer (MonadWriter (tell), WriterT, execWriterT)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Ledger.Sim.Types.Config (LedgerConfig (lc'evaluationContext, lc'maxExBudget, lc'scriptStorage))
import Ledger.Sim.Types.State (LedgerState (ls'currentTime, ls'utxos))
import Ledger.Sim.Utils.Hashing (hashScriptV2)
import Ledger.Sim.Validation (InvalidTxInfoError, runTxInfoValidation)
import Ledger.Sim.Validation.Validator (Validity (Invalid, Valid))
import PlutusLedgerApi.Common.Versions (vasilPV)
import PlutusLedgerApi.V2 (
  Address (addressCredential),
  Credential (ScriptCredential),
  CurrencySymbol (unCurrencySymbol),
  Datum,
  DatumHash,
  EvaluationError,
  ExBudget,
  LogOutput,
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
  evaluateScriptRestricting,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

data EvaluationResult = EvaluationResult
  { evaluationResult'scriptHash :: ScriptHash
  , evaluationResult'scriptPurpose :: ScriptPurpose
  , evaluationResult'logOutput :: LogOutput
  , evaluationResult'outcome :: EvaluationOutcome
  }
  deriving stock (Show, Eq)

data EvaluationOutcome
  = EvaluationOutcome'Success ExBudget
  | EvaluationOutcome'Failure EvaluationError
  deriving stock (Show, Eq)

isEvaluationFailure :: EvaluationOutcome -> Bool
isEvaluationFailure (EvaluationOutcome'Failure _) = True
isEvaluationFailure _ = False

data SubmissionError
  = SubmissionError'Validation [InvalidTxInfoError]
  | SubmissionError'Evaluation [EvaluationResult]
  deriving stock (Show, Eq)

data SubmissionEnv ctx = SubmissionEnv
  { submissionEnv'txInfo :: TxInfo
  , submissionEnv'config :: LedgerConfig ctx
  }

type Submission ctx st e =
  ReaderT (SubmissionEnv ctx) (StateT (LedgerState st) (Except SubmissionError))

asksTxInfo :: (TxInfo -> a) -> Submission ctx st e a
asksTxInfo f = asks $ f . submissionEnv'txInfo

asksConfig :: (LedgerConfig ctx -> a) -> Submission ctx st e a
asksConfig f = asks $ f . submissionEnv'config

submit :: Submission ctx st e [EvaluationResult]
submit = do
  validate
  r <- evaluate
  updateLedgerState
  pure r

validate :: Submission ctx st e ()
validate = do
  state <- get
  config <- asks submissionEnv'config
  txInfo <- asks submissionEnv'txInfo

  case runTxInfoValidation config state txInfo of
    Valid -> pure ()
    Invalid reasons ->
      throwError $
        SubmissionError'Validation reasons

evaluate :: Submission ctx st e [EvaluationResult]
evaluate = do
  redeemers <- asksTxInfo $ AssocMap.toList . txInfoRedeemers
  evaluationResults <- execWriterT $ traverse (uncurry evaluateRedeemer) redeemers

  case filter (isEvaluationFailure . evaluationResult'outcome) evaluationResults of
    [] -> pure evaluationResults
    failedResults -> throwError $ SubmissionError'Evaluation failedResults

evaluateRedeemer ::
  ScriptPurpose ->
  Redeemer ->
  WriterT [EvaluationResult] (Submission ctx st e) ()
evaluateRedeemer p red = do
  result <- lift $ case p of
    Minting cs -> evaluateMintingRedeemer cs red
    Spending ref -> evaluateSpendingRedeemer ref red
    _ -> shouldHaveBeenCaughtByValidation "unsupported script purpose"

  tell [result]

evaluateMintingRedeemer :: CurrencySymbol -> Redeemer -> Submission ctx st e EvaluationResult
evaluateMintingRedeemer cs red = do
  script <- mustResolveMintingPolicy cs
  evaluatePlutusScript Nothing (Minting cs) red script

evaluateSpendingRedeemer :: TxOutRef -> Redeemer -> Submission ctx st e EvaluationResult
evaluateSpendingRedeemer txOutRef red = do
  (script, datum) <- mustResolveValidatorAndDatum txOutRef
  evaluatePlutusScript (Just datum) (Spending txOutRef) red script

mustResolveMintingPolicy :: CurrencySymbol -> Submission ctx st e ScriptForEvaluation
mustResolveMintingPolicy cs = mustFindScriptByHash $ ScriptHash $ unCurrencySymbol cs

mustResolveValidatorAndDatum :: TxOutRef -> Submission ctx st e (ScriptForEvaluation, Datum)
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

mustFindDatumByHash :: DatumHash -> Submission ctx st e Datum
mustFindDatumByHash h = do
  dat <- asksTxInfo txInfoData

  case AssocMap.lookup h dat of
    Just d -> pure d
    Nothing -> shouldHaveBeenCaughtByValidation "datum with the given hash not found"

mustFindScriptByHash :: ScriptHash -> Submission ctx st e ScriptForEvaluation
mustFindScriptByHash h = do
  scriptStorage <- asks $ lc'scriptStorage . submissionEnv'config

  case M.lookup h scriptStorage of
    Nothing -> shouldHaveBeenCaughtByValidation "script with the given hash not found in script storage"
    Just s -> pure s

shouldHaveBeenCaughtByValidation :: String -> Submission ctx st e a
shouldHaveBeenCaughtByValidation msg =
  error $
    "ledger-sim: " <> msg <> " This is a bug; it should have been caught during validation"

evaluatePlutusScript ::
  Maybe Datum ->
  ScriptPurpose ->
  Redeemer ->
  ScriptForEvaluation ->
  Submission ctx st e EvaluationResult
evaluatePlutusScript datum purpose redeemer script = do
  txInfo <- asks submissionEnv'txInfo
  evalCtx <- asksConfig lc'evaluationContext
  maxExBudget <- asksConfig lc'maxExBudget

  let args =
        catMaybes
          [ PlutusTx.toData <$> datum
          , Just $ PlutusTx.toData redeemer
          , Just $ PlutusTx.toData scriptContext
          ]

      scriptContext = ScriptContext txInfo purpose

      (logOutput, evalResultRaw) = case maxExBudget of
        Nothing -> evaluateScriptCounting vasilPV Verbose evalCtx script args
        Just exBudget -> evaluateScriptRestricting vasilPV Verbose evalCtx exBudget script args

      outcome = case evalResultRaw of
        Left err -> EvaluationOutcome'Failure err
        Right exBudget -> EvaluationOutcome'Success exBudget

      scriptHash = hashScriptV2 script

      evalResult = EvaluationResult scriptHash purpose logOutput outcome

  pure evalResult

-- TODO(chfanghr): Use lens please
modifyUtxos :: (Map TxOutRef TxOut -> Map TxOutRef TxOut) -> Submission ctx st e ()
modifyUtxos f = modify' $ \st -> st {ls'utxos = f $ ls'utxos st}

updateUtxos :: Submission ctx st e ()
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

incrementSlot :: Submission ctx st e ()
incrementSlot =
  modify' $ \st -> st {ls'currentTime = ls'currentTime st + 1}

updateLedgerState :: Submission ctx st e ()
updateLedgerState = updateUtxos >> incrementSlot
