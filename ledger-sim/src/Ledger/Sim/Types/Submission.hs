module Ledger.Sim.Types.Submission (
  SubmissionError (..),
  SubmissionEnv (..),
  Submission,
  SubmissionResult (..),
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Ledger.Sim.Types.EvaluationResult (EvaluationResult)
import Ledger.Sim.Types.LedgerConfig (LedgerConfig)
import Ledger.Sim.Types.LedgerState (LedgerState)
import Ledger.Sim.Validation (InvalidTxInfoError)
import PlutusLedgerApi.V2 (ExBudget, TxId, TxInfo)

data SubmissionError
  = SubmissionError'Validation [InvalidTxInfoError]
  | SubmissionError'Evaluation [EvaluationResult]
  | SubmissionError'MaxTxExBudgetExceeded
      ExBudget -- Maximum Total ExBudget
      ExBudget -- Actual Total ExBudget
      [EvaluationResult]
  deriving stock (Show, Eq)

data SubmissionEnv ctx = SubmissionEnv
  { submissionEnv'txInfo :: TxInfo
  , submissionEnv'config :: LedgerConfig ctx
  }

type Submission ctx st e m =
  ReaderT (SubmissionEnv ctx) (StateT (LedgerState st) (ExceptT SubmissionError m))

data SubmissionResult = SubmissionResult
  { submissionResult'TxId :: TxId
  , submissionResult'EvaluationResults :: [EvaluationResult]
  , submissionResult'TotalTxExBudget :: ExBudget
  }
