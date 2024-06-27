module Ledger.Sim.Types.Submission (
  SubmissionError (..),
  SubmissionEnv (..),
  Submission,
  SubmissionResult (..),
) where

import Control.Monad.Except (Except)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Ledger.Sim.Types.EvaluationResult (EvaluationResult)
import Ledger.Sim.Types.LedgerSim.LedgerConfig (LedgerConfig)
import Ledger.Sim.Types.LedgerSim.LedgerState (LedgerState)
import Ledger.Sim.Validation (InvalidTxInfoError)
import PlutusLedgerApi.V2 (TxId, TxInfo)

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

data SubmissionResult = SubmissionResult
  { submissionResult'TxId :: TxId
  , submissionResult'EvaluationResults :: [EvaluationResult]
  }
