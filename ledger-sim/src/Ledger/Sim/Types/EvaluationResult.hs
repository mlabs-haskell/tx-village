module Ledger.Sim.Types.EvaluationResult (
  EvaluationResult (..),
  evaluationResult'isFailure,
  EvaluationOutcome (..),
) where

import PlutusLedgerApi.V2 (
  EvaluationError,
  ExBudget,
  LogOutput,
  ScriptHash,
  ScriptPurpose,
 )

data EvaluationResult = EvaluationResult
  { evaluationResult'scriptHash :: ScriptHash
  , evaluationResult'scriptPurpose :: ScriptPurpose
  , evaluationResult'logOutput :: LogOutput
  , evaluationResult'outcome :: EvaluationOutcome
  }
  deriving stock (Show, Eq)

evaluationResult'isFailure :: EvaluationResult -> Bool
evaluationResult'isFailure result = case evaluationResult'outcome result of
  EvaluationOutcome'Failure _ -> True
  _ -> False

data EvaluationOutcome
  = EvaluationOutcome'Success ExBudget
  | EvaluationOutcome'Failure EvaluationError
  deriving stock (Show, Eq)
