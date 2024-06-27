module Ledger.Sim.Utils.Fee (
  calcScriptFee,
  evaluationResult'fee,
  submissionResult'totalFee,
) where

import Data.Maybe (mapMaybe)
import Ledger.Sim.Types.EvaluationResult (
  EvaluationOutcome (
    EvaluationOutcome'Success
  ),
  EvaluationResult (evaluationResult'outcome),
 )
import Ledger.Sim.Types.Prices (Prices (prMem, prStep))
import Ledger.Sim.Types.Submission (SubmissionResult (submissionResult'EvaluationResults))
import PlutusLedgerApi.V2 (
  ExBudget (exBudgetCPU, exBudgetMemory),
  ExCPU (ExCPU),
  ExMemory (ExMemory),
  SatInt,
  fromSatInt,
 )

unExCpu :: ExCPU -> SatInt
unExCpu (ExCPU n) = n

unExMem :: ExMemory -> SatInt
unExMem (ExMemory n) = n

calcScriptFee :: Prices -> ExBudget -> Integer
calcScriptFee price exBudget =
  ceiling $
    prStep price * fromSatInt (unExCpu $ exBudgetCPU exBudget)
      + prMem price * fromSatInt (unExMem $ exBudgetMemory exBudget)

evaluationResult'fee :: Prices -> EvaluationResult -> Maybe Integer
evaluationResult'fee prices result = case evaluationResult'outcome result of
  EvaluationOutcome'Success budget -> Just $ calcScriptFee prices budget
  _ -> Nothing

submissionResult'totalFee :: Prices -> SubmissionResult -> Integer
submissionResult'totalFee prices =
  sum
    . mapMaybe (evaluationResult'fee prices)
    . submissionResult'EvaluationResults
