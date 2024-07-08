module Ledger.Sim.Utils.Fee (
  calcScriptFee,
  evaluationResult'fee,
  submissionResult'totalFee,
  calcExBudgtRatio,
  calcScriptFeeRatio,
) where

import Control.Composition (on)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Ledger.Sim.Types.EvaluationResult (
  EvaluationOutcome (
    EvaluationOutcome'Success
  ),
  EvaluationResult (evaluationResult'outcome),
 )
import Ledger.Sim.Types.Prices (ExBudgetRatio (ExBudgetRatio, exBudgetRatio'Cpu, exBudgetRatio'Mem), Prices (prMem, prStep))
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

exBudgetToNumPair :: (Num a) => ExBudget -> (a, a)
exBudgetToNumPair =
  liftA2
    (,)
    (fromSatInt . unExCpu . exBudgetCPU)
    (fromSatInt . unExMem . exBudgetMemory)

calcScriptFee :: Prices -> ExBudget -> Integer
calcScriptFee price exBudget =
  let (cpu, mem) = exBudgetToNumPair exBudget
   in ceiling $
        prStep price * cpu + prMem price * mem

evaluationResult'fee :: Prices -> EvaluationResult -> Maybe Integer
evaluationResult'fee prices result = case evaluationResult'outcome result of
  EvaluationOutcome'Success budget -> Just $ calcScriptFee prices budget
  _ -> Nothing

submissionResult'totalFee :: Prices -> SubmissionResult -> Integer
submissionResult'totalFee prices =
  sum
    . mapMaybe (evaluationResult'fee prices)
    . submissionResult'EvaluationResults

calcExBudgtRatio :: ExBudget -> ExBudget -> ExBudgetRatio
calcExBudgtRatio a b =
  let (a'cpu, a'mem) = exBudgetToNumPair a
      (b'cpu, b'mem) = exBudgetToNumPair b
   in ExBudgetRatio
        { exBudgetRatio'Cpu = a'cpu % b'cpu
        , exBudgetRatio'Mem = a'mem % b'mem
        }

calcScriptFeeRatio ::
  Prices ->
  ExBudget ->
  ExBudget ->
  Rational
calcScriptFeeRatio prices = (%) `on` calcScriptFee prices
