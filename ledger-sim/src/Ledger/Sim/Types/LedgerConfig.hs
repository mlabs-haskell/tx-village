module Ledger.Sim.Types.LedgerConfig (
  LedgerConfig (..),
  PlutusCostModel (..),
  ScriptMode (..),
  mkLedgerConfig,
) where

import Data.Map (Map)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as PlutusV2

import Ledger.Sim.Types.CostModel (PlutusCostModel (PlutusCostModel), mkEvaluationContext)
import PlutusLedgerApi.Common (ExBudget)

data ScriptMode = ScriptMode'AllowWitness | ScriptMode'MustBeReference

data LedgerConfig ctx = LedgerConfig
  { lc'evaluationContext :: Plutus.EvaluationContext
  , lc'scriptStorage :: !(Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation)
  , lc'maxExBudget :: Maybe ExBudget
  , lc'scriptMode :: ScriptMode
  , lc'appCtx :: !ctx
  }

-- | A ledger config built with 'practicalSlotConfig' and the given cost model.
mkLedgerConfig ::
  Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation ->
  PlutusCostModel ->
  Maybe ExBudget ->
  ScriptMode ->
  ctx ->
  Either PlutusV2.CostModelApplyError (LedgerConfig ctx)
mkLedgerConfig scripts costModel exBudget scriptWitnessMode userCtx = do
  evalContext <- mkEvaluationContext costModel
  pure $ LedgerConfig evalContext scripts exBudget scriptWitnessMode userCtx
