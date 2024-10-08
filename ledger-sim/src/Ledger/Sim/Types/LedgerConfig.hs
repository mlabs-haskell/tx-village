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

data ScriptMode = ScriptMode'AsWitness | ScriptMode'AsReference

data LedgerConfig ctx = LedgerConfig
  { lc'evaluationContext :: Plutus.EvaluationContext
  , lc'scriptStorage :: !(Map PlutusV2.ScriptHash (ScriptMode, PlutusV2.ScriptForEvaluation))
  , lc'maxTxExBudget :: Maybe ExBudget
  , lc'appCtx :: !ctx
  }

-- | A ledger config built with 'practicalSlotConfig' and the given cost model.
mkLedgerConfig ::
  Map PlutusV2.ScriptHash (ScriptMode, PlutusV2.ScriptForEvaluation) ->
  PlutusCostModel ->
  Maybe ExBudget ->
  ctx ->
  Either PlutusV2.CostModelApplyError (LedgerConfig ctx)
mkLedgerConfig scripts costModel exBudget userCtx = do
  evalContext <- mkEvaluationContext costModel
  pure $ LedgerConfig evalContext scripts exBudget userCtx
