module Ledger.Sim.Types.Config (LedgerConfig (..), PlutusCostModel (..), mkLedgerConfig) where

import Data.Map (Map)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as PlutusV2

import Ledger.Sim.Types.CostModel (PlutusCostModel (PlutusCostModel), mkEvaluationContext)

data LedgerConfig = LedgerConfig
    { lc'evaluationContext :: Plutus.EvaluationContext
    , lc'scriptStorage :: !(Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation)
    }

-- | A ledger config built with 'practicalSlotConfig' and the given cost model.
mkLedgerConfig ::
    Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation ->
    PlutusCostModel ->
    Either PlutusV2.CostModelApplyError LedgerConfig
mkLedgerConfig scripts costModel =
    LedgerConfig
        <$> mkEvaluationContext costModel
        <*> pure scripts
