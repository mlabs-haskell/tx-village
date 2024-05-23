module Ledger.Sim.Types.Config (LedgerConfig (..), PlutusCostModel (..), mkLedgerConfig) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import Data.Map.Strict qualified as M

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as PlutusV2

newtype PlutusCostModel = PlutusCostModel (Map PlutusV2.ParamName Integer)

data LedgerConfig = LedgerConfig
    { lc'evaluationContext :: Plutus.EvaluationContext
    , lc'scriptStorage :: !(Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation)
    }

-- | A ledger config built with 'practicalSlotConfig' and the given cost model.
mkLedgerConfig ::
    Map PlutusV2.ScriptHash PlutusV2.ScriptForEvaluation ->
    PlutusCostModel ->
    Either PlutusV2.CostModelApplyError LedgerConfig
mkLedgerConfig scripts (PlutusCostModel costModel) =
    LedgerConfig
        <$> runIdentity (runExceptT . fmap fst . runWriterT $ PlutusV2.mkEvaluationContext (M.elems costModel))
        <*> pure scripts
