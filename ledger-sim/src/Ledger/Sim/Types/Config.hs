module Ledger.Sim.Types.Config (LedgerConfig (..), PlutusCostModel (..), ledgerConfigWithCostModel) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map (Map)
import Data.Map.Strict qualified as M

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as PlutusV2

import Ledger.Sim.Types.Slot (SlotConfig, practicalSlotConfig)

newtype PlutusCostModel = PlutusCostModel (Map PlutusV2.ParamName Integer)

data LedgerConfig = LedgerConfig
    { lc'slotConfig :: SlotConfig
    , lc'evaluationContext :: Plutus.EvaluationContext
    }

-- | A ledger config built with 'practicalSlotConfig' and the given cost model.
ledgerConfigWithCostModel :: PlutusCostModel -> Either PlutusV2.CostModelApplyError LedgerConfig
ledgerConfigWithCostModel (PlutusCostModel costModel) =
    LedgerConfig practicalSlotConfig
        <$> runIdentity (runExceptT . fmap fst . runWriterT $ PlutusV2.mkEvaluationContext (M.elems costModel))
