module Ledger.Sim.Types.TxInfo (TxInfoWithScripts (..)) where

import Data.Map.Strict (Map)

import PlutusLedgerApi.V2 (ScriptForEvaluation, ScriptHash, TxInfo)

data TxInfoWithScripts = TxInfoWithScripts
    { txInfoRaw :: !TxInfo
    , txInfoScripts :: !(Map ScriptHash ScriptForEvaluation)
    }
