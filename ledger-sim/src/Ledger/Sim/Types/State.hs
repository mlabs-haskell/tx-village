module Ledger.Sim.Types.State (LedgerState (..)) where

import Data.Map (Map)
import PlutusLedgerApi.V2 (POSIXTime, TxOut, TxOutRef)

data LedgerState st = LedgerState
    { ls'currentTime :: !POSIXTime
    , ls'utxos :: !(Map TxOutRef TxOut)
    , ls'userState :: !st
    }
