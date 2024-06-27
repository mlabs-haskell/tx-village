module Ledger.Sim.Types.LedgerSim.LedgerState (
  LedgerState (..),
  ledgerStateWithUtxos,
  emptyLedgerState,
) where

import Data.Map (Map)
import PlutusLedgerApi.V2 (POSIXTime, TxOut, TxOutRef)

data LedgerState st = LedgerState
  { ls'currentTime :: !POSIXTime
  , ls'utxos :: !(Map TxOutRef TxOut)
  , ls'userState :: !st
  }

ledgerStateWithUtxos :: Map TxOutRef TxOut -> st -> LedgerState st
ledgerStateWithUtxos existingUtxos userState =
  LedgerState
    { ls'currentTime = 1
    , ls'utxos = existingUtxos
    , ls'userState = userState
    }

emptyLedgerState :: st -> LedgerState st
emptyLedgerState = ledgerStateWithUtxos mempty
