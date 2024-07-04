module Ledger.Sim.Types.LedgerState (
  LedgerState (..),
  ledgerStateWithUtxos,
  ledgerStateWithUtxos',
  emptyLedgerState,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import PlutusLedgerApi.V2 (POSIXTime, TxInInfo (txInInfoOutRef, txInInfoResolved), TxOut, TxOutRef)

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

ledgerStateWithUtxos' :: [TxInInfo] -> st -> LedgerState st
ledgerStateWithUtxos' =
  ledgerStateWithUtxos
    . Map.fromList
    . fmap (liftA2 (,) txInInfoOutRef txInInfoResolved)

emptyLedgerState :: st -> LedgerState st
emptyLedgerState = ledgerStateWithUtxos mempty
