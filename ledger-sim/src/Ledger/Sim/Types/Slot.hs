{- Based on mlabs-haskell/plutus-simple-model -}
module Ledger.Sim.Types.Slot (Slot (..), SlotConfig (..), slotToPOSIXTimeRange, practicalSlotConfig) where

import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)

{- | The slot number. This is a good proxy for time, since on the Cardano blockchain
 slots pass at a constant rate.
-}
newtype Slot = Slot {getSlot :: Integer}
    deriving stock (Eq, Ord, Show)

{- | Datatype to configure the length (ms) of one slot and the beginning of the
 first slot.
-}
data SlotConfig = SlotConfig
    { sc'slotLength :: Integer
    -- ^ Length (number of milliseconds) of one slot
    , sc'slotZeroTime :: POSIXTime
    -- ^ Beginning of slot 0 (in milliseconds)
    }
    deriving stock (Eq, Show)

-- | Practical slot config for use in mainnet and official testnets (preprod and preview).
practicalSlotConfig :: SlotConfig
practicalSlotConfig =
    SlotConfig
        { sc'slotLength = 1000 -- 1 second
        , sc'slotZeroTime = 1596059091000 -- Shelley launch date (2020-07-29T21:44:51Z)
        }

{- | Convert a 'Slot' to a 'POSIXTimeRange' given a 'SlotConfig'. Each 'Slot'
 can be represented by an interval of time.
-}
slotToPOSIXTimeRange :: SlotConfig -> Slot -> POSIXTimeRange
slotToPOSIXTimeRange sc slot =
    interval (slotToBeginPOSIXTime sc slot) (slotToEndPOSIXTime sc slot)

-- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime SlotConfig{sc'slotLength, sc'slotZeroTime} (Slot n) =
    let msAfterBegin = n * sc'slotLength
     in POSIXTime $ getPOSIXTime sc'slotZeroTime + msAfterBegin

-- | Get the ending 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToEndPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToEndPOSIXTime sc@SlotConfig{sc'slotLength} slot =
    slotToBeginPOSIXTime sc slot + POSIXTime (sc'slotLength - 1)
