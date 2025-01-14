{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- The '-fno-worker-wrapper' GHC option prevents the error:
-- "GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: *"
-- Because Plutus can't handle unboxed tuples which come from worker/wrapper
{-# OPTIONS_GHC -fno-worker-wrapper #-}
{-# OPTIONS_GHC -Wno-orphans        #-}

module Ledger.TimeSlot(
  SlotConfig(..)
, slotRangeToPOSIXTimeRange
, slotToPOSIXTimeRange
, slotToBeginPOSIXTime
, slotToEndPOSIXTime
, posixTimeRangeToSlotRange
, posixTimeToEnclosingSlot
, currentSlot
) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Default              (Default (def))
import qualified Data.Time.Clock           as Time
import qualified Data.Time.Clock.POSIX     as Time
import           GHC.Generics              (Generic)
import           Plutus.V1.Ledger.Interval (Interval (Interval, ivFrom, ivTo), interval)
import           Plutus.V1.Ledger.Slot     (Slot (Slot), SlotRange)
import           Plutus.V1.Ledger.Time     (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import           PlutusTx.Lift             (makeLift)
import           PlutusTx.Prelude          hiding (Eq, (<$>))
import           Prelude                   (Eq, IO, Show, (<$>))
import qualified Prelude                   as Haskell

-- | Datatype to configure the length of one slot and the beginning of the
-- first slot.
data SlotConfig =
    SlotConfig
        { scSlotLength   :: Integer -- ^ Length (number of seconds) of 1 slot
        , scZeroSlotTime :: POSIXTime -- ^ Beginning of the first slot
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

makeLift ''SlotConfig

instance Default SlotConfig where
  {-# INLINABLE def #-}
  def = SlotConfig{ scSlotLength = 1, scZeroSlotTime = POSIXTime beginningOfTime }

{-# INLINABLE beginningOfTime #-}
-- | 'beginningOfTime' corresponds to the Shelley launch date
-- (2020-07-29T21:44:51Z) which is 1596059091 in POSIX time
-- (number of seconds since 1970-01-01T00:00:00Z).
beginningOfTime :: Integer
beginningOfTime = 1596059091

{-# INLINABLE slotRangeToPOSIXTimeRange #-}
-- | Convert a 'SlotRange' to a 'POSIXTimeRange' given a 'SlotConfig'. The
-- resulting 'POSIXTimeRange' refers to the starting time of the lower bound of
-- the 'SlotRange' and the ending time of the upper bound of the 'SlotRange'.
slotRangeToPOSIXTimeRange :: SlotConfig -> SlotRange -> POSIXTimeRange
slotRangeToPOSIXTimeRange sc sr =
  let lbound = fmap (slotToBeginPOSIXTime sc) $ ivFrom sr
      ubound = fmap (slotToEndPOSIXTime sc) $ ivTo sr
   in Interval lbound ubound

{-# INLINABLE slotToPOSIXTimeRange #-}
-- | Convert a 'Slot' to a 'POSIXTimeRange' given a 'SlotConfig'. Each 'Slot'
-- can be represented by an interval of time.
slotToPOSIXTimeRange :: SlotConfig -> Slot -> POSIXTimeRange
slotToPOSIXTimeRange sc slot =
  interval (slotToBeginPOSIXTime sc slot) (slotToEndPOSIXTime sc slot)

{-# INLINABLE slotToBeginPOSIXTime #-}
-- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime SlotConfig{scSlotLength, scZeroSlotTime} (Slot n) =
  let secondsAfterBegin = n * scSlotLength
   in POSIXTime $ getPOSIXTime scZeroSlotTime + secondsAfterBegin

{-# INLINABLE slotToEndPOSIXTime #-}
-- | Get the ending 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToEndPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToEndPOSIXTime sc@SlotConfig{scSlotLength} slot =
  slotToBeginPOSIXTime sc slot + POSIXTime (scSlotLength - 1)

{-# INLINABLE posixTimeRangeToSlotRange #-}
-- | Convert a 'POSIXTimeRange' to 'SlotRange' given a 'SlotConfig'. This gives
-- the smallest slot range that entirely contains the given time range.
posixTimeRangeToSlotRange :: SlotConfig -> POSIXTimeRange -> SlotRange
posixTimeRangeToSlotRange sc = fmap (posixTimeToEnclosingSlot sc)

{-# INLINABLE posixTimeToEnclosingSlot #-}
-- | Convert a 'POSIXTime' to 'Slot' given a 'SlotConfig'.
posixTimeToEnclosingSlot :: SlotConfig -> POSIXTime -> Slot
posixTimeToEnclosingSlot SlotConfig{scSlotLength, scZeroSlotTime} (POSIXTime t) =
  let timePassed = t - getPOSIXTime scZeroSlotTime
      slotsPassed = divide timePassed scSlotLength
  in Slot slotsPassed

-- | Get the current slot number
currentSlot :: SlotConfig -> IO Slot
currentSlot sc = timeToSlot <$> Time.getPOSIXTime
    where
      timeToSlot = posixTimeToEnclosingSlot sc
                 . POSIXTime
                 . Haskell.floor
                 . Time.nominalDiffTimeToSeconds

