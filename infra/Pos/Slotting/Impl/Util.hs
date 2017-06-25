-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( approxSlotUsingOutdated
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units             (convertUnit)
import           NTP.Example                 ()

import qualified Pos.Core.Constants          as C
import           Pos.Core.Slotting           (flattenEpochIndex, unflattenSlotId)
import           Pos.Core.Types              (EpochIndex, SlotId (..), Timestamp (..),
                                              mkLocalSlotIndex)
import           Pos.Util.Util               (leftToPanic)

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), SlottingData (..))

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated :: MonadSlotsData m => EpochIndex -> Timestamp -> m SlotId
approxSlotUsingOutdated penult t = do
    SlottingData {..} <- getSlottingData
    systemStart <- getSystemStart
    let epochStart = systemStart + esdStartDifference sdLast
    pure $
        if | t < epochStart -> SlotId (penult + 1) minBound
           | otherwise      -> outdatedEpoch systemStart t (penult + 1) sdLast
  where
    outdatedEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp (esdStartDifference + systemStart) in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: MonadSlotsData m
    => Timestamp -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
    SlottingData {..} <- getSlottingData
    systemStart <- getSystemStart
    let tryEpoch = computeSlotUsingEpoch systemStart approxCurTime
    let penultRes = tryEpoch sdPenultEpoch sdPenult
    let lastRes = tryEpoch (succ sdPenultEpoch) sdLast
    return $ penultRes <|> lastRes

computeSlotUsingEpoch
    :: Timestamp
    -> Timestamp
    -> EpochIndex
    -> EpochSlottingData
    -> Maybe SlotId
computeSlotUsingEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..}
    | curTime < epochStart = Nothing
    | curTime < epochStart + duration * C.epochSlots = Just $ SlotId epoch localSlot
    | otherwise = Nothing
  where
    localSlotNumeric = fromIntegral $ (curTime - epochStart) `div` duration
    localSlot =
        leftToPanic "computeSlotUsingEpoch: " $
        mkLocalSlotIndex localSlotNumeric
    duration = convertUnit esdSlotDuration
    epochStart = getTimestamp (esdStartDifference + systemStart)
