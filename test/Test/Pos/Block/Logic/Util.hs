-- | Utilities for block logic testing.

module Test.Pos.Block.Logic.Util
       ( bpGenBlocks
       , bpGoToArbitraryState
       , withCurrentSlot
       ) where

import           Universum

import           Test.QuickCheck.Gen       (sized)
import           Test.QuickCheck.Monadic   (pick)

import           Pos.Block.Types           (Blund)
import           Pos.Core                  (BlockCount, SlotId)
import           Pos.Generator.Block       (BlockGenParams (..), genBlocks)
import           Pos.Ssc.GodTossing        (SscGodTossing)
import           Pos.Util.Chrono           (OldestFirst)
import           Pos.Util.Util             (HasLens (..))

import           Test.Pos.Block.Logic.Mode (BlockProperty, BlockTestContext,
                                            BlockTestContextTag, btcSlotId_L,
                                            tpAllSecrets)

-- | Generate arbitrary valid blocks inside 'BlockProperty'. The first
-- argument specifies how many blocks should be generated. If it's
-- 'Nothing', the number of blocks will be generated by QuickCheck
-- engine.
bpGenBlocks :: Maybe BlockCount -> BlockProperty (OldestFirst [] (Blund SscGodTossing))
bpGenBlocks blkCnt = do
    allSecrets <- lift $ view (lensOf @BlockTestContextTag . tpAllSecrets)
    let genBlockGenParams s =
            pure
                BlockGenParams
                { _bgpSecrets = allSecrets
                , _bgpBlockCount = fromMaybe (fromIntegral s) blkCnt
                }
    params <- pick $ sized genBlockGenParams
    lift (genBlocks params)

-- | Go to arbitrary global state in 'BlockProperty'.
bpGoToArbitraryState :: BlockProperty ()
-- TODO: generate arbitrary blocks, apply them.
bpGoToArbitraryState = pass

-- | Perform action pretending current slot is the given one.
withCurrentSlot :: MonadReader BlockTestContext m => SlotId -> m a -> m a
withCurrentSlot slot = local (set btcSlotId_L $ Just slot)
