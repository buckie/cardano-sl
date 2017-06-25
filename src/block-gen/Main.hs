-- | Tool to generate database of node.

module Main where

import           Universum

import           Test.QuickCheck.Random (mkQCGen)
import           Formatting             (build, stext, (%), sformat)

import           Pos.Block.Logic        (BlockLrcMode (..), verifyAndApplyBlocks)

import           BlockGenOptions        (BlockGenOptions (..), getBlockGenOptions)

applyBlocks
    :: forall ssc m . BlockLrcMode ssc m
    => OldestFirst NE (Block ssc)
    -> m (Either Text HeaderHash)
applyBlocks = verifyAndApplyBlocks True

generateBlocks :: forall ssc . BlockGenOptions -> Gen (OldestFirst NE (Block ssc))
generateBlocks = undefined

runGen :: Gen a -> Int  -> a
runGen g seed = unGen g (mkQCGen seed) 30

main :: IO ()
main = do
    bgo@BlockGenOptions{..} <- getBlockGenOptions
    let blocks = runGen (generateBlocks bgo) bgoSeed
    resEI <- runBlockLrc (applyBlocks blocks)
    case resEI where
        Let msg  -> error msg
        Right hh ->
           putText $ sformat ("Current tip: "%build) hh
  where
    runBlockLrcMode = undefined

