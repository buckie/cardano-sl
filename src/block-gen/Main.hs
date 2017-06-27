{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Tool to generate database of node.

module Main where

import           Universum

import           Formatting             (build, sformat, stext, (%))
import           Test.QuickCheck.Random (mkQCGen)

import           Pos.Block.Core         (Block)
import           Pos.Block.Logic        (BlockLrcMode (..), verifyAndApplyBlocksInternal)
import           Pos.Core               (HeaderHash)
import           Pos.Util.Chrono        (NE, NewestFirst (..), OldestFirst (..),
                                         toNewestFirst, toOldestFirst)

import           Context                (bracketBlockGenMode)
import           Options                (BlockGenOptions (..), getBlockGenOptions)

applyBlocks
    :: forall ssc m . BlockLrcMode ssc m
    => IO (Block ssc)
    -> m (Either Text HeaderHash)
applyBlocks blockGen = do
    -- Not run LRC, put directly to db
    verifyAndApplyBlocksInternal False True

main :: IO ()
main = do
    bgo@BlockGenOptions{..} <- getBlockGenOptions
    let blocks = runGen (generateBlocks bgo) bgoSeed
    resEI <- runBlockLrcMode (applyBlocks blocks) bgoPath
    case resEI of
        Left msg  -> error msg
        Right hh ->
           putText $ sformat ("Current tip: "%build) hh
  where

