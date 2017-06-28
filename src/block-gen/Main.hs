{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

-- | Tool to generate database of node.

module Main where

import           Universum

import           Formatting         (build, sformat, stext, (%))
import           Mockable           (runProduction)

import           Pos.Block.Core     (Block, mkGenesisHeader)
import           Pos.Block.Logic    (BlockLrcMode, verifyAndApplyBlocksInternal)
import           Pos.Core           (HeaderHash, StakeholderId)
import           Pos.Core.Genesis   (genesisDevSecretKeys)
import           Pos.Crypto         (SecretKey, keyGen)
import           Pos.Ssc.GodTossing (GtParams (..), SscGodTossing)
import           Pos.Util.Chrono    (NE, NewestFirst (..), OldestFirst (..),
                                     toNewestFirst, toOldestFirst)

import           Context            (bracketBlockGenMode)
import           Options            (BlockGenOptions (..), getBlockGenOptions)

type Ssc = SscGodTossing

mkEmptyMainBlock :: BlockHeader Ssc -> SlotId -> SecretKey -> MainBlock Ssc
mkEmtyMainBlock prev slot sk =
    mkMainBlock (Just prev) SlotId{..} sk Nothing emptyBody defEHData emptyEBData
  where
    emptyBody :: MainBody
    emptyBody = case siSlot of
        0                    ->
          | siEpoch `mod` vssMaxTTL == 0
          | otherwise           ->
        4 * blkSecurityParam ->
        8 * blkSecurityParam ->
        otherwise            ->
            MainBody (fromJust $ mkTxPayload []) (CertificatesPayload mempty) def def

    defEHData :: MainExtraHeaderData
    defEHData = MainExtraHeaderData
                    (BlockVersion 0 0 0)
                    (SoftwareVersion (ApplicationVersion "cardano-sl") 0)
                    (mkAttributes ())

    emptyEBData :: MainExtraBodyData
    emptyEBData = MainExtraBodyData (mkAttributes ())

generateBlocks
    :: forall m . BlockLrcMode Ssc m
    => BlockGenOptions
    -> HashMap StakeholderId SecretKey
    -> m (Either Text HeaderHash)
generateBlocks bgo@BlockGenOptions{..} sks = do
    -- Disable LRC, put directly to db
    -- Enable rollback on fail
    verifyAndApplyBlocksInternal False True undefined

main :: IO ()
main = do
    bgo@BlockGenOptions{..} <- getBlockGenOptions
    fakeVssKeyPair <- vssKeyGen
    let keys = take bgoLength $ genesisDevSecretKeys
    stakeholders <- map (hash . hash . fst) keys
    let initUtxo = undefined
    resEI <- runProduction $
             bracketBlockGenMode @Ssc
                 (generateBlocks bgo blockGen sks)
                 bgoPath
                 initUtxo
                 (GtParams False fakeVssKeyPair)
    case resEI of
        Left msg -> error msg
        Right hh ->
           putText $ sformat ("Current tip: "%build) hh
