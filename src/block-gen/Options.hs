{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Command line options

module Options
       ( BlockGenOptions (..)
       , getBlockGenOptions
       ) where

import           Universum

import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, strOption, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Pos.Constants                (blkSecurityParam, genesisSlotDuration)

data BlockGenOptions = BlockGenOptions
    { bgoK            :: !Int
    -- ^ k constant of algorithm (from paper)
    -- Will be taken from constants if command-line is not specified.
    , bgoSlotDuration :: !Int
    -- ^ Slot duration (in milliseconds)
    -- Will be taken from constants if command-line is not specified.
    , bgoNodes        :: !Int
    -- ^ Number of nodes to generate blocks.
    , bgoLength       :: !Int
    -- ^ Number of blocks to generate.
    , bgoPath         :: !FilePath
    -- ^ Location of generated database.
    , bgoSeed         :: !(Maybe Int)
    -- ^ Seed to generate blocks.
    }

optionsParser :: Parser BlockGenOptions
optionsParser = do
    bgoK <- option auto $
        long    "k" <>
        metavar "INT" <>
        value blkSecurityParam <>
        help "k constant of algorithm"

    bgoSlotDuration <- option auto $
        long    "slot-duration" <>
        metavar "INT" <>
        value (fromIntegral genesisSlotDuration) <>
        help "Slot duration (in ms)"

    bgoLength <- option auto $
        long    "length" <>
        metavar "INT" <>
        help "Length of generated blockchain."

    bgoNodes <- option auto $
        long    "nodes" <>
        metavar "INT" <>
        help "Number of nodes to participate in generation"

    bgoPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        value   "generated-db" <>
        help    "Location of generated database."

    bgoSeed <- optional $ option auto $
        long    "seed" <>
        metavar "INT" <>
        help    "Custom seed to generate blocks."

    return BlockGenOptions{..}

getBlockGenOptions :: IO BlockGenOptions
getBlockGenOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc "It generates database of node, corresponding to some correct blockchain"
                 <> header "Cardano SL blockchain generator"
                 <> footerDoc usageExample

    versionOption = infoOption
        "cardano-block-gen-1.0"
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = undefined
-- usageExample = Just [s|
-- Command example:

--   stack exec -- cardano-block-gen           \
--     -k 2                                    \
--     --slot-duration 15                      \
--     --length 5000                                 \
--     --db-path generated-db
--     --seed 701|]
