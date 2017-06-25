{-# LANGUAGE TemplateHaskell #-}

-- | Command line options

module BlockGenOptions
       ( BlockGenOptions (..)
       , getBlockGenOptions
       ) where

import           Universum

import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, short, strOption, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Pos.Constants                (blkSecurityParam, genesisSlotDuration)
import           Pos.Core                     (Timestamp (..))

data BlockGenOptions = BlockGenOptions
    { bgoK            :: !Int
    -- ^ k constant of algorithm (from paper)
    -- Will be taken from constants if command-line is not specified.
    , bgoSlotDuration :: !Int
    -- ^ Slot duration (in seconds)
    -- Will be taken from constants if command-line is not specified.
    , bgoN            :: !Int
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
        value (getTimestamp genesisSlotDuration) <>
        help "Slot duration (in secs)"

    bgoN <- option auto $
        long    "n" <>
        metavar "INT" <>
        help "Length of blockchain."

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
--     -n 5000                                 \
--     --db-path generated-db
--     --seed 701|]
