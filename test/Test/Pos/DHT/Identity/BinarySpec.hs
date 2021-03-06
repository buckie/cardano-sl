-- | This module tests Binary instances.

module Test.Pos.DHT.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec               (Spec, describe)
import           Universum

import qualified Pos.DHT.Model            as DHT
import           Pos.Infra.Arbitrary      ()

import           Test.Pos.Util            (binaryTest, storeTest)

spec :: Spec
spec = describe "DHT.Model" $ do
    describe "Bi instances" $ do
        binaryTest @DHT.DHTKey
        binaryTest @DHT.DHTData
    describe "Store instances" $ do
        storeTest @DHT.DHTKey
