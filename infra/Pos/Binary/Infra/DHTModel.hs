-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Infra.DHTModel () where

import           Universum

import qualified Data.Store                  as Store
import qualified Data.Store.Internal         as Store
import           Network.Kademlia.HashNodeId (HashId (..))

import           Pos.Binary.Class            (Bi (..), Size (..), getSize, label, labelP)
import           Pos.DHT.Model.Types         (DHTData (..), DHTKey (..))

instance Bi DHTKey where
    -- CSL-1122: is this a constant?
    size = VarSize $ \(DHTKey (HashId bs)) -> getSize bs
    put (DHTKey (HashId bs)) = labelP "DHTKey" $ put bs
    get = label "DHTKey" $ DHTKey . HashId <$> get

instance Bi DHTData where
    size = ConstSize 0
    put (DHTData ()) = labelP "DHTData" $ pure ()
    get = label "DHTData" $ pure $ DHTData ()

-- Kademlia uses Store, so we define a Store instance here as well.
instance Store.Store DHTKey where
    size = VarSize $ \(DHTKey (HashId bs)) -> Store.getSize bs
    poke (DHTKey (HashId bs)) = labelP "Store: DHTKey" $ Store.poke bs
    peek = label "Store: DHTKey" $ DHTKey . HashId <$> Store.peek
