{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common methods/datatypes across Delegation.Logic.*

module Pos.Delegation.Logic.Common
       (
       -- * Exceptions
         DelegationError(..)

       -- * Modifying memstate
       , DelegationStateAction
       , runDelegationStateAction
       , invalidateProxyCaches

       -- * Common helpers
       , initDelegation
       , getPSKsFromThisEpoch
       ) where

import           Control.Exception         (Exception (..))
import           Control.Lens              ((%=), (.=))
import qualified Data.HashSet              as HS
import qualified Data.Text.Buildable       as B
import           Data.Time.Clock           (UTCTime, addUTCTime)
import           Formatting                (bprint, stext, (%))
import           Universum

import           Pos.Block.Core            (Block, mainBlockDlgPayload)
import           Pos.Constants             (lightDlgConfirmationTimeout,
                                            messageCacheTimeout)
import           Pos.Core                  (HeaderHash, epochIndexL, headerHash)
import           Pos.Crypto                (ProxySecretKey (..))
import qualified Pos.DB.Block              as DB
import qualified Pos.DB.DB                 as DB
import           Pos.Delegation.Class      (DelegationWrap (..), MonadDelegation,
                                            askDelegationState, dwConfirmationCache,
                                            dwEpochId, dwMessageCache, dwThisEpochPosted)
import           Pos.Delegation.Types      (DlgPayload (getDlgPayload))
import           Pos.Exception             (cardanoExceptionFromException,
                                            cardanoExceptionToException)
import           Pos.Types                 (ProxySKHeavy)
import qualified Pos.Util.Concurrent.RWVar as RWV
import           Pos.Util.LRU              (filterLRU)

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data DelegationError =
    -- | Can't apply blocks to state of transactions processing.
    DelegationCantApplyBlocks Text
    deriving (Typeable, Show)

instance Exception DelegationError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

instance B.Buildable DelegationError where
    build (DelegationCantApplyBlocks msg) =
        bprint ("can't apply in delegation module: "%stext) msg

----------------------------------------------------------------------------
-- Modifying memstate
----------------------------------------------------------------------------

-- | Convenient monad to work in 'DelegationWrap' state context.
type DelegationStateAction m = StateT DelegationWrap m

-- Misha knows better probably.
{-# ANN runDelegationStateAction ("HLint: ignore Use fmap" :: Text) #-}
-- | Effectively takes a lock on ProxyCaches mvar in NodeContext and
-- allows you to run some computation producing updated ProxyCaches
-- and return value. Will put MVar back on exception.
runDelegationStateAction
    :: (MonadIO m, MonadMask m, MonadDelegation m)
    => DelegationStateAction m a -> m a
runDelegationStateAction action = do
    var <- askDelegationState
    RWV.modify var $ \startState -> swap <$> runStateT action startState

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: (Monad m) => UTCTime -> DelegationStateAction m ()
invalidateProxyCaches curTime = do
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime messageCacheTimeout) t > curTime)
    dwConfirmationCache %=
        filterLRU (\t -> addUTCTime (toDiffTime lightDlgConfirmationTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Retrieves psk certificated that have been accumulated before
-- given block. The block itself should be in DB.
getPSKsFromThisEpoch
    :: forall ssc m.
       DB.MonadBlockDB ssc m
    => HeaderHash -> m [ProxySKHeavy]
getPSKsFromThisEpoch tip =
    concatMap (either (const []) (getDlgPayload . view mainBlockDlgPayload)) <$>
        (DB.loadBlocksWhile @ssc) isRight tip

-- | Initializes delegation in-memory storage.
--
-- * Sets `_dwEpochId` to epoch of tip.
-- * Loads `_dwThisEpochPosted` from database
initDelegation
    :: forall ssc m.
       (MonadIO m, DB.MonadBlockDB ssc m, MonadDelegation m, MonadMask m)
    => m ()
initDelegation = do
    tip <- DB.getTipHeader @(Block ssc)
    let tipEpoch = tip ^. epochIndexL
    fromGenesisPsks <-
        map pskIssuerPk <$> (getPSKsFromThisEpoch @ssc) (headerHash tip)
    runDelegationStateAction $ do
        dwEpochId .= tipEpoch
        dwThisEpochPosted .= HS.fromList fromGenesisPsks
