{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions related to delegation.

module Pos.Delegation.Helpers
       ( dlgVerifyPayload
       , isRevokePsk
       , dlgMemPoolApplyBlock
       , detectCycleOnAddition
       , dlgReachesIssuance
       ) where

import           Universum

import           Control.Lens              (uses, (%=))
import           Control.Monad.Except      (MonadError (throwError))
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Data.List                 (partition)

import           Pos.Block.Core.Main.Lens  (mainBlockDlgPayload)
import           Pos.Block.Core.Main.Types (MainBlock)
import           Pos.Core                  (EpochIndex, ProxySKHeavy)
import           Pos.Crypto                (ProxySecretKey (..), PublicKey)
import           Pos.Delegation.Types      (DlgMemPool, DlgPayload (getDlgPayload))

-- | Verify delegation payload without using GState. This function can
-- be used for block verification in isolation, also it can be used
-- for mempool verification.
dlgVerifyPayload :: MonadError Text m => EpochIndex -> DlgPayload -> m ()
dlgVerifyPayload epoch (getDlgPayload -> proxySKs) =
    unless (null notMatchingEpochs) $
    throwError "Block contains psk(s) that have non-matching epoch index"
  where
    notMatchingEpochs = filter ((/= epoch) . pskOmega) proxySKs

-- | Checks if given PSK revokes delegation (issuer = delegate).
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk ProxySecretKey{..} = pskIssuerPk == pskDelegatePk

-- | Applies block certificates to 'ProxySKHeavyMap'.
dlgMemPoolApplyBlock :: MainBlock ssc -> DlgMemPool -> DlgMemPool
dlgMemPoolApplyBlock block m = flip execState m $ do
    let (toDelete,toReplace) =
            partition isRevokePsk (getDlgPayload $ block ^. mainBlockDlgPayload)
    for_ toDelete $ \psk -> identity %= HM.delete (pskIssuerPk psk)
    for_ toReplace $ \psk -> identity %= HM.insert (pskIssuerPk psk) psk

-- | Checks if addition of the PSK to the map will lead to cycles. The
-- initial map may or may not contain this PSK. Returns nothing if
-- it's good, first-already-visited public key otherwise.
detectCycleOnAddition
    :: forall m . (Monad m)
    => (PublicKey -> m (Maybe ProxySKHeavy)) -- ^ Resolving function, should not
                                             -- return revocation psks.
    -> ProxySKHeavy                          -- ^ PSK to check against
    -> m (Maybe PublicKey)
detectCycleOnAddition resolve toAdd
    -- deleting (revoking) can't add a cycle
    | isRevokePsk toAdd = pure Nothing
    | otherwise = evalStateT (trav (pskDelegatePk toAdd))
                             (HS.singleton $ pskIssuerPk toAdd)
  where
    trav :: PublicKey -> StateT (HashSet PublicKey) m (Maybe PublicKey)
    trav cur = ifM (uses identity $ HS.member cur) (pure $ Just cur) $ do
        next <- lift $ resolve cur
        identity %= HS.insert cur
        let stop = pure Nothing
        let panicRevoke p = error $ "dlgMemPoolDetectCycle: found revoke psk: " <> pretty p
        maybe stop (\psk -> bool (trav $ pskDelegatePk psk)
                                 (panicRevoke psk)
                                 (isRevokePsk psk))
                   next

-- | Given a psk resolver, issuer, delegate and cert he uses (to sign,
-- or taken from psk), checks if there's a psk chain "issuer →
-- delegate" and the last cert matches the provided one (can be
-- retrieved using 'psigPsk'). This *does not* check that delegate
-- didn't issue a psk to somebody else.
dlgReachesIssuance
    :: (Monad m)
    => (PublicKey -> m (Maybe ProxySKHeavy)) -- ^ Resolving function (HM.lookup in pure case).
                                             -- Should never return revocation certs.
    -> PublicKey                             -- ^ Issuer
    -> PublicKey                             -- ^ Delegate
    -> ProxySKHeavy                          -- ^ i->d psk
    -> m Bool
dlgReachesIssuance _ i d _ | i == d = pure True
dlgReachesIssuance resolve i d psk = reach i
  where
    -- Delegate 'd' has right to issue block instead of issuer 'i' if
    -- there's a delegation chain:
    --
    -- i → x₁ → x₂ → … xₖ → d
    --
    -- where every arrow is resolved psk, and the last one xₖ → d
    -- equals to the passed one.
    reach curUser = resolve curUser >>= \case
        Nothing -> pure False
        Just psk'@ProxySecretKey{..}
            | pskDelegatePk == d -> pure $ psk' == psk
            | otherwise          -> reach pskDelegatePk
