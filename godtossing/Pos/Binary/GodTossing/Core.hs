-- | Serialization of core types from GodTossing SSC.

module Pos.Binary.GodTossing.Core
       (
       ) where

import qualified Data.HashMap.Strict           as HM
import           Universum

import           Pos.Binary.Class              (Bi (..), PokeWithSize, convertToSizeNPut,
                                                getWord8, label, labelS, putField, putS,
                                                putWord8S)
import           Pos.Binary.Crypto             ()
import           Pos.Core.Address              (addressHash)
import           Pos.Ssc.GodTossing.Core.Types (Commitment (..), Commitment (..),
                                                CommitmentsMap (..), GtPayload (..),
                                                GtProof (..), Opening (..),
                                                VssCertificate (..), mkCommitmentsMap,
                                                recreateVssCertificate)

instance Bi Commitment where
    sizeNPut = labelS "Commitment" $
           putField commShares
        <> putField commProof
    get = label "Commitment" $ do
        commShares <- get
        when (null commShares) $ fail "get@Commitment: no shares"
        commProof <- get
        return Commitment {..}

instance Bi CommitmentsMap where
    sizeNPut = labelS "CommitmentsMap" $ putField (toList . getCommitmentsMap)
    get = label "CommitmentsMap" $ mkCommitmentsMap <$> get

instance Bi VssCertificate where
    sizeNPut = labelS "VssCertificate" $
        putField vcVssKey <>
        putField vcExpiryEpoch <>
        putField vcSignature <>
        putField vcSigningKey
    get = label "VssCertificate" $
        join $ liftM4 recreateVssCertificate get get get get

instance Bi Opening where
    sizeNPut = labelS "Opening" $ putField getOpening
    get = label "Opening" $ Opening <$> get

instance Bi GtPayload where
    sizeNPut = labelS "GtPayload" $ convertToSizeNPut toBi
      where
        toBi :: GtPayload -> PokeWithSize ()
        toBi = \case
            CommitmentsPayload commMap vssMap ->
                putWord8S 0 <> putS commMap <> putS (toList vssMap)
            OpeningsPayload opMap vssMap ->
                putWord8S 1 <> putS opMap <> putS (toList vssMap)
            SharesPayload sharesMap vssMap ->
                putWord8S 2 <> putS sharesMap <> putS (toList vssMap)
            CertificatesPayload vssMap ->
                putWord8S 3 <> putS (toList vssMap)
    get = label "GtPayload" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsPayload get getVssCerts
            1 -> liftM2 OpeningsPayload get getVssCerts
            2 -> liftM2 SharesPayload get getVssCerts
            3 -> CertificatesPayload <$> getVssCerts
            tag -> fail ("get@GtPayload: invalid tag: " ++ show tag)
          where
            getVssCerts = HM.fromList . map toCertPair <$> get
            toCertPair vc = (addressHash $ vcSigningKey vc, vc)

instance Bi GtProof where
    sizeNPut = labelS "GtProof" $ convertToSizeNPut toBi
      where
        toBi :: GtProof -> PokeWithSize ()
        toBi = \case
            CommitmentsProof a b -> putWord8S 0 <> putS a <> putS b
            OpeningsProof a b    -> putWord8S 1 <> putS a <> putS b
            SharesProof a b      -> putWord8S 2 <> putS a <> putS b
            CertificatesProof a  -> putWord8S 3 <> putS a
    get = label "GtProof" $ do
        getWord8 >>= \case
            0 -> liftM2 CommitmentsProof get get
            1 -> liftM2 OpeningsProof get get
            2 -> liftM2 SharesProof get get
            3 -> CertificatesProof <$> get
            tag -> fail ("get@GtProof: invalid tag: " ++ show tag)
