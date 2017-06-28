{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Context for generation

module Context
       ( bracketBlockGenMode
       ) where

import           Universum

import qualified Control.Monad.Reader      as Mtl
import           Data.Default              (def)
import           Mockable                  (Production)
import qualified STMContainers.Map         as SM
import           System.Wlog               (LoggerName)

import           Pos.Context               (GenesisLeaders (..), GenesisUtxo (..),
                                            NodeContext (..), NodeParams (..))
import           Pos.Core                  (Timestamp (..))
import           Pos.Crypto                (keyGen)
import           Pos.DB.DB                 (closeNodeDBs, initNodeDBs, openNodeDBs)
import           Pos.DB.GState             (getTip)
import           Pos.Genesis               (genesisLeaders)
import           Pos.Launcher              (BaseParams (..), InitModeContext (..),
                                            LoggingParams (..), NetworkParams (..),
                                            allocateNodeContext, newInitFuture,
                                            runInitMode, setupLoggers)
import           Pos.Security              (SecurityParams (..), SecurityWorkersClass)
import           Pos.Ssc.Class             (SscConstraint, SscParams)
import           Pos.Ssc.Extra             (mkSscState)
import           Pos.Txp                   (Utxo, mkTxpLocalData)
import           Pos.Update                (UpdateParams (..))
import qualified Pos.Util.Concurrent.RWVar as RWV
import           Pos.Util.JsonLog          (JsonLogConfig (..))
import           Pos.Util.TimeWarp         (currentTime)
import           Pos.WorkMode              (RealMode, RealModeContext (..), unRealMode)

bracketBlockGenMode
    :: forall ssc a .
      (SscConstraint ssc, SecurityWorkersClass ssc)
    => RealMode ssc a
    -> FilePath
    -> Utxo
    -> SscParams ssc
    -> Production a
bracketBlockGenMode action dbPath utxo sscnp =
  bracket (openNodeDBs True dbPath) closeNodeDBs $ \nodeDBs -> do
    (_, fakeSK) <- keyGen
    systemStart <- Timestamp <$> currentTime
    (futureLrcContext, putLrcContext) <- newInitFuture
    (futureSlottingVar, putSlottingVar) <- newInitFuture
    (futureSlottingContext, putSlottingContext) <- newInitFuture
    let loggerName :: LoggerName
        loggerName = "block-gen"
    let bp = BaseParams $ LoggingParams {
          lpHandlerPrefix = Nothing -- TODO change
        , lpConfigPath = Nothing
        , lpRunnerTag = loggerName
        , lpEkgPort = Nothing
        }
    let putSlotting sv sc = do
            putSlottingVar sv
            putSlottingContext sc
    let np = NodeParams
                { npDbPathM        = dbPath
                , npRebuildDb      = True
                , npSystemStart    = systemStart
                , npSecretKey      = fakeSK
                , npUserSecret     = def
                , npBaseParams     = bp
                , npCustomUtxo     = utxo
                , npJLFile         = Nothing
                , npPropagation    = False
                , npReportServers  = []
                , npUpdateParams   = emptyUpdateParams
                , npSecurityParams = emptySecretParams
                , npUseNTP         = False
                , npNetwork        = emptyNetworkParams
                }
    let initModeContext = InitModeContext
            nodeDBs
            (GenesisUtxo utxo)
            (GenesisLeaders (genesisLeaders utxo))
            np
            futureSlottingVar
            futureSlottingContext
            futureLrcContext
    realContext <- runInitMode initModeContext $ do
        initNodeDBs @ssc
        ctx@NodeContext {..} <- allocateNodeContext np sscnp putSlotting
        putLrcContext ncLrcContext
        initTip <- getTip
        setupLoggers $ bpLoggingParams (npBaseParams np)
        dlgVar <- RWV.new def
        txpVar <- mkTxpLocalData mempty initTip
        sscState <- mkSscState @ssc
        peerState <- liftIO SM.newIO
        pure $ RealModeContext
                   nodeDBs
                   sscState
                   txpVar
                   dlgVar
                   peerState
                   JsonLogDisabled
                   loggerName
                   ctx
    Mtl.runReaderT (unRealMode @ssc action) realContext
  where
    emptyUpdateParams = UpdateParams "" False []
    emptySecretParams = SecurityParams [] []
    emptyNetworkParams = NetworkParams (Left mempty) undefined
