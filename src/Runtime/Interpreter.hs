{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Runtime.Interpreter where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import           Data.Maybe            (isJust)
import           Data.Map.Strict       as Map
import           Data.Monoid           ((<>))
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import qualified Data.Vector            as V
import           GHC.Generics          (Generic)

import qualified DB.Native             as DB
import           Language
import           Playback.Entries
import           Playback.Machine
import           Playback.Types
import           Runtime.SystemCommands
import           Runtime.Types
import           Types

forkF :: Runtime -> Flow a -> IO () 
forkF rt flow = void $ forkIO $ void $ runFlow rt flow

forkPlayerRt :: String -> PlayerRuntime -> IO (Maybe PlayerRuntime)
forkPlayerRt newFlowGUID PlayerRuntime{..} =
  case Map.lookup newFlowGUID forkedFlowRecordings of
    Nothing -> do
      let missedRecsErr = PlaybackError
            { errorType    = ForkedFlowRecordingsMissed
            , errorMessage = "No recordings found for forked flow: " <> newFlowGUID
            }
      forkedFlowErrors <- takeMVar forkedFlowErrorsVar
      let forkedFlowErrors' = Map.insert newFlowGUID missedRecsErr forkedFlowErrors
      putMVar forkedFlowErrorsVar forkedFlowErrors'
      pure Nothing
    Just recording' -> do
      stepVar'  <- newIORef 0
      errorVar' <- newIORef Nothing
      pure $ Just $ PlayerRuntime
        { flowGUID = newFlowGUID
        , stepRef = stepVar'
        , errorRef = errorVar'
        , recording = recording'
        , ..
        }

forkRecorderRt :: String -> RecorderRuntime -> IO RecorderRuntime
forkRecorderRt newFlowGUID RecorderRuntime{..} = do
  recordingVar <- newMVar V.empty
  forkedRecs   <- takeMVar forkedRecordingsVar
  let forkedRecs' = Map.insert newFlowGUID recordingVar forkedRecs
  putMVar forkedRecordingsVar forkedRecs'
  pure RecorderRuntime
    { flowGUID = newFlowGUID
    , ..
    }

forkBackendRuntime flowGUID Runtime{..} = do
  mbForkedMode <- case runMode of
    RegularMode              -> pure $ Just RegularMode
    RecordingMode recorderRt -> Just . RecordingMode <$> forkRecorderRt flowGUID recorderRt
    ReplayingMode playerRt   -> do
      mbRt <- forkPlayerRt flowGUID playerRt
      pure $ ReplayingMode <$> mbRt

  case mbForkedMode of
    Nothing         -> pure Nothing
    Just forkedMode -> pure $ Just $ Runtime
          { runMode = forkedMode
          , ..
          }

-- Entry point into the recording replaying mechanism
withRunMode
  :: RRItem rrItem
  => MockedResult rrItem native
  => RunMode
  -> (native -> rrItem)
  -> IO native
  -> IO native

withRunMode RegularMode _ act = act

withRunMode (RecordingMode recorderRt) mkRRItem act
  = record recorderRt mkRRItem act

withRunMode (ReplayingMode playerRt) mkRRItem act
  = replay playerRt mkRRItem act

--------------------------------------------------------------------------------
-- DB interpreter
interpretDatabaseF :: DB.Connection -> DatabaseF a -> IO a

interpretDatabaseF nativeConn (Query q next) =
  next <$> DB.query nativeConn q

runDatabase :: DB.Connection -> Database a -> IO a
runDatabase nativeConn = foldFree (interpretDatabaseF nativeConn)

--------------------------------------------------------------------------------
-- Flow interpreter
interpretFlowF :: Runtime -> FlowF a -> IO a

interpretFlowF Runtime{..} (GetOption k next) = 
  next <$> withRunMode runMode (mkGetOptionEntry k) maybeValue
  where 
    maybeValue = do
          m <- readMVar options
          pure $ decodeFromStr =<< Map.lookup (encodeToStr k) m

interpretFlowF Runtime{..} (SetOption k v next) =
  next <$> withRunMode runMode (mkSetOptionEntry k v) set
  where
    set = do
      m <- takeMVar options
      let newMap = Map.insert (encodeToStr k) (encodeToStr v) m
      putMVar options newMap

interpretFlowF rt (RunSysCmd cmd next) = do
  next <$> withRunMode (runMode rt) (mkRunSysCmdEntry cmd) (runCmd cmd)

interpretFlowF rt (Fork desc flowGUID flow next) = do
  mbForkedRt <- forkBackendRuntime flowGUID rt
  void $ withRunMode (runMode rt) (mkForkFlowEntry desc flowGUID) 
    (case mbForkedRt of
      Nothing -> putStrLn (flowGUID <> " Failed to fork flow.") *> pure ()
      Just forkedBrt -> forkF forkedBrt flow *> pure ())
  pure $ next ()

interpretFlowF rt (GenerateGUID next) = do
  next <$> withRunMode (runMode rt) mkGenerateGUIDEntry
    (toString <$> nextRandom)

interpretFlowF rt (RunIO ioAct next) =
  next <$> withRunMode (runMode rt) mkRunIOEntry ioAct

interpretFlowF rt (LogInfo msg next) =
  next <$> withRunMode (runMode rt)
    (mkLogInfoEntry msg)
    (putStrLn msg)

interpretFlowF rt (Connect dbName dbConfig next) = do
  conn <- withRunMode (runMode rt)
    (mkConnectEntry dbName dbConfig)
    (NativeConn dbName <$> DB.connect dbName dbConfig)
  pure $ next conn

interpretFlowF rt (RunDB conn qInfo db next) = do
  res <- withRunMode (runMode rt)
    (mkRunDBEntry conn qInfo)
    (case conn of
        NativeConn _ nativeConn -> runDatabase nativeConn db
        MockedConn _            -> error "Should not be evaluated.")
  pure $ next res



runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)
