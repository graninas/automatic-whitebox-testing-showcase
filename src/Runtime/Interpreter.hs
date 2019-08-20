{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Runtime.Interpreter where

import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe            (isJust)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           GHC.Generics          (Generic)

import qualified DB.Native             as DB
import           Language
import           Playback.Entries
import           Playback.Machine
import           Playback.Types
import           Runtime.Types
import           Types

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
  = record recorderRt Proxy mkRRItem act

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

interpretFlowF rt (GenerateGUID next) =
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

interpretFlowF rt (RunDB conn db next) = do
  res <- withRunMode (runMode rt)
    (mkRunDBEntry conn)
    (case conn of
        NativeConn _ nativeConn -> runDatabase nativeConn db
        MockedConn _            -> error "Should not be evaulated.")
  pure $ next res



runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)
