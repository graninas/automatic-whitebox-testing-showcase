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

module TestInterpreter where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar
                                         , putMVar, readMVar)
import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
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
import GHC.Any (Any)

data TestRuntime = TestRuntime
  { runIOMocks   :: MVar [Any]
  , connectMocks :: MVar [Any]
  , runDBMocks   :: MVar [Any]
  }


mkMocks :: [a] -> MVar [Any]
mkMocks as = newMVar $ map unsafeCoerce as



getNextRunIOMock :: TestRuntime -> IO a
getNextRunIOMock _ = error ""

getNextConnectMock :: TestRuntime -> IO a
getNextConnectMock _ = error ""

getNextRunDBMock :: TestRuntime -> IO a
getNextRunDBMock _ = error ""


interpretFlowFTest :: TestRuntime -> FlowF a -> IO a
interpretFlowFTest rt (RunIO ioAct next)      = next <$> getNextRunIOMock rt
interpretFlowFTest rt (LogInfo msg next)      = pure $ next ()
interpretFlowFTest rt (Connect name cfg next) = next <$> getNextConnectMock rt
interpretFlowFTest rt (RunDB conn db _ next)  = next <$> getNextRunDBMock rt
interpretFlowFTest _ _ = error ""

runFlow :: TestRuntime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)
