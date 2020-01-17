{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE BangPatterns              #-}

module Runtime.Types where

import           Control.Concurrent.MVar (MVar)
import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust)
import           Data.Map.Strict    (Map)
import qualified Data.IntMap as MArr
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, Value, encode, decode)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           GHC.Exts           (Any)

import           Playback.Types

data OperationalData = OperationalData
    { options :: MVar (Map String String)
    }

data MockedData = MockedData
    { runIOMocks   :: MVar [BSL.ByteString]
    , connectMocks :: MVar [BSL.ByteString]
    , runDBMocks   :: MVar [BSL.ByteString]
    }

data Runtime = Runtime
  { runMode     :: RunMode
  , runtimeData :: Either OperationalData MockedData
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime
