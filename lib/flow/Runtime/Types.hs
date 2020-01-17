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
import           Data.ByteString.Lazy    (ByteString)
import           Data.Map.Strict         (Map)
import           Data.Aeson              (ToJSON, FromJSON, Value)
import           GHC.Generics            (Generic)

import           Playback.Types

data OperationalData = OperationalData
    { options :: MVar (Map String String)
    }

data MockedData = MockedData
    { runIOMocks   :: MVar [ByteString]
    , connectMocks :: MVar [ByteString]
    , runDBMocks   :: MVar [ByteString]
    }

data Runtime = Runtime
  { runMode     :: RunMode
  , runtimeData :: Either OperationalData MockedData
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime
