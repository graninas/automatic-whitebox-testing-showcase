{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Playback.Entries where

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
import           Playback.Types
import           Types

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: String -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

data RunIOEntry = RunIOEntry
  { jsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunIOEntry :: ToJSON a => a -> RunIOEntry
mkRunIOEntry = RunIOEntry . encodeToStr

data LogInfoEntry = LogInfoEntry
  { message :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkLogInfoEntry :: String -> () -> LogInfoEntry
mkLogInfoEntry msg _ = LogInfoEntry msg

data ConnectEntry = ConnectEntry String DB.Config MockedConn
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkConnectEntry :: String -> DB.Config -> Connection -> ConnectEntry
mkConnectEntry dbName dbCfg (NativeConn _ _)  = ConnectEntry dbName dbCfg (MC dbName)
mkConnectEntry dbName dbCfg (MockedConn mc) = ConnectEntry dbName dbCfg mc

data RunDBEntry = RunDBEntry
  { dbMockedConn :: MockedConn
  , dbJsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunDBEntry :: ToJSON a => Connection -> a -> RunDBEntry
mkRunDBEntry (NativeConn dbName _) dbRes = RunDBEntry (MC dbName) $ encodeToStr dbRes
mkRunDBEntry (MockedConn mc) dbRes = RunDBEntry mc $ encodeToStr dbRes


instance RRItem GenerateGUIDEntry where
  toRecordingEntry rrItem idx = RecordingEntry idx "GenerateGUIDEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) = decodeFromStr payload
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry String where
  getMock (GenerateGUIDEntry g) = Just g

instance RRItem RunIOEntry where
  toRecordingEntry rrItem idx = RecordingEntry idx "RunIOEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) = decodeFromStr payload
  getTag _ = "RunIOEntry"

instance FromJSON a => MockedResult RunIOEntry a where
  getMock (RunIOEntry r) = decodeFromStr r

instance RRItem LogInfoEntry where
  toRecordingEntry rrItem idx = RecordingEntry idx "LogInfoEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) = decodeFromStr payload
  getTag _ = "LogInfoEntry"

instance MockedResult LogInfoEntry () where
  getMock _ = Just ()



instance RRItem ConnectEntry where
  toRecordingEntry rrItem idx = RecordingEntry idx "ConnectEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) = decodeFromStr payload
  getTag _ = "ConnectEntry"

instance MockedResult ConnectEntry Connection where
  getMock (ConnectEntry _ _ mc) = Just $ MockedConn mc



instance RRItem RunDBEntry where
  toRecordingEntry rrItem idx = RecordingEntry idx "RunDBEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ payload) = decodeFromStr payload
  getTag _ = "RunDBEntry"

instance FromJSON a => MockedResult RunDBEntry a where
  getMock (RunDBEntry _ r) = decodeFromStr r
