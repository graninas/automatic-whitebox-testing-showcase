{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE RecordWildCards           #-}

module Playback.Entries where

import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, Value, decode, encode, toJSON, parseJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
import           Data.Maybe            (isJust)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           GHC.Generics          (Generic)

import           Playback.Types
import           Types
import           Runtime.Options

data SetOptionEntry = SetOptionEntry
  { key   :: Value
  , value :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: OptionEntity k v => k -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry (encodeToValue k) (encodeToValue v)

data GetOptionEntry = GetOptionEntry
  { key   :: Value
  , value :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: OptionEntity k v => k -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry (encodeToValue k) (encodeToValue mv)

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

data ForkFlowEntry = ForkFlowEntry
  { description :: String
  , guid        :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkForkFlowEntry :: String -> String -> () -> ForkFlowEntry
mkForkFlowEntry desc guid _ = ForkFlowEntry desc guid

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: String -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

data RunIOEntry = RunIOEntry
  { jsonResult :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunIOEntry :: ToJSON a => a -> RunIOEntry
mkRunIOEntry = RunIOEntry . encodeToValue

data LogInfoEntry = LogInfoEntry
  { message :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkLogInfoEntry :: String -> () -> LogInfoEntry
mkLogInfoEntry msg _ = LogInfoEntry msg

data ConnectEntry = ConnectEntry
  { ceDBConfig :: DBConfig
  , ceDBName :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkConnectEntry :: String -> DBConfig -> DBConnection -> ConnectEntry
mkConnectEntry dbName dbCfg _ = ConnectEntry dbCfg dbName

data RunDBEntry = RunDBEntry
  { dbeDBName :: String
  , dbeDescription :: String
  , dbeJsonResult :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunDBEntry
  :: ToJSON a
  => DBConnection
  -> String
  -> a
  -> RunDBEntry
mkRunDBEntry (NativeConn dbName _) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToValue dbRes
mkRunDBEntry (MockedConn (MockedConnection dbName)) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToValue dbRes

instance RRItem GetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "GetOptionEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{..} = decodeFromValue value

instance RRItem SetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "SetOptionEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

instance RRItem RunSysCmdEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunSysCmdEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

instance RRItem ForkFlowEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ForkFlowEntry" $ encodeToValue  rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "ForkFlowEntry"

instance MockedResult ForkFlowEntry () where
  getMock _ = Just ()

instance RRItem GenerateGUIDEntry where
  toRecordingEntry rrItem idx mode  = RecordingEntry idx mode "GenerateGUIDEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry String where
  getMock (GenerateGUIDEntry g) = Just g

instance RRItem RunIOEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunIOEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "RunIOEntry"

instance FromJSON a => MockedResult RunIOEntry a where
  getMock (RunIOEntry r) = decodeFromValue r

instance RRItem LogInfoEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "LogInfoEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "LogInfoEntry"

instance MockedResult LogInfoEntry () where
  getMock _ = Just ()

instance RRItem ConnectEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ConnectEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "ConnectEntry"

instance MockedResult ConnectEntry DBConnection where
  getMock (ConnectEntry _ dbName) = Just $ MockedConn $ MockedConnection dbName

instance RRItem RunDBEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunDBEntry" $ encodeToValue rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromValue payload
  getTag _ = "RunDBEntry"

instance FromJSON a => MockedResult RunDBEntry a where
  getMock (RunDBEntry _ _ r) = decodeFromValue r
