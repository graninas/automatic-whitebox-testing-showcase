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
import           Runtime.Options

data SetOptionEntry = SetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: OptionEntity k v => k -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry (encodeToStr k) (encodeToStr v)

data GetOptionEntry = GetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: OptionEntity k v => k -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry (encodeToStr k) (encodeToStr mv)

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

data ForkFlowEntry = ForkFlowEntry
  { description :: String
  , guid        :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkForkFlowEntry :: String -> String -> () -> ForkFlowEntry
mkForkFlowEntry desc guid _ = ForkFlowEntry desc guid

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

data ConnectEntry = ConnectEntry
  { ceDBConfig :: DB.Config
  , ceDBName :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkConnectEntry :: String -> DB.Config -> Connection -> ConnectEntry
mkConnectEntry dbName dbCfg _ = ConnectEntry dbCfg dbName

data RunDBEntry = RunDBEntry
  { dbeDBName :: String
  , dbeDescription :: String
  , dbeJsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunDBEntry
  :: ToJSON a
  => Connection
  -> String
  -> a
  -> RunDBEntry
mkRunDBEntry (NativeConn dbName _) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToStr dbRes
mkRunDBEntry (MockedConn dbName) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToStr dbRes

instance RRItem GetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "GetOptionEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{..} = decodeFromStr value

instance RRItem SetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "SetOptionEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

instance RRItem RunSysCmdEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunSysCmdEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

instance RRItem ForkFlowEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ForkFlowEntry" $ encodeToStr  rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "ForkFlowEntry"

instance MockedResult ForkFlowEntry () where
  getMock _ = Just ()

instance RRItem GenerateGUIDEntry where
  toRecordingEntry rrItem idx mode  = RecordingEntry idx mode "GenerateGUIDEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry String where
  getMock (GenerateGUIDEntry g) = Just g

instance RRItem RunIOEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunIOEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "RunIOEntry"

instance FromJSON a => MockedResult RunIOEntry a where
  getMock (RunIOEntry r) = decodeFromStr r

instance RRItem LogInfoEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "LogInfoEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "LogInfoEntry"

instance MockedResult LogInfoEntry () where
  getMock _ = Just ()



instance RRItem ConnectEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ConnectEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "ConnectEntry"

instance MockedResult ConnectEntry Connection where
  getMock (ConnectEntry _ dbName) = Just $ MockedConn dbName

instance RRItem RunDBEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunDBEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "RunDBEntry"

instance FromJSON a => MockedResult RunDBEntry a where
  getMock (RunDBEntry _ _ r) = decodeFromStr r
