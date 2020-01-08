{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Playback.Types where

import           Control.Concurrent.MVar (MVar)
import           Control.Exception (Exception)
import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import           Data.Vector
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as MArr
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, Value, Result(..), encode, decode, toJSON, fromJSON, parseJSON)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

type EntryIndex = Int
type EntryName = String
type EntryPayload = Value
data RecordingEntry = RecordingEntry EntryIndex EntryReplayingMode EntryName EntryPayload
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type RecordingEntries = Vector RecordingEntry
newtype Recording = Recording
  { entries :: RecordingEntries
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data GlobalReplayingMode = GlobalNormal | GlobalNoVerify | GlobalNoMocking | GlobalSkip

data EntryReplayingMode = Normal | NoVerify | NoMock
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

class (Eq rrItem, ToJSON rrItem, FromJSON rrItem)
  => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> EntryReplayingMode -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String

class RRItem rrItem => MockedResult rrItem native where
  getMock :: rrItem -> Maybe native


data PlaybackErrorType
  = UnexpectedRecordingEnd
  | UnknownRRItem
  | MockDecodingFailed
  | ItemMismatch
  | UnknownPlaybackError
  | ForkedFlowRecordingsMissed
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PlaybackError = PlaybackError
  { errorType    :: PlaybackErrorType
  , errorMessage :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ReplayingException = ReplayingException PlaybackError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Exception ReplayingException

data RecorderRuntime = RecorderRuntime
  { flowGUID            :: String
  , recordingMVar       :: MVar RecordingEntries
  , forkedRecordingsVar :: MVar ( Map String (MVar RecordingEntries))
  , disableEntries      :: [String]
  }

data PlayerRuntime = PlayerRuntime
  { recording            :: RecordingEntries
  , stepMVar             :: MVar Int
  , errorMVar            :: MVar (Maybe PlaybackError)
  , disableVerify        :: [String]
  , disableMocking       :: [String]
  , skipEntries          :: [String]
  , entriesFiltered      :: Bool
  , flowGUID             :: String
  , forkedFlowRecordings :: Map String RecordingEntries
  , forkedFlowErrorsVar  :: MVar (Map String (Maybe PlaybackError))
  }


encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = decode . BSL.fromStrict . BS.pack

encodeToValue :: ToJSON a => a -> Value
encodeToValue = toJSON

decodeFromValue :: FromJSON a => Value -> Maybe a
decodeFromValue val = case fromJSON val of
  Error _ -> Nothing   -- TODO
  Success a -> Just a



note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b
