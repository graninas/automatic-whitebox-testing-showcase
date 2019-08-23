{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}

module Playback.Types where

import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import           Data.Vector
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust)
import qualified Data.IntMap as MArr
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, encode, decode)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

type EntryIndex = Int
type EntryName = String
type EntryPayload = String
data RecordingEntry = RecordingEntry EntryIndex EntryReplayingMode EntryName EntryPayload
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RecordingEntries = Vector RecordingEntry
newtype Recording = Recording RecordingEntries

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

-- TODO: MVar
data RecorderRuntime = RecorderRuntime
  { recordingRef   :: IORef RecordingEntries
  , disableEntries :: [String]
  }

data PlayerRuntime = PlayerRuntime
  { recording       :: RecordingEntries
  , stepRef         :: IORef Int
  , errorRef        :: IORef (Maybe PlaybackError)
  , disableVerify   :: [String]
  , disableMocking  :: [String]
  , skipEntries     :: [String]
  , entriesFiltered :: Bool
  }


encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = decode . BSL.fromStrict . BS.pack

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b
