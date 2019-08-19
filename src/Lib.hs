{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
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

type EntryName = String
type EntryPayload = String
type EntryIndex = Int
data RecordingEntry = RecordingEntry EntryIndex EntryName EntryPayload
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RecordingEntries = MArr.IntMap RecordingEntry
newtype Recording = Recording RecordingEntries

class (Eq rrItem, ToJSON rrItem, FromJSON rrItem)
  => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String

class RRItem rrItem => MockedResult rrItem native where
  getMock :: rrItem -> Maybe native

encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = decode . BSL.fromStrict . BS.pack

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b

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


data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next

instance Functor FlowF where
  fmap f (GenerateGUID next) = GenerateGUID (f . next)
  fmap f (RunIO ioAct next)  = RunIO ioAct (f . next)
  fmap f (LogInfo msg next)  = LogInfo msg (f . next)

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

logInfo :: String -> Flow ()
logInfo msg = liftF $ LogInfo msg id


compareGUIDs :: String -> Flow ()
compareGUIDs fileName = do
  newGuid <- generateGUID
  oldGuid <- runIO $ readFile fileName

  let equal = newGuid == oldGuid
  when equal $ logInfo "GUIDs are equal."
  unless equal $ logInfo "GUIDs are not equal."

data PlaybackErrorType
  = UnexpectedRecordingEnd
  | UnknownRRItem
  | MockDecodingFailed
  | ItemMismatch
  | UnknownPlaybackError
  | ForkedFlowRecordingsMissed
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PlaybackError = PlaybackError
  { errorType :: PlaybackErrorType
  , errorMessage :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- TODO: MVar
data RecorderRuntime = RecorderRuntime
  { recordingRef :: IORef RecordingEntries
  }

data PlayerRuntime = PlayerRuntime
  { recording :: RecordingEntries
  , stepRef :: IORef Int
  , errorRef :: IORef (Maybe PlaybackError)
  }

data Runtime = Runtime
  { runMode :: RunMode
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime

showInfo :: String -> String -> String
showInfo flowStep recordingEntry =
  "\n    Flow step: " ++ flowStep
  ++ "\n    Recording entry: " ++ recordingEntry

unexpectedRecordingEnd :: String -> PlaybackError
unexpectedRecordingEnd flowStep
  = PlaybackError UnexpectedRecordingEnd
  $ "\n    Flow step: " ++ flowStep

unknownRRItem :: String -> String -> PlaybackError
unknownRRItem flowStep recordingEntry
  = PlaybackError UnknownRRItem
  $ showInfo flowStep recordingEntry

mockDecodingFailed :: String -> String -> PlaybackError
mockDecodingFailed flowStep recordingEntry
  = PlaybackError MockDecodingFailed
  $ showInfo flowStep recordingEntry

itemMismatch :: String -> String -> PlaybackError
itemMismatch flowStep recordingEntry
  = PlaybackError ItemMismatch
  $ showInfo flowStep recordingEntry

setReplayingError :: PlayerRuntime -> PlaybackError -> IO a
setReplayingError playerRt err = do
  writeIORef (errorRef playerRt) $ Just err
  error $ show err

pushRecordingEntry
  :: RecorderRuntime
  -> RecordingEntry
  -> IO ()
pushRecordingEntry recorderRt (RecordingEntry _ n p) = do
  entries <- readIORef $ recordingRef recorderRt
  let idx = (MArr.size entries)
  let re = RecordingEntry idx n p
  writeIORef (recordingRef recorderRt) $ MArr.insert idx re entries

popNextRecordingEntry :: PlayerRuntime -> IO (Maybe RecordingEntry)
popNextRecordingEntry playerRt = do
  cur <- readIORef $ stepRef playerRt
  let mbItem = MArr.lookup cur $ recording playerRt
  when (isJust mbItem) $ writeIORef (stepRef playerRt) (cur + 1)
  pure mbItem

popNextRRItem
  :: RRItem rrItem
  => PlayerRuntime
  -> Proxy rrItem
  -> IO (Either PlaybackError (RecordingEntry, rrItem))
popNextRRItem playerRt rrItemP = do
  mbRecordingEntry <- popNextRecordingEntry playerRt
  let flowStep = getTag rrItemP
  -- let flowStepInfo = getInfo' rrItemDict
  pure $ do
    recordingEntry <- note (unexpectedRecordingEnd "") mbRecordingEntry
    let unknownErr = unknownRRItem $ show recordingEntry
    rrItem <- note (unknownErr "") $ fromRecordingEntry recordingEntry
    pure (recordingEntry, rrItem)

popNextRRItemAndResult
  :: RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> Proxy rrItem
  -> IO (Either PlaybackError (RecordingEntry, rrItem, native))
popNextRRItemAndResult playerRt rrItemP = do
  let flowStep = getTag rrItemP
  eNextRRItem <- popNextRRItem playerRt rrItemP
  pure $ do
    (recordingEntry, rrItem) <- eNextRRItem
    let mbNative = getMock rrItem
    nextResult <- note (mockDecodingFailed flowStep (show recordingEntry)) mbNative
    pure (recordingEntry, rrItem, nextResult)

compareRRItems
  :: RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (RecordingEntry, rrItem, native)
  -> rrItem
  -> IO ()
compareRRItems playerRt (recordingEntry, rrItem, mockedResult) flowRRItem = do
  when (rrItem /= flowRRItem) $ do
    -- let flowStep = encodeJSON'  flowRRItem
    let flowStep = encodeToStr flowRRItem
    setReplayingError playerRt $ itemMismatch flowStep (show recordingEntry)

replay
  :: RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (native -> rrItem)
  -> IO native
  -> IO native
replay playerRt mkRRItem ioAct = do
  eNextRRItemRes <- popNextRRItemAndResult playerRt Proxy
  case eNextRRItemRes of
    Left err -> setReplayingError playerRt err
    Right stepInfo@(_, _, r) -> do
      compareRRItems playerRt stepInfo $ mkRRItem r
      pure r

record
  :: RRItem rrItem
  => RecorderRuntime
  -> Proxy rrItem
  -> (native -> rrItem)
  -> IO native
  -> IO native
record recorderRt rrItemP mkRRItem ioAct = do
  native <- ioAct
  let tag = getTag rrItemP
  pushRecordingEntry recorderRt $ toRecordingEntry (mkRRItem native) 0
  pure native

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




-- interpretFlowFTest :: FlowF a -> IO a
-- interpretFlowFTest (GenerateGUID next) = pure $ next "111"
-- interpretFlowFTest (RunIO ioAct next)  = error "IO not supported in tests."
-- interpretFlowFTest (LogInfo msg next)  = pure $ next ()


interpretFlowF :: Runtime -> FlowF a -> IO a
interpretFlowF rt (GenerateGUID next) =
  next <$> withRunMode (runMode rt) mkGenerateGUIDEntry
    (toString <$> nextRandom)
interpretFlowF rt (RunIO ioAct next) =
  next <$> withRunMode (runMode rt) mkRunIOEntry ioAct
interpretFlowF rt (LogInfo msg next) = do
  next <$> withRunMode (runMode rt)
    (mkLogInfoEntry msg)
    (putStrLn msg)

runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)


-- initDB :: String -> DB.DBConfig -> Maybe DB.Connection
-- initDB dbName cfg = do
--   mbConn <- runIO $ DB.initDatabase dbName cfg
--   when (isJust mbConn) $ logInfo $ "Successfully initialized."
--   pure mbConn

-- scenario :: Flow Int
-- scenario = do
--   students <- runDBQuery "SELECT * FROM students"
--   when (null students) $ logInfo "No records found."
--   pure $ length students

someFunc :: IO ()
someFunc = putStrLn "someFunc"
