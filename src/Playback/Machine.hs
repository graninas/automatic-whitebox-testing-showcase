{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}

module Playback.Machine where

import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust)
import qualified Data.IntMap as MArr
import           Control.Concurrent.MVar (MVar, takeMVar, putMVar
                                         , isEmptyMVar, swapMVar)
import           Data.UUID.V4       (nextRandom)
import           Data.Aeson         (ToJSON, FromJSON, encode, decode)
import           Data.Proxy         (Proxy(..))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           Playback.Types


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
  flag <- isEmptyMVar (errorMVar playerRt)
  if flag
    then putMVar (errorMVar playerRt) err
    else void $ swapMVar (errorMVar playerRt) err
  error $ show err

pushRecordingEntry
  :: RecorderRuntime
  -> RecordingEntry
  -> IO ()
pushRecordingEntry recorderRt (RecordingEntry _ n p) = do
  entries <- takeMVar $ recordingMVar recorderRt
  let idx = (MArr.size entries)
  let re = RecordingEntry idx n p
  putMVar (recordingMVar recorderRt) $ MArr.insert idx re entries

popNextRecordingEntry :: PlayerRuntime -> IO (Maybe RecordingEntry)
popNextRecordingEntry playerRt = do
  cur <- takeMVar $ stepMVar playerRt
  let mbItem = MArr.lookup cur $ recording playerRt
  when (isJust mbItem) $ putMVar (stepMVar playerRt) (cur + 1)
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
