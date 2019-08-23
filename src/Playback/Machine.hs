{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Playback.Machine where

import           Control.Monad      (unless, when, void)
import           Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.UUID          (toString)
import           Data.Maybe         (isJust, fromMaybe)
import qualified Data.Vector as V
import           Data.Vector ((!?))
-- import qualified Data.IntMap as MArr
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
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
  writeIORef (errorRef playerRt) $ Just err
  error $ show err

pushRecordingEntry
  :: RecorderRuntime
  -> RecordingEntry
  -> IO ()
pushRecordingEntry recorderRt (RecordingEntry _ mode n p) = do
  entries <- readIORef $ recordingRef recorderRt
  let idx = (V.length entries)
  let re = RecordingEntry idx mode n p
  writeIORef (recordingRef recorderRt) $ V.snoc entries re

popNextRecordingEntry :: PlayerRuntime -> IO (Maybe RecordingEntry)
popNextRecordingEntry PlayerRuntime{..} = do
  cur <- readIORef stepRef
  let mbItem = (!?) recording cur
  when (isJust mbItem) $ writeIORef stepRef (cur + 1)
  pure mbItem

popNextRRItem
  :: forall rrItem native
   . RRItem rrItem
  => PlayerRuntime
  -> IO (Either PlaybackError (RecordingEntry, rrItem))
popNextRRItem playerRt  = do
  mbRecordingEntry <- popNextRecordingEntry playerRt
  let flowStep = getTag $ Proxy @rrItem
  -- let flowStepInfo = getInfo' rrItemDict
  pure $ do
    recordingEntry <- note (unexpectedRecordingEnd "") mbRecordingEntry
    let unknownErr = unknownRRItem $ show recordingEntry
    rrItem <- note (unknownErr "") $ fromRecordingEntry recordingEntry
    pure (recordingEntry, rrItem)

popNextRRItemAndResult
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> IO (Either PlaybackError (RecordingEntry, rrItem, native))
popNextRRItemAndResult playerRt  = do
  let flowStep = getTag $ Proxy @rrItem
  eNextRRItem <- popNextRRItem playerRt
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

getCurrentEntryReplayMode :: PlayerRuntime -> IO EntryReplayingMode
getCurrentEntryReplayMode PlayerRuntime{..} = do
  cur <- readIORef stepRef
  pure $ fromMaybe Normal $ do
    (RecordingEntry _ mode _ _) <- (!?) recording cur
    pure mode

replayWithGlobalConfig 
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> IO native 
  -> (native -> rrItem)
  -> Either PlaybackError (RecordingEntry, rrItem, native)
  -> IO native
replayWithGlobalConfig playerRt  ioAct mkRRItem eNextRRItemRes = do
  let tag = getTag $ Proxy @rrItem
  let config = checkForReplayConfig playerRt tag
  case config of
    GlobalNoVerify -> case eNextRRItemRes of
      Left err -> setReplayingError playerRt err
      Right stepInfo@(_, _, r) -> pure r
    GlobalNormal    -> case eNextRRItemRes of
        Left err -> setReplayingError playerRt err
        Right stepInfo@(_, _, r) -> do
          compareRRItems playerRt stepInfo $ mkRRItem r
          pure r
    GlobalNoMocking -> ioAct
    GlobalSkip -> ioAct

checkForReplayConfig :: PlayerRuntime -> String -> GlobalReplayingMode
checkForReplayConfig  PlayerRuntime{..} tag | tag `elem` disableMocking  = GlobalNoMocking
                                            | tag `elem` disableVerify   = GlobalNoVerify
                                            | otherwise                  = GlobalNormal

replay
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (native -> rrItem)
  -> IO native
  -> IO native
replay playerRt@PlayerRuntime{..} mkRRItem ioAct 
  | getTag (Proxy @rrItem) `elem` skipEntries = ioAct
  | otherwise = do
      entryReplayMode <- getCurrentEntryReplayMode playerRt
      eNextRRItemRes <- popNextRRItemAndResult playerRt
      case entryReplayMode of
        Normal -> replayWithGlobalConfig playerRt ioAct mkRRItem eNextRRItemRes
        NoVerify -> case eNextRRItemRes of
          Left err -> setReplayingError playerRt err
          Right stepInfo@(_, _, r) -> pure r
        NoMock -> ioAct



record
  ::forall rrItem native
   . RRItem rrItem
  => RecorderRuntime
  -> (native -> rrItem)
  -> IO native
  -> IO native
record recorderRt@RecorderRuntime{..} mkRRItem ioAct = do
  native <- ioAct
  let tag = getTag $ Proxy @rrItem
  when (tag `notElem` disableEntries)
    $ pushRecordingEntry recorderRt $ toRecordingEntry (mkRRItem native) 0 Normal
  pure native
