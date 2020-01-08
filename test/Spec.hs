
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}

import Control.Concurrent.MVar
import Control.Monad (when, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Aeson (decode, encode)
import Test.Hspec

import Playback.Types
import Runtime.Types
import Language
import qualified Language as L
import Runtime.Interpreter

initRegularRT = do
  opts <- newMVar Map.empty
  pure $ Runtime
   { runMode = RegularMode
   , options = opts
   }

initRecorderRT = do
  recMVar <- newMVar V.empty
  forkedRecMvar <- newMVar Map.empty
  opts <- newMVar Map.empty
  let recRt = RecorderRuntime
        { flowGUID = "testFlow"
        , recordingMVar = recMVar
        , forkedRecordingsVar = forkedRecMvar
        , disableEntries = []
        }
  pure $ Runtime
    { runMode = RecordingMode recRt
    , options = opts
    }

initPlayerRT recEntries = do
  opts <- newMVar Map.empty
  step <- newMVar 0
  errMVar <- newMVar Nothing
  ffEV <- newMVar Map.empty
  let pRt = PlayerRuntime
        { recording = recEntries
        , stepMVar = step
        , errorMVar = errMVar
        , disableVerify = []
        , disableMocking = []
        , skipEntries = []
        , entriesFiltered = False
        , flowGUID = "MainFlow"
        , forkedFlowRecordings = Map.empty
        , forkedFlowErrorsVar = ffEV
        }
  pure $ Runtime
    { runMode = ReplayingMode pRt
    , options = opts
    }

cmdScript = do
  guid <- generateGUID
  logInfo $ "Generated guid is: " ++ guid
  forkFlow "forked test flow" cmdScript2
  runSysCmd "echo hello"

cmdScript2 = do
  guid <- generateGUID
  logInfo $ "Generated guid from 2-nd script is: " ++ guid
  runSysCmd "echo hello from 2-nd script"


compareGUIDs :: String -> Flow ()
compareGUIDs fileName = do
  newGuid <- generateGUID
  oldGuid <- L.runIO $ readFile fileName

  let equal = newGuid == oldGuid
  when equal $ logInfo "GUIDs are equal."
  unless equal $ logInfo "GUIDs are not equal."

main :: IO ()
main = hspec $ do
  describe "Recordings tests" $ do
    it "Compare guids" $ do
      rt <- initRecorderRT
      runFlow rt $ compareGUIDs "test/guid.txt"
      case runMode rt of
        RecordingMode rrt -> do
          entries <- readMVar $ recordingMVar rrt
          length entries `shouldBe` 3

          pRt <- initPlayerRT entries
          runFlow pRt $ compareGUIDs "test/guid.txt"
          case runMode pRt of
            ReplayingMode prtm -> do
              errors <- readMVar $ errorMVar prtm
              errors `shouldBe` Nothing
              let jsonRec = encode $ Recording recs
              jsonRec `shouldBe` "{\"entries\":[[0,\"Normal\",\"GenerateGUIDEntry\",\"{\\\"guid\\\":\\\"a8c2d0bf-0e06-47e9-ae61-f8e2800ed6db\\\"}\"],[1,\"Normal\",\"RunIOEntry\",\"{\\\"jsonResult\\\":\\\"\\\\\\\"58ee4992-31f6-11ea-978f-2e728ce88125\\\\\\\\n\\\\\\\"\\\"}\"],[2,\"Normal\",\"LogInfoEntry\",\"{\\\"message\\\":\\\"GUIDs are not equal.\\\"}\"]]}"
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"


  describe "Tests" $ do
    it "Regular mode" $ do
      rt <- initRegularRT
      res <- runFlow rt cmdScript
      res `shouldBe` "hello\n"

    it "Recorder mode" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case runMode rt of
        RecordingMode rrt -> do
          recs <- readMVar $ recordingMVar rrt
          V.length recs `shouldBe` 6
          res `shouldBe` "hello\n"
        _ -> fail "wrong mode"

    it "Player mode" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case runMode rt of
        RecordingMode rrt -> do
          entries <- readMVar $ recordingMVar rrt
          pRt <- initPlayerRT entries
          res2 <- runFlow pRt cmdScript
          res `shouldBe` res2
          case runMode pRt of
            ReplayingMode prtm -> do
              errors <- readMVar $ errorMVar prtm
              errors `shouldBe` Nothing
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"
