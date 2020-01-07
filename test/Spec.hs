
{-# LANGUAGE DuplicateRecordFields     #-}

import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Hspec

import Playback.Types
import Runtime.Types
import Language
import Runtime.Interpreter

initRegularRT = do
  opts <- newMVar Map.empty
  pure $ Runtime
   { runMode = RegularMode
   , options = opts}

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
    , options = opts}

cmdScript = do
  guid <- generateGUID
  logInfo $ "Generated guid is: " ++ guid
  forkFlow "forked test flow" cmdScript2
  runSysCmd "echo hello"

cmdScript2 = do
  guid <- generateGUID
  logInfo $ "Generated guid from 2-nd script is: " ++ guid
  runSysCmd "echo hello from 2-nd script"


main :: IO ()
main = hspec $ do
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
