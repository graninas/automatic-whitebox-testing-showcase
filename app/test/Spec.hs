
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}

import Control.Concurrent.MVar
import Control.Monad (when, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, encode)
import Test.Hspec
import Data.UUID             (toString)
import Data.UUID.V4          (nextRandom)

import Playback.Types
import Runtime.Types
import Language
import qualified Language as L
import Runtime.Interpreter
import qualified TestInterpreter as TI

import Scenarios
import qualified Expression.Flow as FlowExpr
import qualified DB.Native as DB

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



dbConfig = DB.Config
student1, student2, student3, expelled1, expelled2 :: Student
student1  = Student 1 False
student2  = Student 2 False
student3  = Student 3 False
expelled1 = Student 4 True
expelled2 = Student 5 True


main :: IO ()
main = hspec $ do

  describe "Test interpreter" $ do
    it "test" $ do

      testRt <- TI.TestRuntime
        <$> TI.mkMocks [expelled1, expelled2, student1, student2, student3]
        <*> TI.mkMocks [expelled1, expelled2]
        <*> TI.mkMocks DB.MockedConn

      res <- TI.runFlow testRt getStudentsCountFlow
      res `shouldBe` 3


  describe "Students count scenarios tests" $ do
    it "Service Handle" $ do
      let handle = Handle DB.connect DB.query putStrLn
      result <- getStudentsCountSH handle "test_db" dbConfig
      result `shouldBe` 3


    it "Service Handle with mocks" $ do
      let allStudents = [student1, student2, student3, expelled1, expelled2]
      let expelledStudents = [expelled1, expelled2]
      let mockedConnect _ _ = pure DB.MockedConn
      let mockedQuery _ q
            | q == queryAll      = pure allStudents
            | q == queryExpelled = pure expelledStudents
      let handle = Handle mockedConnect mockedQuery putStrLn
      result <- getStudentsCountSH handle "test_db" dbConfig
      result `shouldBe` 3

  describe "Compare GUID scenarios tests" $ do
    it "Flow scenario" $ do
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
              -- let jsonRec = encode $ Recording entries
              -- jsonRec `shouldBe` "{\"entries\":[[0,\"Normal\",\"GenerateGUIDEntry\",{\"guid\":\"3a93686e-9b1a-4f02-84fd-1354221b0a63\"}],[1,\"Normal\",\"RunIOEntry\",{\"jsonResult\":\"58ee4992-31f6-11ea-978f-2e728ce88125\\n\"}],[2,\"Normal\",\"LogInfoEntry\",{\"message\":\"GUIDs are not equal.\"}]]}"
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"
