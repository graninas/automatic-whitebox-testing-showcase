
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

import           Control.Concurrent.MVar
import           Control.Monad        (when, unless)
import qualified Data.Map.Strict      as Map
import qualified Data.Vector          as V
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson           (decode, encode, toJSON)
import           Data.UUID            (toString)
import           Data.UUID.V4         (nextRandom)
import           Test.Hspec

import Playback.Types
import Runtime.Types
import Language
import Types
import qualified Language as L
import Runtime.Interpreter
import Scenarios
import qualified Expression.Flow as FlowExpr

initRegularRT = do
  opts <- newMVar Map.empty
  pure $ Runtime
   { runMode = RegularMode
   , runtimeData = Left $ OperationalData opts
   }

initRecorderRT mbMocks = do
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
    , runtimeData = case mbMocks of
        Nothing -> Left $ OperationalData opts
        Just mocks -> Right mocks
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
    , runtimeData = Left $ OperationalData opts
    }

getRecording :: Runtime -> IO RecordingEntries
getRecording rt = case runMode rt of
  RecordingMode rrt -> readMVar $ recordingMVar rrt
  _ -> error "wrong mode."

getErrors :: Runtime -> IO (Maybe PlaybackError)
getErrors rt = case runMode rt of
  ReplayingMode prtm -> readMVar $ errorMVar prtm
  _ -> error "wrong mode."

main :: IO ()
main = hspec $ do

  describe "Students count scenarios tests" $ do
    it "Interpreter with mocks" $ do

      mockedData <- MockedData
        <$> mkMocks @Int []
        <*> mkMocks [ MockedConnection "test_db" ]
        <*> mkMocks [ [expelled1, expelled2, student1, student2, student3]
                    , [expelled1, expelled2] ]

      let testRt = Runtime RegularMode (Right mockedData)

      res <- runFlow testRt $ getStudentsCountFlow "test_db" dbConfig
      res `shouldBe` 3

    -- it "Service Handle without mocks" $ do
    --   let handle = Handle DB.connect DB.query putStrLn
    --   result <- getStudentsCountSH handle "test_db" dbConfig
    --   result `shouldBe` 3

    it "Service Handle with mocks" $ do
      let allStudents = [student1, student2, student3, expelled1, expelled2]
      let expelledStudents = [expelled1, expelled2]
      let mockedConnect _ _ = pure $ MockedConn $ MockedConnection "test_db"
      let mockedQuery _ q
            | q == queryAll      = pure allStudents
            | q == queryExpelled = pure expelledStudents
      let handle = Handle mockedConnect mockedQuery putStrLn
      result <- getStudentsCountSH handle "test_db" dbConfig
      result `shouldBe` 3

    it "Flow recordings with mocks" $ do
      mockedData <- MockedData
        <$> mkMocks @Int []
        <*> mkMocks [ MockedConnection "test_db" ]
        <*> mkMocks [ [expelled1, expelled2, student1, student2, student3]
                    , [expelled1, expelled2] ]

      rt <- initRecorderRT $ Just mockedData
      runFlow rt $ getStudentsCountFlow "test_db" dbConfig
      entries <- getRecording rt
      pRt <- initPlayerRT entries
      runFlow pRt $ getStudentsCountFlow "test_db" dbConfig
      errors <- getErrors pRt
      errors `shouldBe` Nothing


  describe "Compare GUID scenarios tests" $ do
    it "Flow scenario" $ do
      rt <- initRecorderRT Nothing
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
