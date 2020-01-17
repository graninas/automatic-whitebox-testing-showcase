{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Playback where

import           Control.Concurrent.MVar
import qualified Data.Map.Strict      as Map
import           Data.Aeson           (ToJSON, FromJSON, Value, Result(..), encode, decode, eitherDecode, toJSON, fromJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)

import qualified Language as L
import qualified Types as L
import qualified Runtime.Types as R
import qualified Playback.Types as R
import qualified Runtime.Interpreter as R

data FlowRecording = FlowRecording
  { recording :: R.Recording
  , jsonResult :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

getRecording :: R.Runtime -> IO R.Recording
getRecording rt = case R.runMode rt of
  R.RecordingMode rrt -> R.Recording <$> readMVar (R.recordingMVar rrt)
  _ -> error "wrong mode."

getErrors :: R.Runtime -> IO (Maybe R.PlaybackError)
getErrors rt = case R.runMode rt of
  R.ReplayingMode prtm -> readMVar $ R.errorMVar prtm
  _ -> error "wrong mode."

initRecorderRT :: Maybe R.MockedData -> IO R.Runtime
initRecorderRT mbMocks = do
  recMVar       <- newMVar V.empty
  forkedRecMvar <- newMVar Map.empty
  opts          <- newMVar Map.empty
  let recRt = R.RecorderRuntime "" recMVar forkedRecMvar []
  pure $ R.Runtime
    { R.runMode = R.RecordingMode recRt
    , R.runtimeData = case mbMocks of
        Nothing -> Left $ R.OperationalData opts
        Just mocks -> Right mocks
    }

initPlayerRT :: R.RecordingEntries -> IO R.Runtime
initPlayerRT recEntries = do
  opts    <- newMVar Map.empty
  step    <- newMVar 0
  errMVar <- newMVar Nothing
  ffEV    <- newMVar Map.empty
  let pRt = R.PlayerRuntime recEntries step errMVar [] [] [] False "" Map.empty ffEV
  pure $ R.Runtime
    { R.runMode = R.ReplayingMode pRt
    , R.runtimeData = Left $ R.OperationalData opts
    }

recorder
  :: (ToJSON a, FromJSON a)
  => Maybe R.MockedData -> String -> L.Flow a -> IO ()
recorder mbMockedData fName flow = do

  rt <- initRecorderRT mbMockedData

  res <- R.runFlow rt flow
  recording <- getRecording rt

  BSL.writeFile fName $ encode $ FlowRecording recording (toJSON res)

player
  :: (ToJSON a, FromJSON a, Show a, Eq a)
  => String -> L.Flow a -> IO ()
player fName flow = do
  mbFlowRecording <- eitherDecode <$> BSL.readFile fName
  case mbFlowRecording of
    Left err -> putStrLn $ "Failed to parse recordings: " ++ err
    Right (FlowRecording r jsonRes) -> do
      rt <- initPlayerRT $ R.entries r

      res <- R.runFlow rt flow
      mbErrors <- getErrors rt

      case (mbErrors, fromJSON jsonRes) of
        (_, Error err) -> putStrLn $ "Failed to decode result from recordings: " ++ err
        (Nothing, Success res') | res == res' -> putStrLn "Successfully replayed."
        (Nothing, Success res') | res /= res' -> do
          putStrLn "Results do not match."
          putStrLn $ "Expected from recordings: \n" ++ show res'
          putStrLn $ "Got from flow: \n" ++ show res
        (Just err, _) -> putStrLn $ "Playback failed: " ++ show err
