{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE RankNTypes                #-}

module Scenarios where

import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Maybe            (isJust)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString, fromString)
import           Data.UUID.V4          (nextRandom)
import           GHC.Generics          (Generic)

import qualified DB.Native             as DB
import           Language
import qualified Language              as L


data Student = Student Int Bool
  deriving (Generic, ToJSON, FromJSON)

type Students = [Student]

loadOrGenerateGuidIO :: String -> IO String
loadOrGenerateGuidIO fileName = do
  mbGuid <- fromString <$> readFile fileName
  case mbGuid of
    Just (show -> guid) -> do
      putStrLn $ "Guid loaded: " ++ guid
      pure guid
    Nothing -> do
      newGuid <- toString <$> nextRandom
      writeFile fileName newGuid
      putStrLn $ "Guid generated: " ++ newGuid
      pure newGuid

queryAll      = "SELECT * FROM students"
queryExpelled = "SELECT * FROM students WHERE expelled = 1"

type DBName = String
type Query = String
type DBConfig = DB.Config
type DBConnection = DB.Connection

data Handle = Handle
  { hConnect :: DBName -> DBConfig -> IO DBConnection
  , hQuery :: DBConnection -> Query -> IO Students
  , hLogInfo :: String -> IO ()
  }


getStudentsCountIO :: DBName -> DB.Config -> IO Int
getStudentsCountIO dbName cfg = do
  conn <- DB.connect dbName cfg
  students <- DB.query @Students conn queryAll
  expelled <- DB.query @Students conn queryExpelled

  let count = length students - length expelled
  when (count == 0) $ putStrLn "No records found."
  pure count


getStudentsCountSH :: Handle -> DBName -> DBConfig -> IO Int
getStudentsCountSH handle dbName cfg = do
  conn     <- hConnect handle dbName cfg

  students <- hQuery handle conn queryAll
  expelled <- hQuery handle conn queryExpelled

  let count = length students - length expelled

  when (count == 0) $ hLogInfo handle "No records found."
  pure count


getStudentsCountFlow :: String -> DB.Config -> Flow Int
getStudentsCountFlow dbName cfg = do
  conn     <- L.connect dbName cfg
  students <- L.query @Students conn queryAll
  expelled <- L.query @Students conn queryExpelled

  let count = length students - length expelled
  when (count == 0) $ L.logInfo "No records found."
  pure count



compareGUIDs :: String -> Flow ()
compareGUIDs fileName = do
  newGuid <- generateGUID
  oldGuid <- runIO $ readFile fileName

  let equal = newGuid == oldGuid
  when equal $ logInfo "GUIDs are equal."
  unless equal $ logInfo "GUIDs are not equal."

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


-- getStudentsCount :: String -> DB.Config -> Flow Int
-- getStudentsCount dbName cfg = do
--   (students :: [Student]) <- runDB $ do
--     conn <- connect dbName cfg
--     query conn "SELECT * FROM students"
--   pure $ length students

getStudentsCount :: String -> DB.Config -> Flow Int
getStudentsCount dbName cfg = do
  conn <- connect dbName cfg
  (students :: [Student]) <- query conn "SELECT * FROM students"
  (disabled :: [Student]) <- query conn "SELECT * FROM students WHERE disabled=1"
  let count = length students - length disabled
  when (count == 0) $ logInfo "No records found."
  pure count
