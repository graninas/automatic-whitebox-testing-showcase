{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Language where

import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe            (isJust)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           GHC.Generics          (Generic)

import qualified DB.Native             as DB
import           Types

data DatabaseF next where
  Query :: String -> ([a] -> next) -> DatabaseF next

instance Functor DatabaseF where
  fmap f (Query q next) = Query q (f . next)

type Database a = Free DatabaseF a

query' :: String -> Database [a]
query' q = liftF $ Query q id


data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next

  Connect :: DBName -> DB.Config -> (Connection -> next) -> FlowF next
  RunDB :: (ToJSON s, FromJSON s)
        => Connection -> String -> Database s
        -> (s -> next) -> FlowF next


instance Functor FlowF where
  fmap f (GenerateGUID next)            = GenerateGUID (f . next)
  fmap f (RunIO ioAct next)             = RunIO ioAct (f . next)
  fmap f (LogInfo msg next)             = LogInfo msg (f . next)

  fmap f (Connect dbName dbConfig next) = Connect dbName dbConfig (f . next)
  fmap f (RunDB conn qInfo db next)     = RunDB conn qInfo db (f . next)

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

logInfo :: String -> Flow ()
logInfo msg = liftF $ LogInfo msg id

connect :: DBName -> DB.Config -> Flow Connection
connect dbName dbCfg = liftF $ Connect dbName dbCfg id

runDB
  :: (ToJSON s, FromJSON s)
  => Connection
  -> String
  -> Database s
  -> Flow s
runDB conn qInfo db = liftF $ RunDB conn qInfo db id

query :: (ToJSON s, FromJSON s) => Connection -> String -> Flow [s]
query conn q = runDB conn q $ query' q
