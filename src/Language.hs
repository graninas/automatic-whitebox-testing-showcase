{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}

module Language where

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

data DBConfig = DBConfig
data Connection = Connection

data DatabaseF next where
  Connect :: String -> DBConfig -> (Connection -> next) -> DatabaseF next
  Query :: Connection -> String -> ([a] -> next) -> DatabaseF next

instance Functor DatabaseF where
  fmap f (Connect dbName dbConfig next) = Connect dbName dbConfig (f . next)
  fmap f (Query conn q next)  = Query conn q (f . next)

type Database a = Free DatabaseF a

connect :: String -> DBConfig -> Database Connection
connect dbName dbCfg = liftF $ Connect dbName dbCfg id

query :: Connection -> String -> Database [a]
query conn q = liftF $ Query conn q id


data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next
  RunDB :: (ToJSON s, FromJSON s) => Database s -> (s -> next) -> FlowF next

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

runDB :: (ToJSON s, FromJSON s) => Database s -> Flow s
runDB db = liftF $ RunDB db id
