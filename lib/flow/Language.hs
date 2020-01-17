{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Language where

import           Control.Monad         (unless, void, when)
import           Control.Monad.Free
import           Data.Aeson            (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.IntMap           as MArr
import           Data.Maybe            (isJust)
import           Data.Monoid           ((<>))
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           Data.Typeable
import           GHC.Generics          (Generic)

import           Types
import           Runtime.Options

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
  Fork :: String -> String -> Flow s -> (() -> next) -> FlowF next
  RunSysCmd :: String -> (String -> next) -> FlowF next
  GetOption :: OptionEntity k v => k -> (Maybe v -> next) -> FlowF next
  SetOption :: OptionEntity k v => k -> v -> (() -> next) -> FlowF next

  Connect :: DBName -> DBConfig -> (DBConnection -> next) -> FlowF next
  RunDB :: (ToJSON s, FromJSON s, Typeable s)
        => DBConnection -> String -> Database s
        -> (s -> next) -> FlowF next


instance Functor FlowF where
  fmap f (GenerateGUID next)            = GenerateGUID (f . next)
  fmap f (RunIO ioAct next)             = RunIO ioAct (f . next)
  fmap f (LogInfo msg next)             = LogInfo msg (f . next)
  fmap f (Fork desc guid ioAct next)    = Fork desc guid ioAct (f.next)
  fmap f (RunSysCmd cmd next)           = RunSysCmd cmd (f.next)
  fmap f (GetOption k next)             = GetOption k (f.next)
  fmap f (SetOption k v next)           = SetOption k v (f.next)

  fmap f (Connect dbName dbConfig next) = Connect dbName dbConfig (f . next)
  fmap f (RunDB conn qInfo db next)     = RunDB conn qInfo db (f . next)

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

logInfo :: String -> Flow ()
logInfo msg = liftF $ LogInfo msg id

forkFlow :: (ToJSON s, FromJSON s) => String -> Flow s -> Flow ()
forkFlow description flow = do
  flowGUID <- generateGUID
  unless (null description) $ logInfo $ "Flow forked. Description: " <> description <> " GUID: " <> flowGUID
  when   (null description) $ logInfo $ "Flow forked. GUID: " <> flowGUID
  void $ liftF $ Fork description flowGUID flow id

runSysCmd :: String -> Flow String
runSysCmd cmd = liftF $ RunSysCmd cmd id

getOption :: OptionEntity k v => k -> Flow (Maybe v)
getOption k = liftF $ GetOption k id

setOption :: OptionEntity k v => k -> v -> Flow ()
setOption k v = liftF $ SetOption k v id

connect :: DBName -> DBConfig -> Flow DBConnection
connect dbName dbCfg = liftF $ Connect dbName dbCfg id

runDB
  :: (ToJSON s, FromJSON s, Typeable s)
  => DBConnection
  -> String
  -> Database s
  -> Flow s
runDB conn qInfo db = liftF $ RunDB conn qInfo db id

query :: (ToJSON s, FromJSON s, Typeable s) => DBConnection -> String -> Flow [s]
query conn q = runDB conn q $ query' q
