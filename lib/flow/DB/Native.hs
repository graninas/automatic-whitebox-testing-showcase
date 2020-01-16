-- Fake "native DB library" module

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module DB.Native
  ( NativeConnection (..)
  , DBConfig (..)
  , Query
  , connect
  , query
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

newtype NativeConnection = NativeConnection String

data DBConfig = DBConfig
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type Query = String

connect :: String -> DBConfig -> IO NativeConnection
connect dbName _ = pure $ NativeConnection dbName

query :: NativeConnection -> String -> IO a
query _ _ = error "Just a demo, not implemented"
