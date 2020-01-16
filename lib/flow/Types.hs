{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Types
  ( module Types
  , module X
  ) where

import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)
import           DB.Native             (NativeConnection)

import           DB.Native             as X (DBConfig(..), Query)

type DBName = String

newtype MockedConnection = MockedConnection DBName
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data DBConnection
  = NativeConn DBName NativeConnection
  | MockedConn MockedConnection
