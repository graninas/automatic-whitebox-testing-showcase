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
  ( Connection
  , Config(..)
  , connect
  , query
  ) where

import           Data.Aeson   (FromJSON, ToJSON, decode, encode)
import           GHC.Generics (Generic)

data Connection = Connection String

data Config = Config
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

connect :: String -> Config -> IO Connection
connect dbName _ = pure $ Connection dbName

query :: Connection -> String -> IO a
query _ _ = error "Just a demo, not implemented"
