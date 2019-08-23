{-# LANGUAGE FunctionalDependencies    #-}


module Runtime.Options where

import           Data.Aeson         (ToJSON, FromJSON)

class (FromJSON k, FromJSON v, ToJSON k, ToJSON v) => OptionEntity k v |  k -> v