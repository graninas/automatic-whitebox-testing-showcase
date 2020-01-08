{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Expression.IO where

import           Control.Monad         (unless, void, when)
import           Data.Aeson            (FromJSON, ToJSON, Result(), decode, encode, eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy  as BSL
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           Data.Either           (either)
import           GHC.Generics          (Generic)

import           Expression.Expr

expressionScenario :: String -> IO (Maybe Val)
expressionScenario fileName = do
  jsonRequest <- readFile fileName
  let eRes = do
        precExpr <- eitherDecode $ BCL.pack jsonRequest
        eval precExpr
  case eRes of
    Left err  -> putStrLn $ "Error got: " ++ err
    Right res -> putStrLn $ "Result: " ++ show res
  pure $ either (const Nothing) Just eRes
