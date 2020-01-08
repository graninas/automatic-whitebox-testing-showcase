{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Expression.Flow where

import           Control.Monad         (unless, void, when)
import           Data.Aeson            (FromJSON, ToJSON, Result(), decode, encode, eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy  as BSL
import           Data.UUID             (toString)
import           Data.UUID.V4          (nextRandom)
import           Data.Either           (either)
import           GHC.Generics          (Generic)

import qualified Language as L
import Expression.Expr

expressionScenario :: String -> L.Flow (Maybe Val)
expressionScenario fileName = do
  jsonRequest <- L.runIO $ readFile fileName
  let eRes = do
        precExpr <- eitherDecode $ BCL.pack jsonRequest
        eval precExpr
  case eRes of
    Left err  -> L.logInfo $ "Error got: " ++ err
    Right res -> L.logInfo $ "Result: " ++ show res
  pure $ either (const Nothing) Just eRes
