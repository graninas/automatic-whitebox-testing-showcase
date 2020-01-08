module Main where

import qualified Data.Map.Strict as Map
import           Control.Concurrent.MVar
import           System.Environment (getArgs)
import           Data.Aeson         (encode)
import qualified Data.ByteString.Lazy  as BSL

import qualified Language as L
import qualified Runtime.Types as R
import qualified Runtime.Interpreter as R

import qualified Expression.Flow as FlowExpr
import qualified Expression.IO as IOExpr
import Expression.Expr

main :: IO ()
main = do
  opts <- newMVar Map.empty
  let rt = R.Runtime R.RegularMode opts

  args <- getArgs
  mbFlowRes <- case args of
    fileName : _ -> R.runFlow rt $ FlowExpr.expressionScenario fileName
    _            -> R.runFlow rt $ FlowExpr.expressionScenario "app/def_expr.json"

  mbIORes <- case args of
    fileName : _ -> IOExpr.expressionScenario fileName
    _            -> IOExpr.expressionScenario "app/def_expr.json"

  case (mbFlowRes, mbIORes) of
    (Nothing, Nothing) -> putStrLn "No results"
    (Nothing, _) -> putStrLn "Different results (nothing in Flow)"
    (_, Nothing) -> putStrLn "Different results (nothing in IO)"
    (Just v1, Just v2)
      | valsEqual v1 v2 -> putStrLn "Values are equal."
      | otherwise -> putStrLn "Values are not equal."

  mbCircleArea <- R.runFlow rt $ FlowExpr.expressionScenario "app/circle_area.json"
  case mbCircleArea of
    Nothing   -> putStrLn "Failed to evaluate circle area."
    Just area -> putStrLn $ "Circle area: " ++ show area
