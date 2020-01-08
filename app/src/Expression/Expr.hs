{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Expression.Expr where

import           Data.Aeson            (FromJSON, ToJSON, Result(), decode, encode, eitherDecode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy  as BSL
import           GHC.Generics          (Generic)

data Precision
  = P1
  | P2
  | P4
  | P8
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data BinOp
  = Mul
  | Div
  | Add
  | Sub
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Val = Val Precision Double
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Expr
  = BinOpExpr { op :: BinOp, arg1 :: Expr, arg2 :: Expr }
  | ValExpr Double
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PrecExpr = PrecExpr
  { precision :: Precision
  , expression :: Expr
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

precVal :: Precision -> Double
precVal P1 = 0.1
precVal P2 = 0.01
precVal P4 = 0.0001
precVal P8 = 0.00000001

isZero :: Precision -> Double -> Bool
isZero p v = abs v <= precVal p

valsEqual :: Val -> Val -> Bool
valsEqual (Val p1 v1) (Val p2 v2)
  | p1 > p2   = abs (v1 - v2) <= precVal p1
  | otherwise = abs (v1 - v2) <= precVal p2

evalBinOp :: Precision -> BinOp -> Double -> Double -> Either String Double
evalBinOp prec Mul v1 v2 = Right $ v1 * v2
evalBinOp prec Sub v1 v2 = Right $ v1 - v2
evalBinOp prec Add v1 v2 = Right $ v1 + v2
evalBinOp prec Div v1 v2
  | isZero prec v2 = Left "Zero division"
  | otherwise = Right $ v1 / v2

eval :: PrecExpr -> Either String Val
eval (PrecExpr prec expr) = do
  val <- eval' prec expr
  pure $ Val prec val

eval' :: Precision -> Expr -> Either String Double
eval' prec (BinOpExpr op arg1 arg2) = do
  r1 <- eval' prec arg1
  r2 <- eval' prec arg2
  evalBinOp prec op r1 r2
eval' prec (ValExpr v) = Right v

demoExpr :: PrecExpr
demoExpr = PrecExpr P4
  ( BinOpExpr Mul
      (ValExpr 10.0)
      (ValExpr 2.0)
  )
