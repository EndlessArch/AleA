-- {-# LANGUAGE GADTs #-}
module Types where

import Data.Char
import qualified Data.List
import Data.Map
import qualified Data.List as List

type Name = String

data UOp
  = Increment | Decrement | Square
  deriving (Eq, Ord, Show)

data BOp = Add | Sub | Mul | Div
  | Mod | Pow
  | Eq | NEq
  deriving (Eq, Ord, Show)

data Op
  = UnaryOp UOp Expr
  | BinaryOp BOp Expr Expr
  | NOp
  deriving (Eq, Ord, Show)

data BasicFunction
  = Lambda [Tp] Expr
  | Defun String   -- Name
          [Expr] -- Forall []
          [Expr] -- Args []
          [Expr] -- Codes []
  deriving (Eq, Ord, Show)

data Tp
  = IntT | FloatT | CharT | BoolT | ArrayT Tp
  | CurryT [Expr]
  | CustomT String Expr
  deriving (Eq, Ord, Show)

data Val
  = Int   Integer
  | Float Double
  deriving (Eq, Ord, Show)

data Expr
  = Type      Tp
  | Value     Val
  | Idf       String
  | Var       String -- Idf
              (Maybe Expr) -- Type
              (Maybe Expr) -- Value
  | Args      [Expr]
  | Function  BasicFunction
  | Operator  Op
  | Ret       ()
  deriving (Eq, Ord, Show)