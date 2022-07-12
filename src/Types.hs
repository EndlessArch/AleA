-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings  #-}
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
  | DefineFunction Name Expr
  deriving (Eq, Ord, Show)

data Tp
  = IntT | FloatT | CharT | ArrayT Tp
  | CustomT String
  deriving (Eq, Ord, Show)

data Expr
  = Type      Tp
  | Int       Integer
  | Float     Double
  | Idf       String
  | Function  BasicFunction
  | Operator  Op
  deriving (Eq, Ord, Show)