-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types where

import Prelude hiding
  ( head
  , init
  , last
  , tail
  )

import Data.Char
import qualified Data.List
import Data.Map
import Data.Text
import qualified Data.List as List

type Name = Text

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
  | Call Name Expr | Define Text Text | Substitute Text Text
  | DefineSystem Name Expr | ImportSystem Name | RedefineSystem Name Text
  | DefineFunction [Tp] Name Expr | MakeVariable Name (Maybe Expr) | RemoveVariable Name
  | IF Expr Expr Expr | Hook Name Expr
  deriving (Eq, Ord, Show)

data Tp
  = IntT | FloatT | CharT | ArrayT Tp | StructT [Tp]
  | Untyped String
  deriving (Eq, Ord)

instance Show Tp where
  show IntT = "Int"
  show FloatT = "Float"
  show CharT = "Char"
  show (ArrayT t) = '[':show t ++ "]"
  show (StructT ts)
    = '(': show ts ++ ")"
  show (Untyped s) = s

data Expr
  = Type      Tp
  | Int       Integer
  | Float     Double
  | Var       Text
  | Function  BasicFunction
  | Operator  Op
  | Hole      Int
  deriving (Eq, Ord, Show)

-- `s` should be strip-ed
findType :: Text -> Maybe Tp
findType s =
  if '[' == head s then
    if ']' /= last s
      then Nothing
    else
      let t = findType . init $ tail s
      in case t of
        Just t' -> Just $ ArrayT t'
        Nothing -> Nothing
  else
    case s of
      "Int" -> Just IntT
      "Float" -> Just FloatT
      "Char" -> Just CharT
      "Struct" -> Just $ StructT [] -- TODO: Parse {| ~ |}
      _ -> Nothing

getType :: Expr -> [Tp]
getType (Type t) = [t]
getType (Int _) = [IntT]
getType (Float _) = [FloatT]
getType (Var v) = [Untyped $ unpack v]
getType (Function f)
  = case f of
    Lambda ts _ -> ts
    Call n _ -> [Untyped $ unpack n]
    DefineSystem n e -> getType e ++ [Untyped $ unpack n]
    MakeVariable n _ -> [Untyped $ unpack n]
    IF _ e1 e2 ->
      let t1 = getType e1
          t2 = getType e2 in
            if t1 /= t2 then []
            else t1
    _ -> []

getType (Operator op)
  = case op of
    UnaryOp _ e -> getType e
    BinaryOp _ e1 e2 -> getType e1 ++ getType e2
    NOp -> []

getType (Hole _) = []

getNextHoleType :: Expr -> Tp
getNextHoleType = Untyped . __getNextHoleType

-- getHoleType :: Expr -> Tp
-- getHoleType = __getNextHoleType

__getNextHoleType :: Expr -> String
__getNextHoleType (Function f)
  = let nht = __getNextHoleType
  in case f of
    Lambda ts e -> max (nht e) (nht . Type $ StructT ts)
    Call _ e -> nht e
    DefineSystem _ e -> nht e
    DefineFunction ts _ e -> max (nht e) (nht . Type $ StructT ts)
    MakeVariable _ (Just e) -> nht e
    IF e1 e2 e3 ->
      (\a b c -> max a $ max b c)
        (nht e1)
        (nht e2)
        (nht e3)
    Hook _ e -> nht e
    _ -> "a"

__getNextHoleType (Operator op)
  = let nht = __getNextHoleType
  in case op of
    UnaryOp _ e -> nht e
    BinaryOp _ e1 e2 -> max (nht e1) (nht e2)
    NOp -> "a"

__getNextHoleType (Type t)
  = let nht = __getNextHoleType
  in case t of
    ArrayT tp -> nht $ Type tp
    StructT (t:ts) -> max (nht $ Type t) (nht . Type $ StructT ts)
    Untyped s -> __rtnNextS s
    _ -> "a"

__getNextHoleType _ = "a"

__mkNextC :: Char -> Char
__mkNextC 'z' = 'a'
__mkNextC c = chr (1 + ord c)

__rtnNextS :: String -> String
__rtnNextS s
  = if List.last s == 'z' then s ++ ['a']
  else List.init s ++ [__mkNextC (List.last s)]

getNextHole :: Expr -> Expr
getNextHole = Hole . __getNextHole

__getNextHole :: Expr -> Int
__getNextHole (Function f)
    = let nh = __getNextHole
    in case f of
        Lambda _ e -> nh e
        Call _ e -> nh e
        DefineSystem _ e -> nh e
        DefineFunction _ _ e -> nh e
        MakeVariable _ (Just e) -> nh e
        IF e1 e2 e3 ->
          (\a b c -> max a $ max b c)
            (nh e1)
            (nh e2)
            (nh e3)
        Hook _ e -> nh e
        _ -> 0
__getNextHole (Operator op)
    = let nh = __getNextHole
    in case op of
      UnaryOp u'o e -> nh e
      BinaryOp b'o e1 e2
        -> max (nh e1) $ nh e2
      NOp -> 0
__getNextHole (Hole i)    = i + 1
__getNextHole _ = 0