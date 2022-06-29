{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Parser where

import Control.Exception (Exception, throwIO)
import Data.Functor
import qualified Data.List as List
import Data.Maybe
import Data.Text hiding
  ( any
  , map
  )
import Data.Tuple.Extra
import Prelude hiding
  ( all
  , drop
  , elem
  , head
  , init
  , last
  , length
  , tail
  )
import Text.Read

import Text
import Types
import PrettyLog
import ListUtil
import qualified Data.Foldable as Prelude
import Control.Monad
import Data.Char (isNumber, isAlphaNum)

import Debug.Trace (trace)
import Data.List (sortBy)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)

type Priority = Float

-- return ordering but as flipped priority
rtnExprOrd :: ReturnExpr -> ReturnExpr -> Ordering
rtnExprOrd (_, x) (_, y)
  = case x of
    ReturnSuccess ex px
      ->  case y of
          ReturnSuccess ey py -> compare py px
          ReturnFail _ _ -> LT
    ReturnFail _ px
      ->  case y of
          ReturnSuccess _ _ -> GT
          ReturnFail _ py -> compare py px

data ReturnStatus a
  = ReturnSuccess a Priority
  | ReturnFail String Priority

class ShowValue a where
  showVal :: a -> String

instance (Show a) => ShowValue (ReturnStatus a) where
  showVal (ReturnSuccess e p)
    = "RtnSuc (" ++ show e ++ " | " ++ show p ++ ")"
  showVal (ReturnFail s p)
    = "RtnFail: " ++ s ++ " | " ++ show p ++ ")"

type ReturnExpr = (Text, ReturnStatus Expr)

instance ShowValue ReturnExpr where
  showVal (r, rs) = show r ++ ", " ++ showVal rs

parseExpression :: Text -> ReturnExpr
parseExpression ""
  = ("", ReturnFail "Unable to parse expression; No input given" 1.0)

-- NOTE: `s` isn't null
parseExpression s = do
  let parsers = getParserList
      (s1, s2) = wordParser s
      parsed = map ($ (s1, s2)) parsers
      topPriorityExpr :: [ReturnExpr] = sortBy rtnExprOrd parsed -- sortAsPriority parsed

  case topPriorityExpr of
    (a:b:_)
          -> if rtnExprOrd a b == EQ then
        ( ""
        , ReturnFail
          ("Ambigous expression ("
          ++ showVal a ++ " | " ++ showVal b ++ ")") 1.0)
        else a
    -- something happened which should not be
    [] -> ("", ReturnFail "No any parser activated (Internal)" 100.0)
    _ -> topPriorityExpr!!1

getParserList :: [(Text, Text) -> ReturnExpr]
getParserList
  = -- map (wordParser <&>)
    [ numberValueParser
    , variableParser
    -- , lambdaParser -- 
    -- , operatorParser -- 80.0
    -- , namedFunctionParser -- 80.0
    -- , macroParser -- 100.0
    ]

data ParenStruct
  = Text | ParenStruct

-- NOTE: `t` contains at least one '('
parseParen :: Text -> (Text, Text | (Text -> (Text, Tex)))
parseParen t
  | (!) $ '(' `isInfixOf` t = [("", t)]
  | otherwise
  = let (a, b) = splitWith '(' t
        (a', b') = (stripEnd a, stripStart b)
    in '(' `isInfixOf` b'
    | True -> parseParen

wordParser :: Text -> (Text, Text)
wordParser a b =
  let (c, d) = splitWith a b
  in if '(' `isInfixOf` c
    then parseParen a
  else (c, d)

numberValueParser :: (Text, Text) -> ReturnExpr
numberValueParser (s, rs)
  = let intParser = readMaybe . unpack :: Text -> Maybe Integer
        fpParser = readMaybe . unpack :: Text -> Maybe Double
        rtn = case intParser s of
          Just i -> ReturnSuccess (Int i)
          Nothing -> case fpParser s of
            Just f -> ReturnSuccess (Float f)
            Nothing -> ReturnFail "Failed to parse number"
  in (rs, rtn 5.0)

variableParser :: (Text, Text) -> ReturnExpr
variableParser (s, rs)
  = let rtn
          = if isNumber (head s) && all isAlphaNum (tail s)
              then ReturnSuccess (Var s)
            else ReturnFail "Unexpected identifier"
    in (rs, rtn 1.0)

following :: Text -> Text -> ReturnStatus (Text, Text)
following a b
  = let (c, d) = splitWith a b
        (e, f) = (stripEnd c, stripStart d)
    in if e == "" then ReturnFail ("Expected " ++ b)
    else (e, f)

-- (|\a -> )

-- lambdaParser :: (Text, Text) -> ReturnExpr
-- lambdaParser (s, rs)
--   = let rtn
--     = 