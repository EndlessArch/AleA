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

import Text.Builder
import Data.Functor.Identity (Identity)
import Text.Megaparsec as M
import Text.Megaparsec.Char as M
import Control.Monad (join)
import Data.Void

type MParser = ParsecT Void String Identity

exprParser :: MParser Expr
exprParser = try $ do
  numberParser <|> idfParser
  where
    numberParser :: MParser Expr
    numberParser = posParser <|> negParser
      where
        intParserI :: MParser Integer
        intParserI = try $ do
          itgr <- some digitChar
          return $ read itgr

        decParserD :: MParser Double
        decParserD = try $ do
          a <- many digitChar
          M.char '.'
          b <- some digitChar
          return . read $ a ++ '.':b

        posParser :: MParser Expr
        posParser = try $ do
          fmap Float decParserD <|> fmap Int intParserI 

        negParser :: MParser Expr
        negParser = try $ do
          M.char '-'
          fmap (Float . (* (-1))) decParserD <|> fmap (Int . (* (-1))) intParserI

    idfParser :: MParser Expr
    idfParser = try $ do
      str <- many alphaNumChar
      return $ Idf str