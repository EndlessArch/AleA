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
    ( alphaNumChar, char, digitChar, string )
import Control.Monad (join)
import Data.Void
import Text.Megaparsec.Error.Builder (utok, err)

type MParser = ParsecT Void String Identity

-- defunParser ::MParser Expr
-- defunParser = try $ do
--   M.char '('
--   M.string "defun"
--   let idf = idfParser
--   M.char ':'
--   where
--     faKeywordParser :: MParser Expr
--     faKeywordParser = try $ do
--       M.string "forall"

--     -- a.
--     -- (a, (unary closure)).
--     -- (a : (type expression), (unary closure)).
--     -- forall Type Variable Definition Parser
--     faTVDefParser :: MParser (String, Expr)
--     faTVDefParser = do
--       let name = idfStringParser
--       (pp tvSpecParser (return . try $ M.char '.'))
--       where
--         tvSpecParser :: MParser Expr
--         tvSpecParser =
--           (tvExplicitTypeParser dotP) <|> (tvAssignParser <&> dotP)
--           <|> dotP
--           where
--             tvExplicitTypeParser :: MParser Tp
--             tvExplicitTypeParser = try $ do
--               M.char ':'
--               pp typeDefParser

--             tvAssignParser :: MParser Expr
--             tvAssignParser = try $ do
--               M.char ','
--               pp exprParser

--             dotP :: MParser Void
--             dotP = try $ M.char '.'

-- pp :: MParser a -> MParser a
-- pp p = try $ do
--   (parenParser :: MParser a) <|> p
--   where
--     parenParser :: MParser a
--     parenParser = try $ do
--       M.char '('
--       p' <- pp p
--       M.char ')'
--       return p'

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
          if Prelude.null b
            then return . read $ ""
          else
            return . read $ a ++ '.':b

        posParser :: MParser Expr
        posParser = try $ do
          fmap Float decParserD <|> fmap Int intParserI

        negParser :: MParser Expr
        negParser = try $ do
          M.char '-'
          fmap (Float . negate) decParserD <|> fmap (Int . negate) intParserI

    retParser :: MParser Expr
    retParser = try $ do
      M.char '.'
      return $ Ret ()

-- idfStringParser :: MParser String
-- idfStringParser = try $ do
--   fst <- alphaNumChar
--   tail <- many alphaNumChar
--   return $ fst:tail

-- idfParser :: MParser Expr
-- idfParser = Idf <$> idfStringParser

idfParser :: MParser Expr
idfParser = try $ do
  let tok = many alphaNumChar
  str <- tok
  if Prelude.null str
     then do
       off <- getOffset
       err off (utok tok)
  else str