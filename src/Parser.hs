{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE BlockArguments #-}
module Parser where

import Data.Functor

import Types
import PrettyLog

import Text.Builder
import Text.Megaparsec as M
import Text.Megaparsec.Char as M
    ( alphaNumChar, char, digitChar, string, space, space1, letterChar, symbolChar, punctuationChar )
import Data.Void

type MParser = Parsec Void String

defunParser :: MParser Expr
defunParser = try $ do

  let funcNameParser = (idfStringParser <|>) $
        try
          (many (symbolChar <|> punctuationChar
            -- <|> M.oneOf [
            --   '!', '@', '#', '$', '%', '^',
            --   '&', '*', '_', '?', ':', '~'
            --   ]
              ) >>= \s -> return s)

  -- (defun someFunc : forall ~. ~ -> ~ => ~ )
  -- ^~~~~~~~~~~~~~^
  M.char '(' >> space >> M.string "defun" >> space1
  idf <- funcNameParser

  do
    -- (defun someFunc : forall ~. ~ -> ~ => ~ )
    --                 ^~~~~~~~~~^
    space >> M.char ':' >> space

    let
      -- (~ : forall . ~ -> ~ => ~ )
      --      ^~~~~~~^
      emptyFA = try $ M.string "forall" >> space >> "." $> []

      -- (~ : forall a (b : b_t) (c: c_t, c_v). ~~
      --      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^
      nonEmptyFA =
        let tvParser = pp argParser
        in try $ do
          M.string "forall"
          (many (space1 *> tvParser) <* M.char '.')
            <|> (M.char '.' $> [])

    tpdfs <- nonEmptyFA <|> emptyFA

    -- (defun ~ : ~ . ~ -> ~ => ~ )
    --               ^~~~~~~^
    args <- space *> many (pp argParser)
    ret <- space *> M.string "->" *> space *> pp typeExprParser

    -- (defun ~ : ~ . ~ -> ~ => ~ )
    --                       ^~~~^
    exprs <- space *> M.string "=>" *> space
      *> some (pp exprParser <|> (M.char ')' $> Ret ()))

    return . Function $ Defun idf tpdfs (args ++ [ret]) exprs

  where
    -- a
    -- a, (unary closure)
    -- a : (type expression), (unary closure)
    -- Type Variable Definition Parser
    argParser :: MParser Expr
    argParser =
      do
        name <- idfStringParser

        tvParser name <|> return (Var name Nothing Nothing)
      where
        tvParser :: String -> MParser Expr
        tvParser name =
          let exprM =
                tvTypeParser name
                <|> tvValueParser name
                -- <|> try (M.char '.' $> Var iName Nothing Nothing)
          in
            -- when `tvExplicitTypeParser` parsed,
            -- tries parsing `tvAssigneParser`.
            exprM >>= \expr ->
              case expr of
                (Var _ (Just tp) val) -> do
                  assigned <- tvValueParser name
                  case assigned of
                    (Var _ _ (Just v)) ->
                      return $ Var name (Just tp) (Just v)
                    _ -> return expr
                  <|> return expr
                (Var _ Nothing _) -> return expr
                _ -> return expr -- parsed sth unexpected
          where
            -- parse ": type-expression"
            tvTypeParser :: String -> MParser Expr
            tvTypeParser name = try $
              do
                space >> M.char ':' >> space >> do
                  tp <- pp typeExprParser -- parse type-level expression
                  return $ Var name (Just tp) Nothing

            -- parse ", expression"
            tvValueParser :: String -> MParser Expr
            tvValueParser name = try $
                do
                  space >> M.char ',' >> space >> do
                    val <- pp exprParser -- parse value-level expression
                    return . Var name Nothing $ Just val

typeExprParser :: MParser Expr
typeExprParser = try $ do
  s <- idfStringParser
  return . Type $
    case s of
      "Int" -> IntT
      "Float" -> FloatT
      "Char" -> CharT
      "Bool" -> BoolT
      "String" -> ArrayT CharT
      _ -> CustomT s $ Ret ()

-- acts like strip, but removes parentheses
pp :: forall a. MParser a -> MParser a
pp p =
  try (
    (M.char '(' >> space >>) $
      pp p <* (space >> M.char ')'))
  <|> p
  
exprParser :: MParser Expr
exprParser = try $ numberParser <|> idfParser
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
          fmap (Value . Float) decParserD <|> fmap (Value . Int) intParserI

        negParser :: MParser Expr
        negParser = try $ do
          M.char '-'
          fmap (Value . Float . negate) decParserD
            <|> fmap (Value . Int . negate) intParserI

    retParser :: MParser Expr
    retParser = try $ do
      M.char '.'
      return $ Ret ()

idfStringParser :: MParser String
idfStringParser = try $ do
  fst <- letterChar
  tail <- many alphaNumChar
  return $ fst:tail

idfParser :: MParser Expr
idfParser = Idf <$> idfStringParser