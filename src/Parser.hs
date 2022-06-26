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
  ( drop
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
import Data.Char (isNumber)

import Debug.Trace (trace)

data Priority
  = I -- Macro
  | II
  | III
  deriving (Eq, Show)

instance Ord Priority where
  II < I = True
  III < II = True
  _ < _ = False

  I <= I = True
  II <= II = True
  III <= III = True
  a <= b = a < b

data ParserReturnState a b
  = ReturnSuccess a
  | InvalidArgs String b
  | ReturnFail String
  deriving (Show)

-- parse Text which start's with parenthesis
parseBracket :: (Text, Text) -> Text -> (Text, Text)
parseBracket (b1, b2) s
  | b1 `isInfixOf` s && b2 `isInfixOf` s =
    let s' = strip s in
      if not $ b1 `isPrefixOf` s' then
        ("", s)
      else
        let l = length b1
            s'1 = drop l s'
            -- s'2 = dropEnd (length b2) s'1
            (s'2, s'r) = splitWith b2 s'1
            in (stripStart s'2, s'r)
  | otherwise = ("", s)

-- ( ~ )
parseParen :: Text -> (Text, Text)
parseParen = parseBracket ("(", ")")

-- {| ~ |}
parseBrace :: Text -> (Text, Text)
parseBrace = parseBracket ("{|", "|}")

parseIdentifier :: Text -> (Text, Text)
parseIdentifier "" = ("", "")
parseIdentifier s =
  let s' = strip s
  in both strip $ splitWith " " s'

intParser :: Text -> ParserReturnState (Expr, Priority) ()
intParser s =
  let s' = strip s
      rm = readMaybe . unpack :: Text -> Maybe Integer
    in case rm s' of
      Nothing ->
        if Data.Text.null s'
          then ReturnFail "No any input given"
        else
          if isNumber $ head s'
            then InvalidArgs "Invalid integer expression" ()
          else ReturnFail "Not an integer expression at all"
      Just x -> ReturnSuccess (Int x, II)

fpParser :: Text -> ParserReturnState (Expr, Priority) ()
fpParser s =
  let s' = strip s
      rm = readMaybe . unpack :: Text -> Maybe Double
    in case rm s' of
      Nothing ->
        if Data.Text.null s'
          then ReturnFail "No any input given"
        else
          if isNumber (head s') || '.' == head s'
            then InvalidArgs "Expected floating point expression" ()
          else ReturnFail "Not a floating point expression at all"
      Just x -> ReturnSuccess (Float x, III)

getBasicFunction :: Text -> Maybe BasicFunction
getBasicFunction s = let s' = strip s in
  let nullExpr :: Expr = Var ""
  in case s' of
    "__call"  -> Just $ Call "" nullExpr
    "__df"    -> Just $ Define "" ""
    "__sbs"   -> Just $ Substitute "" ""
    "__df_sys"  -> Just $ DefineSystem "" nullExpr
    "__im_sys"  -> Just $ ImportSystem ""
    "__rdf_sys" -> Just $ RedefineSystem "" ""
    "__dfun"  -> Just $ DefineFunction [] "" nullExpr
    "__mkvar" -> Just $ MakeVariable "" Nothing
    "__rmvar" -> Just $ RemoveVariable ""
    "__if"  -> Just $ IF nullExpr nullExpr nullExpr
    "__hk"  -> Just $ Hook "" nullExpr
    _ -> Nothing

-- ( (only applied to ->) "A -> B -> C" )
parseCurryType :: Text -> Maybe [Tp]
parseCurryType s
  | not $ "->" `isInfixOf` s =
    let f = findType $ strip s
    in case f of
      Just tp -> Just [tp]
      Nothing -> Nothing
  | otherwise
  = let tps = splitOn "->" $ strip s
    in
      if Prelude.null tps
        then Nothing
      else
        let m = map (findType . strip) tps
        in if any isNothing m
          then Nothing
        else
          Just $ catMaybes m

-- String parameter should be strip-ed
-- __func EXPR optional EXPR...
basicFunctionParser :: Text -> ParserReturnState (Text, Expr, Priority) Text
basicFunctionParser s =
  if not $ "__" `isPrefixOf` s then
    ReturnFail "Not a function at all"
  else
    let (fKind, s'r) = parseIdentifier s
        (fTypes, s'r') = parseParen s'r
        f = getBasicFunction $ strip fKind
    in case f of
      Nothing ->
        InvalidArgs ("Unexpected function name, '" ++ unpack fKind ++ "'") s'r'
      Just f' ->
        case f' of
          DefineFunction {} ->
            let maybeType = parseCurryType fTypes
            in
              case maybeType of
                Just tp ->
                  let (fName, s'r'') = parseIdentifier s'r' in
                  if Data.Text.null fName
                    then InvalidArgs "No function name given" s'r''
                  else
                    let (s', exp) = parseExpression s'r''
                    in case exp of
                      ParseSuccess expr
                        -> ReturnSuccess (s'r, Function $ DefineFunction tp fName expr, I)
                      ParseFail e
                        -> InvalidArgs ("Unable to parse expression; " ++ e) s'
                Nothing ->
                  if Data.Text.null fTypes
                    then InvalidArgs "No function type given" s'r'
                  else InvalidArgs ("Unexpected function type, '" ++ unpack fTypes ++ "'") s'r'
          _ -> ReturnFail "Other functions are not implemented yet" -- TODO: Impl
  where
    nullF = Function $ Call "" $ Var ""

isOpBegin :: Text -> Bool
isOpBegin s =
  case unpack s of
    ('(':_:')':_) -> True
    ('(':_:_:')':_) -> True
    _ -> False

-- (OP) EXPR optional EXPR
basicOperatorParser :: Text -> ParserReturnState (Text, Expr, Priority) Text
basicOperatorParser s =
  let s' = strip s
      (s1, s2) = splitWith ")" s'
      op = tail s1
  in
    if isOpBegin s' then
      let uo = UnaryOp
          (s'r, p'exp1) = parseExpression s2
          (s'r', p'exp2) = parseExpression s'r
          bo b = BinaryOp b (Type $ Untyped "") (Type $ Untyped "") in -- postpone constructing binary op

          let op' = case op of
                      "++" -> uo Increment $ Var ""
                      "--" -> uo Decrement $ Var ""
                      "^^" -> uo Square $ Var ""

                      "+" -> bo Add
                      "-" -> bo Sub
                      "*" -> bo Mul
                      "/" -> bo Div
                      "%" -> bo Mod
                      "^" -> bo Pow
                      "=="  -> bo Eq
                      "!="  -> bo NEq

                      _ -> NOp in
          case op' of
            UnaryOp u'o _ ->
              if Data.Text.null s2
                then let ut = Untyped "a" in
                  ReturnSuccess
                  ( ""
                  , Function
                    $ Lambda [ut, ut]
                    $ Operator (UnaryOp u'o (Hole 1)), II)
              else case p'exp1 of
                ParseSuccess exp1 ->
                  ReturnSuccess
                    ( s'r
                    , Operator $ UnaryOp u'o exp1, II)
                ParseFail e -> ReturnFail e
            BinaryOp b'o _ _ ->
              if Data.Text.null s2
                then let ut = Untyped "a" in
                  ReturnSuccess
                  ( ""
                  , Function
                    $ Lambda [ut, ut, ut]
                    $ Operator (BinaryOp b'o (Hole 1) (Hole 2)), II)
              else
                case p'exp1 of
                  ParseSuccess exp1 ->
                    if Data.Text.null s'r then
                      let h = getNextHole exp1
                          t = getNextHoleType exp1
                          mt = getType exp1
                      in case mt of
                        [] ->
                          InvalidArgs
                            ("Failed to find type of " ++ show exp1)
                            s'r
                        mt' ->
                         ReturnSuccess
                          ( ""
                          , Function
                            $ Lambda (mt' ++ [t, t])
                            $ Operator (BinaryOp b'o exp1 h), II)
                    else
                      case p'exp2 of
                        ParseSuccess exp2 ->
                          ReturnSuccess
                            ( s'r'
                            , Operator $ BinaryOp b'o exp1 exp2, II)
                        ParseFail e -> ReturnFail e
                  ParseFail e
                    -> let ut = Untyped "a" in
                      ReturnSuccess
                      ( ""
                      , Function
                        $ Lambda [ut, ut, ut]
                        $ Operator (BinaryOp b'o (Hole 1) (Hole 2)), II)
            NOp ->
              InvalidArgs ("Invalid unary operator, '" ++ unpack op ++ "'") s'r
    else
      ReturnFail "Not an operator expression"

-- Similar like maybe, but has message if failed parsing
data ParseException a = ParseSuccess a | ParseFail String -- deriving Show

instance (Show a) => Show (ParseException a) where
  show (ParseSuccess a) = show a
  show (ParseFail s)    = show s

instance Exception (ParseException Expr)

parseExpression :: Text -> (Text, ParseException Expr)
parseExpression s =
  let pipe :: (Text, Text)
        -> (Text -> ParserReturnState (Expr, Priority) ())
        -> ParserReturnState (Text, Expr, Priority) Text
        = (\(t, t'r) f ->
          case f t of
            ReturnSuccess (e, p) -> ReturnSuccess (t'r, e, p)
            InvalidArgs s _ -> InvalidArgs s t'r
            ReturnFail e -> ReturnFail e
        )
      availableParsers =
        [ parseIdentifier <&> (`pipe` intParser)
        , parseIdentifier <&> (`pipe` fpParser)
        , parseIdentifier <&> (`pipe` (\t ->
          if Data.Text.null t then ReturnFail "Failed to parse identifier"
          else ReturnSuccess (Var t, III)))
        , basicFunctionParser
        , basicOperatorParser
          ] :: [Text -> ParserReturnState (Text, Expr, Priority) Text]
      parseResults = map ($ s) availableParsers
      in
        if Prelude.null parseResults then
          ( ""
          , ParseFail $ "Failed to parse \"" ++ unpack (strip s) ++ "\"")
        else
          let (s, e) :: (String, Maybe Expr)
                = filterResultByPriorities parseResults
            in case e of
              Just expr -> (pack s, ParseSuccess expr)
              Nothing -> ("", ParseFail s)

filterResultByPriorities
  :: [ParserReturnState (Text, Expr, Priority) Text]
  -> (String, Maybe Expr)
filterResultByPriorities [] = ("", Nothing)
filterResultByPriorities [ReturnSuccess (t, e, p)] = (unpack t, Just e)
filterResultByPriorities [InvalidArgs s t] = (unpack t, Nothing)
filterResultByPriorities [ReturnFail s] = (s, Nothing)

filterResultByPriorities (ReturnSuccess (t, e, p):a:ss)
  = case a of
      ReturnSuccess (t1, e1, p1)
        | p1 < p -> filterResultByPriorities $ ReturnSuccess (t, e, p):ss
        | p1 > p -> filterResultByPriorities $ ReturnSuccess (t1, e1, p1):ss
        | otherwise -> ("Ambigous expression", Nothing)
      _ -> filterResultByPriorities $ ReturnSuccess (t, e, p):ss

filterResultByPriorities (InvalidArgs s1 t:ReturnFail s2:ss)
  = filterResultByPriorities $ InvalidArgs s1 t:ss

filterResultByPriorities (InvalidArgs s t:ss)
  = let (t', me) = filterResultByPriorities ss
    in case me of
      Just e -> (t' ++ unpack t, Just e)
      Nothing -> (t' ++ unpack t, Nothing)

filterResultByPriorities (ReturnFail s1:ss) = filterResultByPriorities ss