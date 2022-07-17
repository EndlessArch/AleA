{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import LLVM.Core
import LLVM.ExecutionEngine

import System.Process
import System.IO

import Options
import Parser
import PrettyLog
import Types

import Control.Monad
import Data.Functor
import qualified Language.Haskell.Liquid.GHC.API as GHC
import System.Console.Haskeline
  (runInputT, Settings (Settings, historyFile, autoAddHistory, complete), InputT, noCompletion, getInputLine, outputStrLn)
import System.Environment
import System.Exit (exitSuccess, exitFailure)
import Text.Megaparsec

getName = "AleA" :: String
getVersion = "1.0.0" :: String

main :: IO ()
main = do
    -- system "clear"
    -- putStrLn $ craftDisplayText "[ Powered by Macro ]"

    let argParser :: [String] -> ([String], Either (String, String) String) = \case
          "-h":s -> (s, ) $ Right
            $  "Usage: alea   -h              -- print this.\n"
            ++ "              -v              -- print version.\n"
            ++ "              -s  [source]    -- set source file.\n"
            ++ "              -o  [name]      -- set output file name."
          "-v":s -> (s, ) . Right $ getName ++ " v" ++ getVersion
          "-s":src:s -> (s, Left (src, ""))
          "-o":dst:s -> (s, Left ("", dst))
          _ -> argParser ["-h"]

    src'dst <- do
      args <- getArgs

      let
          f (a, Right b) = [Right b]
          f (a, b) = (b:) . f $ argParser a

          -- g :: {- NonEmpty -} [(String, String)] -> (String, String)
          {-@ type NEStringTuple = { v : [(String, String)] | 0 < size v} @-}
          {-@ g :: NEStringTuple -> (String, String) @-}
          g ((a, ""):("", d):x) = g $ (a, d):x
          g ((a, ""):(c, d):x) = g $ (c, d):x
          g (("", b):(c, ""):x) = g $ (c, b):x
          g (("", b):(c, d):x) = g $ (c, d):x
          g [(a, b)] = (a, b)

          l =
            case f $ argParser args of
              [a] -> [a]
              x -> Prelude.init x

      ss <- sequence . (`Prelude.map` l) $
              \case
                Right s -> putStrLn s >> exitSuccess $> ("", "")
                Left (src, "") -> return (src, "")
                Left ("", dst) -> return ("", dst)
                Left _ -> exitFailure $> ("", "")

      return $ g ss

    initializeNativeTarget

    m <- createModule $ setTarget hostTriple >> getModule

    case src'dst of
      ("", "") -> -- interpreter mode
        do
          runInputT Settings {
            complete = noCompletion,
            historyFile = Nothing,
            autoAddHistory = True
          } $ mainLoop m $> ()
      ("", _) -> putStrLn $ getName ++ ": No input file specified."
      (src, dst) -> do
        h <- openFile src ReadMode
        c <- hGetContents h
        case runParser (many defunParser) src c of
          Left e ->
            putStrLn $ getName ++ ": Compile error;\n" ++ errorBundlePretty e
          Right e -> putStrLn $ getName ++ ": " ++ show e

          -- Right es ->
          --   mapM (\e -> putStrLn $ getName ++ ": " ++ show e) es

mainLoop :: Module -> InputT IO Module
mainLoop m = do
    line <- getInputLine $ craftDisplayText ""
    case line of
      Just line' -> do
        case parse (defunParser <|> exprParser) "stdin" line' of
          Left err -> outputStrLn $ "Error: " ++ errorBundlePretty err
          Right expr -> outputStrLn . craftDisplayText $ show expr
        mainLoop m
      Nothing -> outputStrLn "Terminate." $> m