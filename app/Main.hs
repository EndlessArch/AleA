{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text hiding
    ( head
    )

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
import Text.Megaparsec

main :: IO ()
main = do
    system "clear"
    sayLn "[ Powered by Macro ]"
    -- sayLn "Cross my heart!"

    initializeNativeTarget

    m <- createModule $ setTarget hostTriple >> getModule

    mainLoop m
    return ()

mainLoop :: Module -> IO Module
mainLoop m = do
    say ""
    eof <- isEOF
    if eof
      then do
        putStrLn "(EOF)\nTerminate."
        return m
    else
      do
        line <- getLine
        case parse exprParser "stdin" line of
          Left err -> sayLn $ "Error: " ++ show err
          Right expr -> sayLn $ show expr
        mainLoop m