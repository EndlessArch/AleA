module Main where

import Data.Text hiding
    ( head
    )

import LLVM.Core
import LLVM.ExecutionEngine

import Prelude hiding
    ( null
    )

import System.Process
import System.IO

import Options
import Parser
import PrettyLog

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
      getLine >>= parse exprParser "stdin"
        >>= \eith -> do
          e <- eith
          return . sayLn $ show e
            >>= join . mainLoop m
