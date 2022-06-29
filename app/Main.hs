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

main :: IO ()
main = do
    system "clear"
    sayLn "[ Powered by Macro ]"
    -- sayLn "Cross my heart!"

    initializeNativeTarget

    m <- createModule $ setTarget hostTriple >> getModule

    mainLoop m

mainLoop :: Module -> IO ()
mainLoop m = do
    say ""
    eof <- isEOF
    if eof then
        putStrLn "(EOF)\nTerminate."
    else
        getLine >>= parseLine >> mainLoop m

parseLine :: String -> IO ()
parseLine "" = return ()
parseLine line =
    let prefix = head line
    in
      case prefix of
        ':' -> handleOptions line
        _ ->
            let (t, ret) = parseExpression $ pack line
            in do
                case ret of
                    ReturnSuccess expr _ -> sayLn $ show expr
                    ReturnFail msg _ -> putStrLn $ "Error!: " ++ msg
                when (null t) $ parseLine (unpack t)