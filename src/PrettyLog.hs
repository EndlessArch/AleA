module PrettyLog where

import System.Process
import System.IO

flush :: IO ()
flush = hFlush stdout

lambda :: String
lambda = "λ"

craftDisplayText :: String -> String
craftDisplayText msg = "AleA." ++ lambda ++ "> " ++ msg

say :: String -> IO ()
say s = putStr (craftDisplayText s) *> flush

sayLn :: String -> IO ()
sayLn s = say $ s ++ "\n"