module PrettyLog where

import System.Process
import System.IO

flush :: IO ()
flush = hFlush stdout

lambda :: String
lambda = "λ"

craftDisplayText :: String -> String
craftDisplayText msg = "AleA." ++ lambda ++ "> " ++ msg