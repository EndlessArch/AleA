module Main where

import qualified Test.Marshal as Marshal
import qualified Test.Chop as Chop

import qualified LLVM.Core as LLVM

import Data.Tuple.HT (mapPair, mapFst)

import qualified Test.QuickCheck as QC


main :: IO ()
main = do
   LLVM.initializeNativeTarget

   mapM_ (\(msg,prop) -> putStr (msg++": ") >> prop >>= QC.quickCheck) $
      map (mapPair (("Chop."++),return)) Chop.tests ++
      map (mapPair (("Marshal."++),return)) Marshal.testsRoundTrip ++
      map (mapFst ("Marshal."++)) Marshal.testsExtract ++
      []
