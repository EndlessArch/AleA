module Main (main) where

import LLVM.Core
import LLVM.FFI.Core (CallingConvention(GHC))

import Data.Word (Word32)


-- Our module will have these two functions.
data Mod = Mod {
    f1 :: Function (Word32 -> IO Word32),
    f2 :: Function (Word32 -> Word32 -> IO Word32)
    }

main :: IO ()
main = do
    m <- createModule $ setTarget hostTriple >> buildMod >> getModule
    --_ <- optimizeModule 3 m
    writeBitcodeToFile "CallConv.bc" m

buildMod :: CodeGenModule Mod
buildMod = do
    fun2 <- createNamedFunction InternalLinkage "plus" $ \ x y ->
      ret =<< add x y
    setFuncCallConv fun2 GHC
    fun1 <- newNamedFunction ExternalLinkage "test"
    defineFunction fun1 $ \ arg ->
      ret =<< callWithConv GHC fun2 arg (valueOf 1)
    return $ Mod {f1 = fun1, f2 = fun2}
