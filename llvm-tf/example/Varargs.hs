module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Core

import Data.Word (Word8, Word32)


firstChar ::
    (Natural n) =>
    Value (Ptr (Array n Word8)) -> CodeGenFunction r (Value (Ptr Word8))
firstChar str = getElementPtr0 str (0::Word32, ())

bldVarargs :: CodeGenModule (Function (Word32 -> IO ()))
bldVarargs =
   withStringNul "Hello\n" (\fmt1 ->
   withStringNul "A number %d\n" (\fmt2 ->
   withStringNul "Two numbers %d %d\n" (\fmt3 -> do
      printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Word32)
      func <- createFunction ExternalLinkage $ \ x -> do

        tmp1 <- firstChar fmt1
        _ <- call (castVarArgs printf) tmp1

        tmp2 <- firstChar fmt2
        _ <- call (castVarArgs printf) tmp2 x

        tmp3 <- firstChar fmt3
        _ <- call (castVarArgs printf) tmp3 x x

        ret ()
      return func
   )))

main :: IO ()
main = do
    initializeNativeTarget
    varargs <- EE.simpleFunction bldVarargs
    varargs 42
