module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Core

import Data.Word (Word8, Word32)


bldGreet :: CodeGenModule (Function (IO ()))
bldGreet = withStringNul "Hello, JIT!" (\greetz -> do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    func <- createFunction ExternalLinkage $ do
      _ <- call puts =<< getElementPtr0 greetz (0::Word32, ())
      ret ()
    return func)

main :: IO ()
main = do
    initializeNativeTarget
    greet <- EE.simpleFunction bldGreet
    greet
    greet
    greet
