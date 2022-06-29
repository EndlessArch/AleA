module LLVM.Util.File (writeCodeGenModule) where

import qualified LLVM.Core as LLVM


writeCodeGenModule :: FilePath -> LLVM.CodeGenModule a -> IO ()
writeCodeGenModule path f = do
    m <- LLVM.newModule
    _ <- LLVM.defineModule m f
    LLVM.writeBitcodeToFile path m
