module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Util.Proxy (Proxy(Proxy))
import LLVM.Core (Vector, unsafeTypeRef, initializeNativeTarget)

import Type.Data.Num.Decimal.Literal (D1, D4)

import Data.Word (Word32, Word64)


main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    td <- EE.getTargetData
    print (
        EE.littleEndian td,
        EE.dataLayoutStr td,
        EE.abiAlignmentOfType td $ unsafeTypeRef (Proxy :: Proxy Word32),
        EE.abiAlignmentOfType td $ unsafeTypeRef (Proxy :: Proxy Word64),
        EE.abiAlignmentOfType td $ unsafeTypeRef (Proxy :: Proxy (Vector D4 Float)),
        EE.abiAlignmentOfType td $ unsafeTypeRef (Proxy :: Proxy (Vector D1 Double)),
        EE.storeSizeOfType td $ unsafeTypeRef (Proxy :: Proxy (Vector D4 Float)),
        EE.intPtrType td
        )
