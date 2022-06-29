{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
module LLVM.ExecutionEngine.Target (
    TargetData,
    dataLayoutStr,
    abiAlignmentOfType,
    abiSizeOfType,
    littleEndian,
    callFrameAlignmentOfType,
    intPtrType,
    offsetOfElement,
    pointerSize,
    preferredAlignmentOfType,
    sizeOfTypeInBits,
    storeSizeOfType,
    getTargetData,
    targetDataFromString,
    withIntPtrType,
    ) where

import qualified LLVM.ExecutionEngine.Engine as EE
import LLVM.Core.Data (WordN)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Base.Proxy (Proxy)

import Foreign.ForeignPtr
         (ForeignPtr,
          newForeignPtr, withForeignPtr, touchForeignPtr, castForeignPtr)
import Foreign.C.String (withCString, peekCString)

import Control.Monad (liftM2, (<=<))
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)


type Type = FFI.TypeRef

data TargetDataOwner

data TargetData = TargetData (ForeignPtr TargetDataOwner) FFI.TargetDataRef

dataLayoutStr :: TargetData -> String
dataLayoutStr td = unsafeIO td $ peekCString <=< FFI.copyStringRepOfTargetData

abiAlignmentOfType :: TargetData -> Type -> Int
abiAlignmentOfType td = unsafeIntIO td . flip FFI.abiAlignmentOfType

abiSizeOfType :: TargetData -> Type -> Int
abiSizeOfType td = unsafeIntIO td . flip FFI.abiSizeOfType

littleEndian :: TargetData -> Bool
littleEndian td = FFI.bigEndian /= unsafeIO td FFI.byteOrder

callFrameAlignmentOfType :: TargetData -> Type -> Int
callFrameAlignmentOfType td = unsafeIntIO td . flip FFI.callFrameAlignmentOfType

-- elementAtOffset :: TargetData -> Type -> Word64 -> Int

intPtrType :: TargetData -> Type
intPtrType td = unsafeIO td FFI.intPtrType

offsetOfElement :: TargetData -> Type -> Int -> Int
offsetOfElement td ty k =
    unsafeIntIO td $ \r -> FFI.offsetOfElement r ty (fromIntegral k)

pointerSize :: TargetData -> Int
pointerSize td = unsafeIntIO td FFI.pointerSize

-- preferredAlignmentOfGlobal :: TargetData -> Value a -> Int

preferredAlignmentOfType :: TargetData -> Type -> Int
preferredAlignmentOfType td = unsafeIntIO td . flip FFI.preferredAlignmentOfType

sizeOfTypeInBits :: TargetData -> Type -> Int
sizeOfTypeInBits td = unsafeIntIO td . flip FFI.sizeOfTypeInBits

storeSizeOfType :: TargetData -> Type -> Int
storeSizeOfType td = unsafeIntIO td . flip FFI.storeSizeOfType


withIntPtrType :: (forall n . (Dec.Positive n) => WordN n -> a) -> a
withIntPtrType f =
    fromMaybe (error "withIntPtrType: pointer size must be non-negative") $
        Dec.reifyPositive (fromIntegral sz) (\ n -> f (g n))
  where g :: Proxy n -> WordN n
        g _ = error "withIntPtrType: argument used"
        sz = pointerSize $ unsafePerformIO getTargetData


unsafeIO :: TargetData -> (FFI.TargetDataRef -> IO a) -> a
unsafeIO (TargetData fptr td) act =
    unsafePerformIO $ do x <- act td; touchForeignPtr fptr; return x

unsafeIntIO ::
   (Integral i, Num j) => TargetData -> (FFI.TargetDataRef -> IO i) -> j
unsafeIntIO td act = fromIntegral $ unsafeIO td act

-- Normally the TargetDataRef never changes,
-- so the operation are really functions.
-- The ForeignPtr can point to TargetData or to ExecutionEngine.
makeTargetData :: ForeignPtr a -> FFI.TargetDataRef -> TargetData
makeTargetData = TargetData . castForeignPtr

-- Gets the target data for the JIT target.
getTargetData :: IO TargetData
getTargetData =
    EE.runEngineAccess $
    liftM2 makeTargetData
        (EE.fromEngine <$> EE.getEngine)
        EE.getExecutionEngineTargetData

createTargetData :: String -> IO (ForeignPtr FFI.TargetData)
createTargetData s =
    newForeignPtr FFI.ptrDisposeTargetData =<<
    withCString s FFI.createTargetData

targetDataFromString :: String -> TargetData
targetDataFromString s = unsafePerformIO $ do
    td <- createTargetData s
    withForeignPtr td $ return . makeTargetData td
