{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import qualified LLVM.Core as LLVM
import qualified LLVM.ExecutionEngine as EE

import Foreign.Ptr (FunPtr)

import qualified Type.Data.Num.Decimal as TypeNum
import qualified Data.Word as W

import qualified Data.NonEmpty.Class as NonEmptyC


type Vector4 = LLVM.Vector TypeNum.D4 Float
type Vector8 = LLVM.Vector TypeNum.D8 Float
type Vector = Vector4

vector :: Vector
vector = LLVM.vector $ NonEmptyC.iterate (1.2+) (-1.7 :: Float)

roundpsExtern4 ::
   LLVM.CodeGenFunction r
      (LLVM.Function (Vector4 -> W.Word32 -> IO Vector4))
roundpsExtern4 =
   LLVM.externFunction "llvm.x86.sse41.round.ps"

roundpsExtern8 ::
   LLVM.CodeGenFunction r
      (LLVM.Function (Vector8 -> W.Word32 -> IO Vector8))
roundpsExtern8 =
   LLVM.externFunction "llvm.x86.avx.round.ps.256"

roundps ::
   LLVM.Value Vector -> LLVM.Value W.Word32 ->
   LLVM.CodeGenFunction s (LLVM.Value Vector)
roundps xs mode = do
   f <- roundpsExtern4
   LLVM.call f xs mode

modul ::
   LLVM.CodeGenModule
      (LLVM.Function (LLVM.Ptr Vector -> LLVM.Ptr Vector -> IO ()))
modul =
   LLVM.createFunction LLVM.ExternalLinkage $ \ptr0 ptr1 -> do
      flip LLVM.store ptr1 =<< flip roundps (LLVM.valueOf 1) =<< LLVM.load ptr0
      LLVM.ret ()

type Importer func = FunPtr func -> func

foreign import ccall safe "dynamic" derefFloorPtr ::
   Importer (LLVM.Ptr Vector -> LLVM.Ptr Vector -> IO ())

run :: IO ()
run = do
   m <- LLVM.newModule
   floorFunc <- do
      func <- LLVM.defineModule m $ LLVM.setTarget LLVM.hostTriple >> modul
      EE.runEngineAccessWithModule m $
         EE.getExecutionFunction derefFloorPtr func
   LLVM.writeBitcodeToFile "floor.bc" m

   print vector
   EE.with vector $ \ptr0 ->
      EE.alloca $ \ptr1 -> do
         floorFunc ptr0 ptr1
         print =<< EE.peek ptr1


main :: IO ()
main = do
   LLVM.initializeNativeTarget
   run
