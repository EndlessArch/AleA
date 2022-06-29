{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Test.Marshal (testsRoundTrip, testsExtract) where

import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Util.Optimize as Opt
import qualified LLVM.Util.Proxy as LP
import qualified LLVM.Core as LLVM

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Base.Proxy (Proxy(Proxy))

import Foreign.Ptr (FunPtr, Ptr, nullPtr, plusPtr, castPtr)

import qualified Data.Foldable as Fold
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Tuple.HT (mapPair, mapFst)

import qualified Test.QuickCheck.Monadic as QCMon
import qualified Test.QuickCheck as QC

import Control.Monad (liftM2, void, (<=<))



type RoundTrip a = a -> QC.Property
type RoundTripVec n a = RoundTrip (LLVM.Vector n a)

roundTrip :: (EE.Marshal a, Eq a) => RoundTrip a
roundTrip x =
   QCMon.monadicIO $ do
      y <- QCMon.run $ EE.with x EE.peek
      QCMon.assert $ x==y

testsRoundTrip :: [(String, QC.Property)]
testsRoundTrip =
   map (mapFst ("RoundTrip." ++)) $
   ("f32", QC.property (roundTrip :: RoundTrip Float)) :
   ("f64", QC.property (roundTrip :: RoundTrip Double)) :
   ("i1", QC.property (roundTrip :: RoundTrip Bool)) :
   ("i2", QC.property (roundTrip :: RoundTrip Int2)) :
   ("i3", QC.property (roundTrip :: RoundTrip Int3)) :
   ("i24", QC.property (roundTrip :: RoundTrip Int24)) :
   ("i64", QC.property (roundTrip :: RoundTrip Int64)) :
   ("i2", QC.property (roundTrip :: RoundTrip Word2)) :
   ("i3", QC.property (roundTrip :: RoundTrip Word3)) :
   ("i17", QC.property (roundTrip :: RoundTrip Word17)) :
   ("i32", QC.property (roundTrip :: RoundTrip Word32)) :
   ("ptr", QC.property ((roundTrip :: RoundTrip (Ptr Word8)) . plusPtr nullPtr)) :
   ("()", QC.property (roundTrip :: RoundTrip (LLVM.Struct ()))) :
   ("struct-i8",
      QC.property (roundTrip :: RoundTrip (LLVM.Struct (Word8,())))) :
   ("struct-i8-i24",
      QC.property (roundTrip :: RoundTrip (LLVM.Struct (Word8,(Int24,()))))) :
   ("struct-i3-f32",
      QC.property (roundTrip :: RoundTrip (LLVM.Struct (Int3,(Float,()))))) :
   ("struct-i16-i1-i64",
      QC.property
         (roundTrip :: RoundTrip (LLVM.Struct (Int16,(Bool,(Word64,())))))) :
   ("v8f32", QC.property (roundTrip :: RoundTripVec TypeNum.D8 Float)) :
   ("v5f64", QC.property (roundTrip :: RoundTripVec TypeNum.D5 Double)) :
   ("v7i1", QC.property (roundTrip :: RoundTripVec TypeNum.D7 Bool)) :
   ("v13i1", QC.property (roundTrip :: RoundTripVec TypeNum.D13 Bool)) :
   ("v4i2", QC.property (roundTrip :: RoundTripVec TypeNum.D4 Int2)) :
   ("v10i2", QC.property (roundTrip :: RoundTripVec TypeNum.D10 Word2)) :
   ("v7i3", QC.property (roundTrip :: RoundTripVec TypeNum.D7 Int3)) :
   ("v5i3", QC.property (roundTrip :: RoundTripVec TypeNum.D5 Word3)) :
   ("v9i24", QC.property (roundTrip :: RoundTripVec TypeNum.D9 Int24)) :
   ("v3i17", QC.property (roundTrip :: RoundTripVec TypeNum.D3 Word17)) :
   ("v5i8", QC.property (roundTrip :: RoundTripVec TypeNum.D5 Word8)) :
   ("v3i16", QC.property (roundTrip :: RoundTripVec TypeNum.D3 Word16)) :
   ("v4i8", QC.property (roundTrip :: RoundTripVec TypeNum.D4 Int8)) :
   ("v7i32", QC.property (roundTrip :: RoundTripVec TypeNum.D7 Int32)) :
   []


type Importer func = FunPtr func -> func

generateFunction ::
   EE.ExecutionFunction f =>
   Importer f -> LLVM.CodeGenModule (LLVM.Function f) -> IO f
generateFunction imprt code = do
   td <- EE.getTargetData
   (m,func) <-
      LLVM.createModule $ do
         LLVM.setTarget LLVM.hostTriple
         LLVM.setDataLayout $ EE.dataLayoutStr td
         liftM2 (,) LLVM.getModule code
   LLVM.writeBitcodeToFile "Test.bc" m
   void $ Opt.optimizeModule 3 m
   LLVM.writeBitcodeToFile "TestOpt.bc" m
   EE.runEngineAccessWithModule m $ EE.getExecutionFunction imprt func


foreign import ccall safe "dynamic" derefTestCasePtr ::
   Importer (LLVM.Ptr inp -> LLVM.Ptr out -> IO ())

modul ::
   (LLVM.IsType inp, LLVM.IsType out) =>
   (LLVM.Value inp -> LLVM.CodeGenFunction () (LLVM.Value out)) ->
   LLVM.CodeGenModule (LLVM.Function (LLVM.Ptr inp -> LLVM.Ptr out -> IO ()))
modul codegen =
   LLVM.createFunction LLVM.ExternalLinkage $ \xPtr yPtr -> do
      flip LLVM.store yPtr =<< codegen =<< LLVM.load xPtr
      LLVM.ret ()

run ::
   (Show inp, EE.Marshal inp, EE.Marshal out) =>
   QC.Gen inp ->
   (LLVM.Value inp -> LLVM.CodeGenFunction () (LLVM.Value out)) ->
   (inp -> out -> Bool) ->
   IO QC.Property
run qcgen codegen predicate = do
   funIO <- generateFunction derefTestCasePtr $ modul codegen
   return $ QC.forAll qcgen $ \x ->
      QCMon.monadicIO $ do
         y <-
            QCMon.run $
               EE.with x $ \xPtr ->
               EE.alloca $ \yPtr -> do
                  funIO xPtr yPtr
                  EE.peek yPtr
         QCMon.assert $ predicate x y


type Extract n a = QC.Gen (LLVM.Vector n a, Word32)

extractElem ::
   (TypeNum.Positive n,
    (n TypeNum.:*: LLVM.SizeOf a) ~ size, TypeNum.Natural size,
    Show a, Eq a,
    EE.MarshalVector a, EE.Marshal a, LLVM.IsSized a, LLVM.IsPrimitive a) =>
   Extract n a -> IO QC.Property
extractElem qcgen =
   run
      (fmap (uncurry LLVM.consStruct) qcgen)
      (\vi -> do
         v <- LLVM.extractvalue vi TypeNum.d0
         i <- LLVM.extractvalue vi TypeNum.d1
         LLVM.extractelement v i)
      (LLVM.uncurryStruct $ \v i a ->
         a == Fold.toList v !! fromIntegral i)


vectorSize :: LLVM.Vector n a -> Proxy n
vectorSize _ = Proxy

genVector :: (TypeNum.Positive n, QC.Arbitrary a) => Extract n a
genVector = do
   v <- QC.arbitrary
   i <- QC.choose (0, TypeNum.integralFromProxy (vectorSize v) - 1)
   return (v,i)


type Int2 = LLVM.IntN TypeNum.D2
type Int3 = LLVM.IntN TypeNum.D3
type Word2 = LLVM.WordN TypeNum.D2
type Word3 = LLVM.WordN TypeNum.D3
type Int24 = LLVM.IntN TypeNum.D24
type Word17 = LLVM.IntN TypeNum.D17


testsVector :: [(String, IO QC.Property)]
testsVector =
   map (mapFst ("Vector." ++)) $
   ("v8f32", extractElem (genVector :: Extract TypeNum.D8 Float)) :
   ("v5f64", extractElem (genVector :: Extract TypeNum.D5 Double)) :
   ("v7i1", extractElem (genVector :: Extract TypeNum.D7 Bool)) :
   ("v13i1", extractElem (genVector :: Extract TypeNum.D13 Bool)) :
   ("v4i2", extractElem (genVector :: Extract TypeNum.D4 Int2)) :
   ("v10i2", extractElem (genVector :: Extract TypeNum.D10 Word2)) :
   -- ToDo: broken on LLVM<=9: https://bugs.llvm.org/show_bug.cgi?id=44915
   ("v7i3", extractElem (genVector :: Extract TypeNum.D7 Int3)) :
   ("v5i3", extractElem (genVector :: Extract TypeNum.D5 Word3)) :
   ("v9i24", extractElem (genVector :: Extract TypeNum.D9 Int24)) :
   ("v3i17", extractElem (genVector :: Extract TypeNum.D3 Word17)) :
   ("v5i8", extractElem (genVector :: Extract TypeNum.D5 Word8)) :
   ("v3i16", extractElem (genVector :: Extract TypeNum.D3 Word16)) :
   ("v4i8", extractElem (genVector :: Extract TypeNum.D4 Int8)) :
   ("v7i32", extractElem (genVector :: Extract TypeNum.D7 Int32)) :
   []


{-
Conversion from a Ptr Word8 triggers improper optimization
if target data layout is not set for module prior to optimization.
-}
runViaBytePtr ::
   (Show inp, EE.Marshal inp, EE.Marshal out) =>
   QC.Gen inp ->
   (LLVM.Value inp -> LLVM.CodeGenFunction () (LLVM.Value out)) ->
   (inp -> out -> Bool) ->
   IO QC.Property
runViaBytePtr qcgen codegen predicate = do
   funIO <-
      generateFunction derefTestCasePtr $
         LLVM.createFunction LLVM.ExternalLinkage $ \xPtr yPtr -> do
            flip LLVM.store yPtr =<< codegen =<< LLVM.load =<< LLVM.bitcast xPtr
            LLVM.ret ()
   return $ QC.forAll qcgen $ \x ->
      QCMon.monadicIO $ do
         y <-
            QCMon.run $
               EE.with x $ \xPtr ->
               EE.alloca $ \yPtr -> do
                  funIO (castToBytePtr xPtr) yPtr
                  EE.peek yPtr
         QCMon.assert $ predicate x y

castToBytePtr :: LLVM.Ptr a -> LLVM.Ptr Word8
castToBytePtr = LLVM.fromPtr . castPtr . LLVM.uncheckedToPtr

extractValue ::
   (QC.Arbitrary s, Show s, EE.Marshal s, EE.Marshal a, Eq a) =>
   LP.Proxy s ->
   (s -> a) ->
   (forall r. LLVM.Value s -> LLVM.CodeGenFunction r (LLVM.Value a)) ->
   Bool ->
   IO QC.Property
extractValue LP.Proxy select extract viaBytePtr =
   (if viaBytePtr then runViaBytePtr else run)
      QC.arbitrary extract (\s x -> select s == x)

type Pair a b = LLVM.Struct (a,(b,()))
type Triple a b c = LLVM.Struct (a,(b,(c,())))

sfst :: LLVM.Struct (a,z) -> a
sfst (LLVM.Struct (a,_)) = a
ssnd :: LLVM.Struct (a,(b,z)) -> b
ssnd (LLVM.Struct (_,(b,_))) = b
sthd :: LLVM.Struct (a,(b,(c,z))) -> c
sthd (LLVM.Struct (_,(_,(c,_)))) = c

exv ::
   (LLVM.GetField s i, TypeNum.Natural i, LLVM.FieldType s i ~ a) =>
   Proxy i ->
   LLVM.Value (LLVM.Struct s) -> LLVM.CodeGenFunction r (LLVM.Value a)
exv = flip LLVM.extractvalue

proxyA :: LP.Proxy (Triple Int16 Bool Word64)
proxyA = LP.Proxy

proxyB :: LP.Proxy (Triple Bool Bool Int8)
proxyB = LP.Proxy

proxyC :: LP.Proxy (Pair Bool (Pair Float Word64))
proxyC = LP.Proxy

testsStruct :: [(String, Bool -> IO QC.Property)]
testsStruct =
   ("{i16,i1,i64} 0",
      extractValue proxyA sfst (exv TypeNum.d0)) :
   ("{i16,i1,i64} 1",
      extractValue proxyA ssnd (exv TypeNum.d1)) :
   ("{i16,i1,i64} 2",
      extractValue proxyA sthd (exv TypeNum.d2)) :
   ("{i1,i1,i8} 0",
      extractValue proxyB sfst (exv TypeNum.d0)) :
   ("{i1,i1,i8} 1",
      extractValue proxyB ssnd (exv TypeNum.d1)) :
   ("{i1,i1,i8} 2",
      extractValue proxyB sthd (exv TypeNum.d2)) :
   ("{i1,{float,i64}} 0",
      extractValue proxyC sfst (exv TypeNum.d0)) :
   ("{i1,{float,i64}} 1 0",
      extractValue proxyC (sfst.ssnd) (exv TypeNum.d0 <=< exv TypeNum.d1)) :
   ("{i1,{float,i64}} 1 1",
      extractValue proxyC (ssnd.ssnd) (exv TypeNum.d1 <=< exv TypeNum.d1)) :
   []


testsExtract :: [(String, IO QC.Property)]
testsExtract =
   map (mapFst ("Extract." ++)) $
      testsVector ++
      map (mapPair (("Struct." ++), ($False))) testsStruct ++
      map (mapPair (("StructByte." ++), ($True))) testsStruct
