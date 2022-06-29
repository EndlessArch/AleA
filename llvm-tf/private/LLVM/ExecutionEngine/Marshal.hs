module LLVM.ExecutionEngine.Marshal (
    Marshal(..),
    MarshalVector(..),
    sizeOf,
    alignment,
    StructFields,
    sizeOfArray,
    pokeList,

    with,
    alloca,

    Stored(..),
    castToStoredPtr,
    castFromStoredPtr,

    -- * for testing
    expandBits,
    gatherBits,
    adjustSign,
    chop,
    cut,
    split,
    merge,
    ) where

import qualified LLVM.Core.Vector as Vector ()
import qualified LLVM.Core.Data as Data
import qualified LLVM.Core.Type as Type
import qualified LLVM.Core.Proxy as LP
import qualified LLVM.ExecutionEngine.Target as Target
import LLVM.ExecutionEngine.Target (TargetData)
import LLVM.Core.Data (Ptr)

import qualified LLVM.Target.Native as Native
import qualified LLVM.FFI.Core as FFI

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Base.Proxy (Proxy(Proxy))

import qualified Foreign.Storable as Store
import qualified Foreign
import Foreign.StablePtr (StablePtr)
import Foreign.Ptr (FunPtr)

import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Monad.Trans.State as MS
import Control.Applicative (liftA2, pure, (<$>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import Data.Bits (shiftL, shiftR, testBit, (.&.))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)



targetData :: TargetData
targetData =
    unsafePerformIO $ Native.initializeNativeTarget >> Target.getTargetData


sizeOf :: (Type.IsType a) => LP.Proxy a -> Int
sizeOf = Target.storeSizeOfType targetData . Type.unsafeTypeRef

alignment :: (Type.IsType a) => LP.Proxy a -> Int
alignment = Target.abiAlignmentOfType targetData . Type.unsafeTypeRef

sizeOfArray :: (Type.IsType a) => LP.Proxy a -> Int -> Int
sizeOfArray proxy n =
   Target.abiSizeOfType targetData (Type.unsafeTypeRef proxy) * n


{- |
Exchange data via memory in a format that is compatible with LLVM's data layout.
Prominent differences to 'Foreign.Storable' are:

* LLVM's @i1@ requires a byte in memory,
  whereas Haskell's 'Bool' occupies a 32-bit word with 'Foreign.poke'.

* LLVM's @<4 x i8>@ orders vector elements depending on machine endianess,
  whereas 'Foreign.poke' uses ascending order
  which is compatible with arrays.

This class also supports 'Data.Struct', 'Data.Vector', 'Data.Array'.
-}
class (Type.IsType a) => Marshal a where
    peek :: Ptr a -> IO a
    poke :: Ptr a -> a -> IO ()

peekPrimitive :: (Type.Storable a) => Ptr a -> IO a
peekPrimitive = Store.peek . Type.toPtr

pokePrimitive :: (Type.Storable a) => Ptr a -> a -> IO ()
pokePrimitive = Store.poke . Type.toPtr

instance Marshal Float  where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Double where
    peek = peekPrimitive; poke = pokePrimitive

instance Marshal Int   where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Int8  where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Int16 where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Int32 where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Int64 where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Word   where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Word8  where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Word16 where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Word32 where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal Word64 where
    peek = peekPrimitive; poke = pokePrimitive
instance (Store.Storable a) => Marshal (Foreign.Ptr a) where
    peek = peekPrimitive; poke = pokePrimitive
instance (Type.IsType a) => Marshal (Ptr a) where
    peek = peekPrimitive; poke = pokePrimitive
instance (Type.IsFunction a) => Marshal (FunPtr a) where
    peek = peekPrimitive; poke = pokePrimitive
instance Marshal (StablePtr a) where
    peek = peekPrimitive; poke = pokePrimitive

instance (Type.Positive d) => Marshal (Data.WordN d) where
    peek ptr =
        fmap (Data.WordN . merge 8 . map toInteger . word8s) $
        peekVectorGen ptr $ sizeOf (proxyFromPtr ptr)
    poke ptr (Data.WordN a) =
        pokeVectorGen ptr . word8s . map fromInteger .
        take (sizeOf (proxyFromPtr ptr)) . split 8 $ a

instance (Type.Positive d) => Marshal (Data.IntN d) where
    peek ptr =
        fmap (consIntN Proxy . merge 8 . map toInteger . word8s) $
        peekVectorGen ptr $ sizeOf (proxyFromPtr ptr)
    poke ptr a =
        pokeVectorGen ptr . word8s . map fromInteger .
        take (sizeOf (proxyFromPtr ptr)) . split 8 $ deconsIntN Proxy a

cut :: Int -> Integer -> Integer
cut n w = (shiftL 1 n - 1) .&. w

split :: Int -> Integer -> [Integer]
split n = map (cut n) . iterate (flip shiftR n)

merge :: Int -> [Integer] -> Integer
merge m xs = sum $ zipWith shiftL xs $ iterate (m+) 0

instance Marshal Bool where
    peek = fmap (/= 0) . Store.peek . castBoolPtr
    poke ptr a = Store.poke (castBoolPtr ptr) (fromIntegral $ fromEnum a)

castBoolPtr :: Ptr Bool -> Foreign.Ptr Word8
castBoolPtr = Foreign.castPtr . Data.uncheckedToPtr

instance
    (Type.Natural n, Marshal a, Type.IsSized a) =>
        Marshal (Data.Array n a) where
    peek = peekArray Proxy LP.Proxy
    poke = pokeArray Fold.toList

peekArray ::
    (Type.Natural n, Marshal a) =>
    Proxy n -> LP.Proxy a ->
    Ptr (Data.Array n a) -> IO (Data.Array n a)
peekArray n proxy =
    let step = Target.abiSizeOfType targetData $ Type.unsafeTypeRef proxy
    in \ptr ->
        fmap Data.Array $ mapM peek $
        take (Dec.integralFromProxy n) $
        iterate (flip plusPtr step) (castElemPtr ptr)

pokeArray :: (Marshal a) => (f a -> [a]) -> Ptr (f a) -> f a -> IO ()
pokeArray toList ptr = pokeList (castElemPtr ptr) . toList

pokeList :: (Marshal a) => Ptr a -> [a] -> IO ()
pokeList = pokeListAux LP.Proxy

pokeListAux :: (Marshal a) => LP.Proxy a -> Ptr a -> [a] -> IO ()
pokeListAux proxy =
    let step = Target.abiSizeOfType targetData $ Type.unsafeTypeRef proxy
    in \ptr -> sequence_ . zipWith poke (iterate (flip plusPtr step) ptr)

castElemPtr :: Ptr (f a) -> Ptr a
castElemPtr = Data.uncheckedFromPtr . Foreign.castPtr . Data.uncheckedToPtr


instance
    (Type.Positive n, MarshalVector a) =>
        Marshal (Data.Vector n a) where
    peek = peekVector
    poke = pokeVector

class (Type.IsPrimitive a) => MarshalVector a where
    peekVector ::
        (Type.Positive n) =>
        Ptr (Data.Vector n a) -> IO (Data.Vector n a)
    pokeVector ::
        (Type.Positive n) =>
        Ptr (Data.Vector n a) -> Data.Vector n a -> IO ()

instance MarshalVector Bool where
    peekVector ptr =
        fmap (vectorFromList . expandBits) $
        peekVectorGen ptr $ sizeOf (proxyFromPtr ptr)
    pokeVector ptr = pokeVectorGen ptr . gatherBits . Fold.toList

expandBits :: [Word8] -> [Bool]
expandBits = concatMap (\byte -> map (testBit byte) [0..7])

vectorFromList :: (Type.Positive n) => [a] -> Data.Vector n a
vectorFromList =
    MS.evalState $ Trav.sequence $ pure $ MS.state $ \(y:ys) -> (y,ys)

gatherBits :: [Bool] -> [Word8]
gatherBits =
    map (sum . zipWith (flip shiftL) [0..] . map (fromIntegral . fromEnum)) .
    ListHT.sliceVertical 8


instance (Type.Positive d) => MarshalVector (Data.WordN d) where
    peekVector ptr = fmap Data.WordN <$> peekNVector Proxy ptr
    pokeVector ptr = pokeNVector Proxy ptr . fmap (\(Data.WordN x) -> x)

instance (Type.Positive d) => MarshalVector (Data.IntN d) where
    peekVector ptr = fmap (consIntN Proxy) <$> peekNVector Proxy ptr
    pokeVector ptr = pokeNVector Proxy ptr . fmap (deconsIntN Proxy)

consIntN :: (Type.Positive d) => Proxy d -> Integer -> Data.IntN d
consIntN proxy = Data.IntN . adjustSign (Dec.integralFromProxy proxy)

deconsIntN :: (Type.Positive d) => Proxy d -> Data.IntN d -> Integer
deconsIntN proxy (Data.IntN a) = cut (Dec.integralFromProxy proxy) a

adjustSign :: Int -> Integer -> Integer
adjustSign d =
    let range = shiftL 1 d
    in  \a -> if a < div range 2 then a else a-range

peekNVector ::
    (Type.Positive n, Type.Positive d, Type.IsPrimitive (intn d)) =>
    Proxy d -> Ptr (Data.Vector n (intn d)) -> IO (Data.Vector n Integer)
peekNVector proxy ptr =
    fmap (vectorFromList . chop 8 (Dec.integralFromProxy proxy) .
          map toInteger . word8s) $
    peekVectorGen ptr $ sizeOf (proxyFromPtr ptr)

pokeNVector ::
    (Type.Positive n, Type.Positive d, Type.IsPrimitive (intn d)) =>
    Proxy d ->
    Ptr (Data.Vector n (intn d)) -> Data.Vector n Integer -> IO ()
pokeNVector proxy ptr =
    pokeVectorGen ptr . take (sizeOf (proxyFromPtr ptr)) . word8s .
    map fromInteger . chop (Dec.integralFromProxy proxy) 8 . Fold.toList

word8s :: [Word8] -> [Word8]
word8s = id

proxyFromPtr :: Ptr a -> LP.Proxy a
proxyFromPtr _ = LP.Proxy

chop :: Int -> Int -> [Integer] -> [Integer]
chop m n =
    concat . snd .
    Trav.mapAccumL
        (\(valid,acc) x ->
            let newAcc = acc + cut n (shiftL x valid)
                nextValid = valid+m
            in  if nextValid<n
                    then ((nextValid, newAcc), [])
                    else
                        case divMod nextValid n of
                            (chunks,remd) ->
                                ((remd, shiftR x (m-remd)),
                                 (newAcc :) $
                                 map (cut n . shiftR x) $
                                 take (chunks-1) $ iterate (n+) (n-valid)))
        (0,0) .
    (++ repeat 0)


instance MarshalVector Float where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Double where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Word where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Word8 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Word16 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Word32 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Word64 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Int where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Int8 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Int16 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Int32 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList

instance MarshalVector Int64 where
    peekVector = peekVectorAuto Proxy
    pokeVector ptr = pokeVectorGen ptr . Fold.toList


peekVectorAuto ::
    (Type.Positive n, Type.IsPrimitive a, Store.Storable a) =>
    Proxy n -> Ptr (Data.Vector n a) -> IO (Data.Vector n a)
peekVectorAuto proxy ptr =
    fmap vectorFromList $ peekVectorGen ptr $ Dec.integralFromProxy proxy

peekVectorGen ::
    (Type.IsType b, Store.Storable chunk) =>
    Ptr b -> Int -> IO [chunk]
peekVectorGen = peekVectorAux LP.Proxy (error "vector")

peekVectorAux ::
    (Type.IsType b, Store.Storable chunk) =>
    LP.Proxy b -> chunk -> Ptr b -> Int -> IO [chunk]
peekVectorAux proxy dummyChunk =
    let (offset,step) = arrayParams proxy dummyChunk
    in  \ptr n ->
            mapM (Store.peekByteOff (Data.uncheckedToPtr ptr)) $
            take n $ iterate (step+) offset

pokeVectorGen ::
    (Type.IsType b, Store.Storable chunk) =>
    Ptr b -> [chunk] -> IO ()
pokeVectorGen = pokeVectorAux LP.Proxy (error "vector")

pokeVectorAux ::
    (Type.IsType b, Store.Storable chunk) =>
    LP.Proxy b -> chunk -> Ptr b -> [chunk] -> IO ()
pokeVectorAux proxy dummyChunk =
    let (offset,step) = arrayParams proxy dummyChunk
    in  \ptr xs ->
            sequence_ $
            zipWith (Store.pokeByteOff (Data.uncheckedToPtr ptr))
                (iterate (step+) offset) xs

arrayParams ::
    (Type.IsType b, Store.Storable chunk) =>
    LP.Proxy b -> chunk -> (Int,Int)
arrayParams proxy dummyChunk =
    let chunkSize = Store.sizeOf dummyChunk
    in  if Target.littleEndian targetData
            then (0, chunkSize)
            else (sizeOf proxy - chunkSize, -chunkSize)


instance (StructFields fields) => Marshal (Data.Struct fields) where
    peek = withPtrProxy $ \proxy ->
        let typeRef = Type.unsafeTypeRef proxy
        in fmap Data.Struct . peekStruct typeRef 0
    poke = withPtrProxy $ \proxy ->
        let typeRef = Type.unsafeTypeRef proxy
            pokePlain = pokeStruct typeRef 0
        in \ptr (Data.Struct as) -> pokePlain ptr as

withPtrProxy :: (LP.Proxy a -> Ptr a -> b) -> Ptr a -> b
withPtrProxy act = act LP.Proxy

class (Type.StructFields fields) => StructFields fields where
    peekStruct :: FFI.TypeRef -> Int -> Ptr struct -> IO fields
    pokeStruct :: FFI.TypeRef -> Int -> Ptr struct -> fields -> IO ()

instance
    (Marshal a, Type.IsSized a, StructFields as) =>
        StructFields (a,as) where
    peekStruct typeRef i =
        let offset = Target.offsetOfElement targetData typeRef i
            peekIs = peekStruct typeRef (i+1)
        in \ptr -> liftA2 (,) (peek $ plusPtr ptr offset) (peekIs ptr)
    pokeStruct typeRef i =
        let offset = Target.offsetOfElement targetData typeRef i
            pokeIs = pokeStruct typeRef (i+1)
        in \ptr (a,as) -> poke (plusPtr ptr offset) a >> pokeIs ptr as

instance StructFields () where
    peekStruct _type _i _ptr = return ()
    pokeStruct _type _i _ptr () = return ()

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr ptr offset =
    Data.uncheckedFromPtr $ Foreign.plusPtr (Data.uncheckedToPtr ptr) offset


with :: (Marshal a) => a -> (Ptr a -> IO b) -> IO b
with a act = alloca $ \ptr -> poke ptr a >> act ptr

alloca :: (Type.IsType a) => (Ptr a -> IO b) -> IO b
alloca = allocaAux LP.Proxy

allocaAux :: (Type.IsType a) => LP.Proxy a -> (Ptr a -> IO b) -> IO b
allocaAux proxy f =
    Foreign.allocaBytesAligned (sizeOf proxy) (alignment proxy)
        (f . Data.uncheckedFromPtr)


{- |
Provide @Marshal@ functionality through Haskell's 'Storable' interface.
Thus, @'Ptr' a@ is equivalent to @'Foreign.Ptr' ('Stored' a)@.
You may e.g. use a @'Foreign.ForeignPtr' ('Stored' a)@
to manage LLVM data with Haskell's garbage collector.
-}
newtype Stored a = Stored {getStored :: a}

castToStoredPtr :: Ptr a -> Foreign.Ptr (Stored a)
castToStoredPtr = Foreign.castPtr . Data.uncheckedToPtr

castFromStoredPtr :: Foreign.Ptr (Stored a) -> Ptr a
castFromStoredPtr = Data.uncheckedFromPtr . Foreign.castPtr


instance (Marshal a) => Store.Storable (Stored a) where
    sizeOf = sizeOf . proxyFromStored
    alignment = alignment . proxyFromStored
    peek = fmap Stored . peek . castFromStoredPtr
    poke ptr = poke (castFromStoredPtr ptr) . getStored

proxyFromStored :: Stored a -> LP.Proxy a
proxyFromStored _ = LP.Proxy
