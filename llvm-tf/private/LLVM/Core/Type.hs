{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- |The LLVM type system is captured with a number of Haskell type classes.
-- In general, an LLVM type @T@ is represented as @Value T@, where @T@ is some Haskell type.
-- The various types @T@ are classified by various type classes, e.g., 'IsFirstClass' for
-- those types that are LLVM first class types (passable as arguments etc).
-- All valid LLVM types belong to the 'IsType' class.
module LLVM.Core.Type(
    -- * Type classifier
    IsType(..),
    -- ** Special type classifiers
    Dec.Natural,
    Dec.Positive,
    IsArithmetic(arithmeticType),
    ArithmeticType(IntegerType,FloatingType),
    IsInteger, Signed,
    IsIntegerOrPointer,
    IsFloating,
    IsPrimitive,
    IsFirstClass,
    IsSized, SizeOf, sizeOf,
    IsFunction,
    Storable, fromPtr, toPtr,
    -- ** Others
    IsScalarOrVector,
    ShapeOf, ScalarShape, VectorShape,
    Shape, ShapedType,
    StructFields,
    PtrSize, IntSize,
    UnknownSize, -- needed for arrays of structs
    -- ** Structs
    ConsStruct(..), consStruct,
    CurryStruct, Curried, curryStruct, uncurryStruct,
    (:&), (&),
    -- ** Type tests
    TypeDesc(..),
    isFloating,
    isSigned,
    typeRef,
    unsafeTypeRef,
    typeName,
    intrinsicTypeName,
    typeDesc2,
    VarArgs, CastVarArgs,
    ) where

import qualified LLVM.FFI.Core as FFI

import qualified LLVM.Core.Data as Data
import LLVM.Core.Util (functionType, structType)
import LLVM.Core.Data
        (IntN, WordN, Vector, Array, FP128,
         Struct(Struct), PackedStruct(PackedStruct), Label)
import LLVM.Core.Proxy (Proxy(Proxy))

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Data.Num.Decimal.Number ((:*:))
import Type.Data.Num.Decimal.Literal (D1, D8, D16, D32, D64, D128, D99)
import Type.Data.Bool (True, False)

import qualified Foreign
import Foreign.StablePtr (StablePtr, )
import Foreign.Ptr (FunPtr)
import System.IO.Unsafe (unsafePerformIO)

import Data.Typeable (Typeable)
import Data.List (intercalate)
import Data.Bits (bitSize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)


#include "MachDeps.h"

-- TODO:
-- Move IntN, WordN to a special module that implements those types
--   properly in Haskell.
-- Also move Array and Vector to a Haskell module to implement them.
-- Add Label?
-- Add structures (using tuples, maybe nested).

-- |The 'IsType' class classifies all types that have an LLVM representation.
class IsType a where
    typeDesc :: Proxy a -> TypeDesc

typeRef :: (IsType a) => Proxy a -> IO FFI.TypeRef
typeRef = code . typeDesc
  where code TDFloat  = FFI.floatType
        code TDDouble = FFI.doubleType
        code TDFP128  = FFI.fp128Type
        code TDVoid   = FFI.voidType
        code (TDInt _ n)  = FFI.integerType (fromInteger n)
        code (TDArray n a) = withCode FFI.arrayType (code a) (fromInteger n)
        code (TDVector n a) = withCode FFI.vectorType (code a) (fromInteger n)
        code (TDPtr a) = withCode FFI.pointerType (code a) 0
        code (TDFunction va as b) = do
            bt <- code b
            ast <- mapM code as
            functionType va bt ast
        code TDLabel = FFI.labelType
        code (TDStruct ts packed) = withCode structType (mapM code ts) packed
        code TDInvalidType = error "typeRef TDInvalidType"

unsafeTypeRef :: (IsType a) => Proxy a -> FFI.TypeRef
unsafeTypeRef = unsafePerformIO . typeRef


withCode ::
    Monad m =>
    (a -> b -> m c) ->
    m a -> b -> m c
withCode f mx y =
    mx >>= \x -> f x y


typeName :: (IsType a) => Proxy a -> String
typeName = code . typeDesc
  where code TDFloat  = "f32"
        code TDDouble = "f64"
        code TDFP128  = "f128"
        code TDVoid   = "void"
        code (TDInt _ n)  = "i" ++ show n
        code (TDArray n a) = "[" ++ show n ++ " x " ++ code a ++ "]"
        code (TDVector n a) = "<" ++ show n ++ " x " ++ code a ++ ">"
        code (TDPtr a) = code a ++ "*"
        code (TDFunction _ as b) = code b ++ "(" ++ intercalate "," (map code as) ++ ")"
        code TDLabel = "label"
        code (TDStruct as packed) = (if packed then "<{" else "{") ++
                                    intercalate "," (map code as) ++
                                    (if packed then "}>" else "}")
        code TDInvalidType = error "typeName TDInvalidType"

intrinsicTypeName :: (IsType a) => Proxy a -> String
intrinsicTypeName = code . typeDesc
  where code TDFloat  = "f32"
        code TDDouble = "f64"
        code TDFP128  = "f128"
        code (TDInt _ n)  = "i" ++ show n
        code (TDVector n a) = "v" ++ show n ++ code a
        code _ = error "intrinsicTypeName: type not supported in intrinsics"

typeDesc2 :: FFI.TypeRef -> IO TypeDesc
typeDesc2 t = do
    tk <- FFI.getTypeKind t
    case tk of
      FFI.VoidTypeKind -> return TDVoid
      FFI.FloatTypeKind -> return TDFloat
      FFI.DoubleTypeKind -> return TDDouble
      -- FIXME: FFI.X86_FP80TypeKind -> return "X86_FP80"
      FFI.FP128TypeKind -> return TDFP128
      -- FIXME: FFI.PPC_FP128TypeKind -> return "PPC_FP128"
      FFI.LabelTypeKind -> return TDLabel
      FFI.IntegerTypeKind -> do
                n <- FFI.getIntTypeWidth t
                return $ TDInt False (fromIntegral n)
      -- FIXME: FFI.FunctionTypeKind
      -- FIXME: FFI.StructTypeKind -> return "(Struct ...)"
      FFI.ArrayTypeKind -> do
                n <- FFI.getArrayLength t
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDArray (fromIntegral n) etd
      FFI.PointerTypeKind -> do
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDPtr etd
      -- FIXME: FFI.OpaqueTypeKind -> return "Opaque"
      FFI.VectorTypeKind -> do
                n <- FFI.getVectorSize t
                et <- FFI.getElementType t
                etd <- typeDesc2 et
                return $ TDVector (fromIntegral n) etd
      -- FIXME: LLVMMetadataTypeKind,    /**< Metadata */
      -- FIXME: LLVMX86_MMXTypeKind      /**< X86 MMX */
      _ -> return TDInvalidType

-- |Type descriptor, used to convey type information through the LLVM API.
data TypeDesc = TDFloat | TDDouble | TDFP128 | TDVoid | TDInt Bool Integer
              | TDArray Integer TypeDesc | TDVector Integer TypeDesc
              | TDPtr TypeDesc | TDFunction Bool [TypeDesc] TypeDesc | TDLabel
              | TDStruct [TypeDesc] Bool | TDInvalidType
    deriving (Eq, Ord, Show, Typeable)

-- XXX isFloating and typeName could be extracted from typeRef
-- Usage:
--   superclass of IsConst
--   add, sub, mul, neg context
--   used to get type name to call intrinsic
-- |Arithmetic types, i.e., integral and floating types.
class IsFirstClass a => IsArithmetic a where
    arithmeticType :: ArithmeticType a

data ArithmeticType a = IntegerType | FloatingType

instance Functor ArithmeticType where
    fmap _ IntegerType  = IntegerType
    fmap _ FloatingType = FloatingType

vectorArithmeticType :: ArithmeticType a -> ArithmeticType (Vector n a)
vectorArithmeticType t =
    case t of
        IntegerType  -> IntegerType
        FloatingType -> FloatingType


-- Usage:
--  constI, allOnes
--  many instructions.  XXX some need vector
--  used to find signedness in Arithmetic
-- |Integral types.
class (IsArithmetic a, IsIntegerOrPointer a) => IsInteger a where
   type Signed a :: *

-- Usage:
--  icmp
-- |Integral or pointer type.
class IsIntegerOrPointer a

isSigned :: (IsArithmetic a) => Proxy a -> Bool
isSigned = is . typeDesc
  where is (TDInt s _) = s
        is (TDVector _ a) = is a
        is TDFloat = True
        is TDDouble = True
        is TDFP128 = True
        is _ = error "isSigned got impossible input"

-- Usage:
--  constF
--  many instructions
-- |Floating types.
class IsArithmetic a => IsFloating a

isFloating :: (IsArithmetic a) => Proxy a -> Bool
isFloating = is . typeDesc
  where is TDFloat = True
        is TDDouble = True
        is TDFP128 = True
        is (TDVector _ a) = is a
        is _ = False

-- Usage:
--  Precondition for Vector
-- |Primitive types.
-- class (IsType a) => IsPrimitive a
class (IsScalarOrVector a, ShapeOf a ~ ScalarShape) => IsPrimitive a

data ScalarShape
data VectorShape n

class Shape shape where
    type ShapedType shape a :: *

instance Shape ScalarShape where
    type ShapedType ScalarShape a = a

instance Shape (VectorShape n) where
    type ShapedType (VectorShape n) a = Vector n a

-- |Number of elements for instructions that handle both primitive and vector types
class (IsFirstClass a) => IsScalarOrVector a where
    type ShapeOf a :: *


-- Usage:
--  Precondition for function args and result.
--  Used by some instructions, like ret and phi.
--  XXX IsSized as precondition?
-- |First class types, i.e., the types that can be passed as arguments, etc.
class IsType a => IsFirstClass a

-- Usage:
--  Context for Array being a type
--  thus, allocation instructions
-- |Types with a fixed size.
class (IsType a, Dec.Natural (SizeOf a)) => IsSized a where
    type SizeOf a :: *

sizeOf :: TypeDesc -> Integer
sizeOf TDFloat  = 32
sizeOf TDDouble = 64
sizeOf TDFP128  = 128
sizeOf (TDInt _ bits) = bits
sizeOf (TDArray n typ) = n * sizeOf typ
sizeOf (TDVector n typ) = n * sizeOf typ
sizeOf (TDStruct ts _packed) = sum (map sizeOf ts)
sizeOf _ = error "type has no size"

-- |Function type.
class (IsType a) => IsFunction a where
    funcType :: [TypeDesc] -> Proxy a -> TypeDesc

-- Only make instances for types that make sense in Haskell
-- (i.e., some floating types are excluded).

-- Floating point types.
instance IsType Float  where typeDesc _ = TDFloat
instance IsType Double where typeDesc _ = TDDouble
instance IsType FP128  where typeDesc _ = TDFP128

-- Void type
instance IsType ()     where typeDesc _ = TDVoid

-- Label type
instance IsType Label  where typeDesc _ = TDLabel

-- Variable size integer types
instance (Dec.Positive n) => IsType (IntN n)
    where typeDesc _ =
             TDInt True
                (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n))

instance (Dec.Positive n) => IsType (WordN n)
    where typeDesc _ =
             TDInt False
                (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n))

-- Fixed size integer types.
instance IsType Bool   where typeDesc _ = TDInt False  1
instance IsType Word8  where typeDesc _ = TDInt False  8
instance IsType Word16 where typeDesc _ = TDInt False 16
instance IsType Word32 where typeDesc _ = TDInt False 32
instance IsType Word64 where typeDesc _ = TDInt False 64
instance IsType Word   where
   typeDesc _ = TDInt False (toInteger$bitSize(0::Word))
instance IsType Int8   where typeDesc _ = TDInt True   8
instance IsType Int16  where typeDesc _ = TDInt True  16
instance IsType Int32  where typeDesc _ = TDInt True  32
instance IsType Int64  where typeDesc _ = TDInt True  64
instance IsType Int    where
   typeDesc _ = TDInt True  (toInteger$bitSize(0::Int))

-- Sequence types
instance (Dec.Natural n, IsSized a) => IsType (Array n a)
    where typeDesc _ =
             TDArray
                (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n))
                (typeDesc (Proxy :: Proxy a))
instance (Dec.Positive n, IsPrimitive a) => IsType (Vector n a)
    where typeDesc _ =
             TDVector
                (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n))
                (typeDesc (Proxy :: Proxy a))

-- Pointer type.
instance IsType (Foreign.Ptr a) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy (Struct ())))

instance (IsType a) => IsType (Data.Ptr a) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy a))

instance (IsFunction f) => IsType (FunPtr f) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy f))

instance IsType (StablePtr a) where
    typeDesc _ = TDPtr (typeDesc (Proxy :: Proxy (Struct ())))
{-
    typeDesc _ = TDPtr TDVoid

List: Type.cpp:1311: static llvm::PointerType* llvm::PointerType::get(const llvm::Type*, unsigned int): Assertion `ValueType != Type::VoidTy && "Pointer to void is not valid, use sbyte* instead!"' failed.
-}


-- Functions.
instance (IsFirstClass a, IsFunction b) => IsType (a->b) where
    typeDesc = funcType []

-- Function base type, always IO.
instance (IsFirstClass a) => IsType (IO a) where
    typeDesc = funcType []

-- Struct types, basically a list of component types.
instance (StructFields a) => IsType (Struct a) where
    typeDesc p = TDStruct (fieldTypes $ fmap (\(Struct a) -> a) p) False

instance (StructFields a) => IsType (PackedStruct a) where
    typeDesc p = TDStruct (fieldTypes $ fmap (\(PackedStruct a) -> a) p) True

-- Use a nested tuples for struct fields.
class StructFields as where
    fieldTypes :: Proxy as -> [TypeDesc]

instance (IsSized a, StructFields as) => StructFields (a :& as) where
    fieldTypes p = typeDesc (fmap fst p) : fieldTypes (fmap snd p)
instance StructFields () where
    fieldTypes Proxy = []


-- Simplifies construction, pattern matching and conversion to and from records
class ConsStruct f where
    type PartialStruct f
    type ConsResult f
    curryConsStruct :: (PartialStruct f -> Struct (ConsResult f)) -> f

instance ConsStruct (Struct a) where
    type PartialStruct (Struct a) = ()
    type ConsResult (Struct a) = a
    curryConsStruct g = g ()

instance (ConsStruct f) => ConsStruct (a->f) where
    type PartialStruct (a->f) = (a, PartialStruct f)
    type ConsResult (a->f) = ConsResult f
    curryConsStruct g a = curryConsStruct (\r -> g (a,r))

consStruct :: (ConsStruct f, ConsResult f ~ PartialStruct f) => f
consStruct = curryConsStruct Struct

class CurryStruct a where
    type Curried a b
    curryStruct' :: (a -> b) -> Curried a b
    uncurryStruct' :: Curried a b -> a -> b

instance CurryStruct () where
    type Curried () b = b
    curryStruct' f = f ()
    uncurryStruct' f () = f

instance (CurryStruct r) => CurryStruct (a,r) where
    type Curried (a,r) b = a -> Curried r b
    curryStruct' f a = curryStruct' (\r -> f (a,r))
    uncurryStruct' f (a,r) = uncurryStruct' (f a) r

curryStruct :: (CurryStruct a) => (Struct a -> b) -> Curried a b
curryStruct f = curryStruct' (f . Struct)

uncurryStruct :: (CurryStruct a) => Curried a b -> (Struct a -> b)
uncurryStruct f (Struct a) = uncurryStruct' f a


-- An alias for pairs to make structs look nicer
infixr :&
type (:&) a as = (a, as)
infixr &
(&) :: a -> as -> a :& as
a & as = (a, as)


--- Instances to classify types
instance IsArithmetic Float  where arithmeticType = FloatingType
instance IsArithmetic Double where arithmeticType = FloatingType
instance IsArithmetic FP128  where arithmeticType = FloatingType
instance (Dec.Positive n) => IsArithmetic (IntN n)  where arithmeticType = IntegerType
instance (Dec.Positive n) => IsArithmetic (WordN n) where arithmeticType = IntegerType
{-
This instance is more dangerous than useful.
E.g. 'inv' can be mixed up with 'neg'.
For arithmetic on i1 you might better use @IntN D1@ or @WordN D1@.
-}
instance IsArithmetic Bool   where arithmeticType = IntegerType
instance IsArithmetic Int8   where arithmeticType = IntegerType
instance IsArithmetic Int16  where arithmeticType = IntegerType
instance IsArithmetic Int32  where arithmeticType = IntegerType
instance IsArithmetic Int64  where arithmeticType = IntegerType
instance IsArithmetic Int    where arithmeticType = IntegerType
instance IsArithmetic Word8  where arithmeticType = IntegerType
instance IsArithmetic Word16 where arithmeticType = IntegerType
instance IsArithmetic Word32 where arithmeticType = IntegerType
instance IsArithmetic Word64 where arithmeticType = IntegerType
instance IsArithmetic Word   where arithmeticType = IntegerType
instance (Dec.Positive n, IsPrimitive a, IsArithmetic a) =>
         IsArithmetic (Vector n a) where
   arithmeticType = vectorArithmeticType arithmeticType
--   arithmeticType = fmap (pure :: a -> Vector n a) arithmeticType

instance IsFloating Float
instance IsFloating Double
instance IsFloating FP128
instance (Dec.Positive n, IsPrimitive a, IsFloating a) => IsFloating (Vector n a)

data Indecisive

instance (Dec.Positive n) => IsInteger (IntN  n) where type Signed (IntN  n) = True
instance (Dec.Positive n) => IsInteger (WordN n) where type Signed (WordN n) = False
instance IsInteger Bool   where type Signed Bool = Indecisive
instance IsInteger Int8   where type Signed Int8 = True
instance IsInteger Int16  where type Signed Int16 = True
instance IsInteger Int32  where type Signed Int32 = True
instance IsInteger Int64  where type Signed Int64 = True
instance IsInteger Int    where type Signed Int   = True
instance IsInteger Word8  where type Signed Word8 = False
instance IsInteger Word16 where type Signed Word16 = False
instance IsInteger Word32 where type Signed Word32 = False
instance IsInteger Word64 where type Signed Word64 = False
instance IsInteger Word   where type Signed Word   = False
instance (Dec.Positive n, IsPrimitive a, IsInteger a) => IsInteger (Vector n a)
                          where type Signed (Vector n a) = Signed a

instance (Dec.Positive n) => IsIntegerOrPointer (IntN n)
instance (Dec.Positive n) => IsIntegerOrPointer (WordN n)
instance IsIntegerOrPointer Bool
instance IsIntegerOrPointer Int8
instance IsIntegerOrPointer Int16
instance IsIntegerOrPointer Int32
instance IsIntegerOrPointer Int64
instance IsIntegerOrPointer Int
instance IsIntegerOrPointer Word8
instance IsIntegerOrPointer Word16
instance IsIntegerOrPointer Word32
instance IsIntegerOrPointer Word64
instance IsIntegerOrPointer Word
instance (Dec.Positive n, IsPrimitive a, IsInteger a) => IsIntegerOrPointer (Vector n a)
instance IsIntegerOrPointer (Foreign.Ptr a)
instance (IsType a) => IsIntegerOrPointer (Data.Ptr a)

instance IsFirstClass Float
instance IsFirstClass Double
instance IsFirstClass FP128
instance (Dec.Positive n) => IsFirstClass (IntN n)
instance (Dec.Positive n) => IsFirstClass (WordN n)
instance IsFirstClass Bool
instance IsFirstClass Int
instance IsFirstClass Int8
instance IsFirstClass Int16
instance IsFirstClass Int32
instance IsFirstClass Int64
instance IsFirstClass Word
instance IsFirstClass Word8
instance IsFirstClass Word16
instance IsFirstClass Word32
instance IsFirstClass Word64
instance (Dec.Positive n, IsPrimitive a) => IsFirstClass (Vector n a)
instance (Dec.Natural n, IsSized a) => IsFirstClass (Array n a)
instance IsFirstClass (Foreign.Ptr a)
instance (IsType a) => IsFirstClass (Data.Ptr a)
instance (IsFunction a) => IsFirstClass (FunPtr a)
instance IsFirstClass (StablePtr a)
instance IsFirstClass Label
instance IsFirstClass () -- XXX This isn't right, but () can be returned
instance (StructFields as) => IsFirstClass (Struct as)


{- |
Types where LLVM and 'Foreign.Storable' memory layout are compatible.
-}
class (Foreign.Storable a, IsFirstClass a, IsSized a) => Storable a
instance Storable Float
instance Storable Double
instance Storable Int
instance Storable Int8
instance Storable Int16
instance Storable Int32
instance Storable Int64
instance Storable Word
instance Storable Word8
instance Storable Word16
instance Storable Word32
instance Storable Word64
instance (Foreign.Storable a) => Storable (Foreign.Ptr a)
instance (IsType a) => Storable (Data.Ptr a)
instance (IsFunction a) => Storable (FunPtr a)
instance Storable (StablePtr a) where

fromPtr :: (Storable a) => Foreign.Ptr a -> Data.Ptr a
fromPtr = Data.uncheckedFromPtr

toPtr :: (Storable a) => Data.Ptr a -> Foreign.Ptr a
toPtr = Data.uncheckedToPtr


instance (Dec.Positive n) => IsSized (IntN n)  where type SizeOf (IntN  n) = n
instance (Dec.Positive n) => IsSized (WordN n) where type SizeOf (WordN n) = n
instance IsSized Float  where type SizeOf Float  = D32
instance IsSized Double where type SizeOf Double = D64
instance IsSized FP128  where type SizeOf FP128  = D128
instance IsSized Bool   where type SizeOf Bool   = D1
instance IsSized Int8   where type SizeOf Int8   = D8
instance IsSized Int16  where type SizeOf Int16  = D16
instance IsSized Int32  where type SizeOf Int32  = D32
instance IsSized Int64  where type SizeOf Int64  = D64
instance IsSized Int    where type SizeOf Int    = IntSize
instance IsSized Word8  where type SizeOf Word8  = D8
instance IsSized Word16 where type SizeOf Word16 = D16
instance IsSized Word32 where type SizeOf Word32 = D32
instance IsSized Word64 where type SizeOf Word64 = D64
instance IsSized Word   where type SizeOf Word   = IntSize
{-
Can we derive Dec.Natural (n :*: SizeOf a)
from (Dec.Natural n, Dec.Natural (n :*: SizeOf a))?
-}
instance
    (Dec.Natural n, IsSized a, Dec.Natural (n :*: SizeOf a)) =>
        IsSized (Array n a) where
    type SizeOf (Array n a) = n :*: SizeOf a
instance
    (Dec.Positive n, IsPrimitive a, IsSized a, Dec.Natural (n :*: SizeOf a)) =>
        IsSized (Vector n a) where
    type SizeOf (Vector n a) = n :*: SizeOf a
instance IsSized (Foreign.Ptr a) where type SizeOf (Foreign.Ptr a) = PtrSize
instance (IsType a) => IsSized (Data.Ptr a) where
    type SizeOf (Data.Ptr a) = PtrSize
instance (IsFunction a) => IsSized (FunPtr a) where
    type SizeOf (FunPtr a) =  PtrSize
instance IsSized (StablePtr a) where type SizeOf (StablePtr a) =  PtrSize
-- instance IsSized Label PtrSize -- labels are not quite first classed
-- We cannot compute the sizes statically :(
instance (StructFields as) => IsSized (Struct as) where
    type SizeOf (Struct as) = UnknownSize
instance (StructFields as) => IsSized (PackedStruct as) where
    type SizeOf (PackedStruct as) = UnknownSize

type UnknownSize = D99   -- XXX this is wrong!

type IntSize = PtrSize
#if WORD_SIZE_IN_BITS == 32
type PtrSize = D32
#elif WORD_SIZE_IN_BITS == 64
type PtrSize = D64
#else
#error cannot determine type of PtrSize
#endif

instance IsPrimitive Float
instance IsPrimitive Double
instance IsPrimitive FP128
instance (Dec.Positive n) => IsPrimitive (IntN n)
instance (Dec.Positive n) => IsPrimitive (WordN n)
instance IsPrimitive Bool
instance IsPrimitive Int8
instance IsPrimitive Int16
instance IsPrimitive Int32
instance IsPrimitive Int64
instance IsPrimitive Int
instance IsPrimitive Word8
instance IsPrimitive Word16
instance IsPrimitive Word32
instance IsPrimitive Word64
instance IsPrimitive Word
instance IsPrimitive Label
instance IsPrimitive ()
instance IsPrimitive (Foreign.Ptr a)
instance (IsType a) => IsPrimitive (Data.Ptr a)


instance (Dec.Positive n) =>
         IsScalarOrVector (IntN n)  where type ShapeOf (IntN n)  = ScalarShape
instance (Dec.Positive n) =>
         IsScalarOrVector (WordN n) where type ShapeOf (WordN n) = ScalarShape
instance IsScalarOrVector Float  where type ShapeOf Float  = ScalarShape
instance IsScalarOrVector Double where type ShapeOf Double = ScalarShape
instance IsScalarOrVector FP128  where type ShapeOf FP128  = ScalarShape
instance IsScalarOrVector Bool   where type ShapeOf Bool   = ScalarShape
instance IsScalarOrVector Int8   where type ShapeOf Int8   = ScalarShape
instance IsScalarOrVector Int16  where type ShapeOf Int16  = ScalarShape
instance IsScalarOrVector Int32  where type ShapeOf Int32  = ScalarShape
instance IsScalarOrVector Int64  where type ShapeOf Int64  = ScalarShape
instance IsScalarOrVector Int    where type ShapeOf Int    = ScalarShape
instance IsScalarOrVector Word8  where type ShapeOf Word8  = ScalarShape
instance IsScalarOrVector Word16 where type ShapeOf Word16 = ScalarShape
instance IsScalarOrVector Word32 where type ShapeOf Word32 = ScalarShape
instance IsScalarOrVector Word64 where type ShapeOf Word64 = ScalarShape
instance IsScalarOrVector Word   where type ShapeOf Word   = ScalarShape
instance IsScalarOrVector Label  where type ShapeOf Label  = ScalarShape
instance IsScalarOrVector ()     where type ShapeOf ()     = ScalarShape
instance IsScalarOrVector (Foreign.Ptr a) where
    type ShapeOf (Foreign.Ptr a) = ScalarShape
instance (IsType a) => IsScalarOrVector (Data.Ptr a) where
    type ShapeOf (Data.Ptr a) = ScalarShape

instance (Dec.Positive n, IsPrimitive a) =>
         IsScalarOrVector (Vector n a) where
    type ShapeOf (Vector n a) = VectorShape n


-- Functions.
instance (IsFirstClass a, IsFunction b) => IsFunction (a->b) where
    funcType ts _ = funcType (typeDesc (Proxy :: Proxy a) : ts) (Proxy :: Proxy b)
instance (IsFirstClass a) => IsFunction (IO a) where
    funcType ts _ = TDFunction False (reverse ts) (typeDesc (Proxy :: Proxy a))
instance (IsFirstClass a) => IsFunction (VarArgs a) where
    funcType ts _ = TDFunction True  (reverse ts) (typeDesc (Proxy :: Proxy a))

-- |The 'VarArgs' type is a placeholder for the real 'IO' type that
-- can be obtained with 'castVarArgs'.
data VarArgs a
    deriving (Typeable)
instance IsType (VarArgs a) where
    typeDesc _ = error "typeDesc: Dummy type VarArgs used incorrectly"

-- |Define what vararg types are permissible.
class CastVarArgs a b
instance (x~y, CastVarArgs a b) => CastVarArgs (x -> a) (y -> b)
instance (x~y) => CastVarArgs (VarArgs x) (IO y)
instance (IsFirstClass x, CastVarArgs (VarArgs a) b) =>
            CastVarArgs (VarArgs a) (x -> b)




-- XXX Structures not implemented.  Tuples is probably an easy way.

