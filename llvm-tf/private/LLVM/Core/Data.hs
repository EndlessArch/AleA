{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Core.Data (
    Ptr(..), uncheckedFromPtr, uncheckedToPtr,
    IntN(..), WordN(..), FP128(..),
    Array(..), Vector(..), Label, Struct(..), PackedStruct(..),
    FixedList,
    ) where

import qualified LLVM.Core.UnaryVector as UnaryVector
import LLVM.Core.UnaryVector (FixedList)

import qualified Type.Data.Num.Decimal.Proof as DecProof
import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Base.Proxy (Proxy(Proxy))

import qualified Foreign

import qualified Data.Foldable as Fold
import qualified Data.Bits as Bits

import Data.Typeable (Typeable)

import qualified Test.QuickCheck as QC


{- |
We export the constructor such that you can use 'Ptr' in foreign imports.
However, we recommend that you call 'uncheckedFromPtr' instead.
-}
newtype Ptr a = Ptr (Foreign.Ptr a)
    deriving (Show, Eq, Ord, Typeable)

uncheckedFromPtr :: Foreign.Ptr a -> Ptr a
uncheckedFromPtr = Ptr

uncheckedToPtr :: Ptr a -> Foreign.Ptr a
uncheckedToPtr (Ptr ptr) = ptr

instance Foreign.Storable (Ptr a) where
    sizeOf = Foreign.sizeOf . uncheckedToPtr
    alignment = Foreign.alignment . uncheckedToPtr
    poke p = Foreign.pokeByteOff p 0 . uncheckedToPtr
    peek p = fmap uncheckedFromPtr $ Foreign.peekByteOff p 0


-- TODO:
-- Make instances IntN, WordN to actually do the right thing.
-- Make FP128 do the right thing
-- Make Array functions.

-- |Variable sized signed integer.
-- The /n/ parameter should belong to @PosI@.
newtype IntN n = IntN Integer
    deriving (Show, Eq, Ord, Typeable)

instance (Dec.Positive n) => QC.Arbitrary (IntN n) where
    arbitrary = arbitraryInt IntN (\(IntN a) -> a)

instance (Dec.Positive n) => Bounded (IntN n) where
    minBound =
        withBitSize $
        IntN . negate . Bits.shiftL 1 . subtract 1 . Dec.integralFromProxy
    maxBound =
        withBitSize $
        IntN . subtract 1 . Bits.shiftL 1 . subtract 1 . Dec.integralFromProxy

-- |Variable sized unsigned integer.
-- The /n/ parameter should belong to @PosI@.
newtype WordN n = WordN Integer
    deriving (Show, Eq, Ord, Typeable)

instance (Dec.Positive n) => QC.Arbitrary (WordN n) where
    arbitrary = arbitraryInt WordN (\(WordN a) -> a)

instance (Dec.Positive n) => Bounded (WordN n) where
    minBound = WordN 0
    maxBound =
        withBitSize $ WordN . subtract 1 . Bits.shiftL 1 . Dec.integralFromProxy

arbitraryInt :: (Bounded a) => (Integer -> a) -> (a -> Integer) -> QC.Gen a
arbitraryInt wrap unwrap =
    case (minBound, maxBound) of
        (a,b) -> do
            x <- QC.choose (unwrap a, unwrap b)
            return $ wrap x `asTypeOf` a `asTypeOf` b

withBitSize :: (Proxy n -> f n) -> f n
withBitSize f = f Proxy

-- |128 bit floating point.
newtype FP128 = FP128 Rational
    deriving (Show, Typeable)


-- |Fixed sized arrays, the array size is encoded in the /n/ parameter.
newtype Array n a = Array [a]
    deriving (Eq, Show, Typeable)

instance (Dec.Integer n) => Fold.Foldable (Array n) where
    foldMap f (Array xs) = Fold.foldMap f xs

instance (Dec.Integer n, QC.Arbitrary a) => QC.Arbitrary (Array n a) where
    arbitrary = withArraySize $ fmap Array . QC.vector . Dec.integralFromProxy

withArraySize :: (Proxy n -> gen (Array n a)) -> gen (Array n a)
withArraySize f = f Proxy

-- |Fixed sized vector, the array size is encoded in the /n/ parameter.
newtype Vector n a = Vector (FixedList (Dec.ToUnary n) a)

instance (Dec.Natural n, Show a) => Show (Vector n a) where
    showsPrec p (Vector xs) =
        case DecProof.unaryNat :: DecProof.UnaryNat n of
            DecProof.UnaryNat ->
                showParen (p>10) $
                    showString "Vector " .
                    showList (Fold.toList
                        (UnaryVector.fromFixedList xs
                            :: UnaryVector.T (Dec.ToUnary n) a))

-- |Label type, produced by a basic block.
data Label
    deriving (Typeable)

-- |Struct types; a list (nested tuple) of component types.
newtype Struct a = Struct a
    deriving (Eq, Show, Typeable)
newtype PackedStruct a = PackedStruct a
    deriving (Eq, Show, Typeable)

instance (QC.Arbitrary a) => QC.Arbitrary (Struct a) where
    arbitrary = fmap Struct QC.arbitrary
