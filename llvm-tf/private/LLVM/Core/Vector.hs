{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module LLVM.Core.Vector (MkVector(..), vector, cyclicVector, consVector) where

import qualified LLVM.Core.UnaryVector as UnaryVector
import LLVM.Core.Data (Vector(Vector), FixedList)

import qualified Type.Data.Num.Decimal.Proof as DecProof
import qualified Type.Data.Num.Decimal.Number as Dec
import qualified Type.Data.Num.Unary as Unary
import qualified Type.Base.Proxy as Proxy
import Type.Data.Num.Decimal.Literal (D2, D4, D8)

import qualified Foreign.Storable.Traversable as Store
import Foreign.Storable.FixedArray (sizeOfArray)
import Foreign.Storable (Storable(..))

import qualified Test.QuickCheck as QC

import qualified Control.Monad.Trans.State as MS
import Control.Applicative (Applicative, pure, liftA2, (<*>))
import Control.Functor.HT (unzip, outerProduct)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.Traversable (Traversable, foldMapDefault)
import Data.Foldable (Foldable, foldMap)

import Prelude hiding (replicate, map, head, unzip, zipWith, uncurry)


-- XXX Should these really be here?
class (Dec.Positive n) => MkVector n where
    type Tuple n a
    toVector :: Tuple n a -> Vector n a
    fromVector :: Vector n a -> Tuple n a


instance MkVector D2 where
    type Tuple D2 a = (a,a)
    toVector (a1, a2) = consVector a1 a2
    fromVector = uncurry (,)

instance MkVector D4 where
    type Tuple D4 a = (a,a,a,a)
    toVector (a1, a2, a3, a4) = consVector a1 a2 a3 a4
    fromVector = uncurry (,,,)

instance MkVector D8 where
    type Tuple D8 a = (a,a,a,a,a,a,a,a)
    toVector (a1, a2, a3, a4, a5, a6, a7, a8) =
        consVector a1 a2 a3 a4 a5 a6 a7 a8
    fromVector = uncurry (,,,,,,,)


head :: (Dec.Positive n) => Vector n a -> a
head =
    withPosDict1 $ \dict v ->
        case dict of
            DecProof.UnaryPos ->
                UnaryVector.head . unaryFromDecimalVector $ v


unaryFromDecimalVector :: Vector n a -> UnaryVector.T (Dec.ToUnary n) a
unaryFromDecimalVector (Vector xs) = UnaryVector.fromFixedList xs

decimalFromUnaryVector :: UnaryVector.T (Dec.ToUnary n) a -> Vector n a
decimalFromUnaryVector = Vector . UnaryVector.toFixedList


type Curried n a b = UnaryVector.Curried (Dec.ToUnary n) a b

uncurry :: (Dec.Natural n) => Curried n a b -> Vector n a -> b
uncurry f =
    withNatDict1 $ \dict v ->
        case dict of
            DecProof.UnaryNat ->
                UnaryVector.uncurry f $ unaryFromDecimalVector v


withNatDict ::
    (Dec.Natural n) =>
    (DecProof.UnaryNat n -> Vector n a) -> Vector n a
withNatDict f = f DecProof.unaryNat

withNatDict1 ::
    (Dec.Natural n) =>
    (DecProof.UnaryNat n -> Vector n a -> b) -> Vector n a -> b
withNatDict1 f = f DecProof.unaryNat

withPosDict1 ::
    (Dec.Positive n) =>
    (DecProof.UnaryPos n -> Vector n a -> b) -> Vector n a -> b
withPosDict1 f = f DecProof.unaryPos


withUnaryDecVector ::
    (Dec.Natural n) =>
    (forall m. (Dec.ToUnary n ~ m, Unary.Natural m) => UnaryVector.T m a) ->
    Vector n a
withUnaryDecVector v =
    withNatDict
        (\dict ->
            case dict of DecProof.UnaryNat -> decimalFromUnaryVector v)

instance (Storable a, Dec.Positive n) => Storable (Vector n a) where
    sizeOf v = sizeOfArray (Dec.integralFromProxy $ size v) (head v)
    alignment = alignment . head
    peek = Store.peekApplicative
    poke = Store.poke

size :: Vector n a -> Proxy.Proxy n
size _ = Proxy.Proxy

--------------------------------------

{- maybe we should export this in order to allow NumericPrelude instances
unVector :: (Dec.Positive n) => Vector n a -> FixedList n a
unVector (Vector xs) = xs
-}

vector :: (Dec.Positive n) => FixedList (Dec.ToUnary n) a -> Vector n a
vector = Vector

{- |
Make a constant vector.  Replicates or truncates the list to get length /n/.
This behaviour is consistent uncurry that of 'LLVM.Core.CodeGen.constCyclicVector'.
May be abused for constructing vectors from lists uncurry statically unknown size.
-}
cyclicVector :: (Dec.Positive n) => NonEmpty.T [] a -> Vector n a
cyclicVector xs =
   withUnaryDecVector (UnaryVector.cyclicVector xs)


class ConsVector f where
   type NumberOfArguments f
   type ResultSize f
   type ResultElement f
   consAux ::
      (NumberOfArguments f ~ m, ResultSize f ~ n, ResultElement f ~ a) =>
      (FixedList m a -> Vector n a) -> f

instance ConsVector (Vector n a) where
   type NumberOfArguments (Vector n a) = Unary.Zero
   type ResultSize (Vector n a) = n
   type ResultElement (Vector n a) = a
   consAux f = f Empty.Cons

instance (a ~ ResultElement f, ConsVector f) => ConsVector (a -> f) where
   type NumberOfArguments (a->f) = Unary.Succ (NumberOfArguments f)
   type ResultSize (a->f) = ResultSize f
   type ResultElement (a->f) = ResultElement f
   consAux f x = consAux (f . NonEmpty.Cons x)

consVector ::
   (ConsVector f, ResultSize f ~ n, NumberOfArguments f ~ u,
    u ~ Dec.ToUnary n, Dec.FromUnary u ~ n, Dec.Natural n) => f
consVector = consAux Vector


replicate :: (Dec.Positive n) => a -> Vector n a
replicate a = withUnaryDecVector (pure a)


instance (Dec.Positive n) => Functor (Vector n) where
   fmap f a =
      withUnaryDecVector (fmap f $ unaryFromDecimalVector a)

instance (Dec.Positive n) => Applicative (Vector n) where
   pure = replicate
   f <*> a =
      withUnaryDecVector
         (unaryFromDecimalVector f <*> unaryFromDecimalVector a)

instance (Dec.Positive n) => Foldable (Vector n) where
   foldMap = foldMapDefault

instance (Dec.Positive n) => Traversable (Vector n) where
   sequenceA =
      withNatDict1 $ \dict v ->
         case dict of
            DecProof.UnaryNat ->
               fmap decimalFromUnaryVector $ Trav.sequenceA $
               unaryFromDecimalVector v



instance (Eq a, Dec.Positive n) => Eq (Vector n a) where
   x == y  =  Fold.and $ liftA2 (==) x y

instance (Ord a, Dec.Positive n) => Ord (Vector n a) where
   compare x y =
      Fold.foldr (\r rs -> if r==EQ then rs else r) EQ $
      liftA2 compare x y

instance (Num a, Dec.Positive n) => Num (Vector n a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Enum a, Dec.Positive n) => Enum (Vector n a) where
    succ = fmap succ
    pred = fmap pred
    fromEnum = error "Vector fromEnum"
    toEnum = pure . toEnum

instance (Real a, Dec.Positive n) => Real (Vector n a) where
    toRational = error "Vector toRational"

instance (Integral a, Dec.Positive n) => Integral (Vector n a) where
    quot = liftA2 quot
    rem  = liftA2 rem
    div  = liftA2 div
    mod  = liftA2 mod
    quotRem xs ys = unzip $ liftA2 quotRem xs ys
    divMod  xs ys = unzip $ liftA2 divMod  xs ys
    toInteger = error "Vector toInteger"

instance (Fractional a, Dec.Positive n) => Fractional (Vector n a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance (RealFrac a, Dec.Positive n) => RealFrac (Vector n a) where
    properFraction = error "Vector properFraction"

instance (Floating a, Dec.Positive n) => Floating (Vector n a) where
    pi = pure pi
    sqrt = fmap sqrt
    log = fmap log
    logBase = liftA2 logBase
    (**) = liftA2 (**)
    exp = fmap exp
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

instance (RealFloat a, Dec.Positive n) => RealFloat (Vector n a) where
    floatRadix = floatRadix . head
    floatDigits = floatDigits . head
    floatRange = floatRange . head
    decodeFloat = error "Vector decodeFloat"
    encodeFloat = error "Vector encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "Vector scaleFloat"
    isNaN = error "Vector isNaN"
    isInfinite = error "Vector isInfinite"
    isDenormalized = error "Vector isDenormalized"
    isNegativeZero = error "Vector isNegativeZero"
    isIEEE = isIEEE . head


indices :: (Dec.Positive n) => Vector n Int
indices =
    flip MS.evalState 0 $ Trav.sequenceA $ replicate $ MS.state (\k -> (k,k+1))

instance (Dec.Positive n, QC.Arbitrary a) => QC.Arbitrary (Vector n a) where
    arbitrary = Trav.sequenceA $ replicate QC.arbitrary
    shrink v =
        case indices of
            ixs ->
                concatMap
                    (Trav.sequenceA .
                     liftA2
                        (\x doShrink ->
                            if doShrink then QC.shrink x else [x]) v) $
                outerProduct (==) (Fold.toList ixs) ixs
