{-# LANGUAGE TypeFamilies #-}
module LLVM.Core.UnaryVector (
   T, vector, cyclicVector,
   FixedLength.fromFixedList, FixedLength.toFixedList, FixedLength.head,
   FixedList, Length,
   FixedLength.Curried, FixedLength.uncurry, FixedLength.curry,
   ) where

import qualified Type.Data.Num.Unary as Unary

import qualified Data.FixedLength as FixedLength
import Data.FixedLength (T, List, Length, end, (!:))

import qualified Data.NonEmpty as NonEmpty

import Prelude hiding (head)


type FixedList n = List n


vector :: (Unary.Natural n, n ~ Length (List n)) => List n a -> T n a
vector = FixedLength.fromFixedList

cyclicVector :: (Unary.Natural n) => NonEmpty.T [] a -> T n a
cyclicVector xt@(NonEmpty.Cons x xs) =
   runOp0 $
   Unary.switchNat
      (Op0 end)
      (Op0 $ x !: cyclicVectorAppend xt xs)

cyclicVectorAppend :: (Unary.Natural n) => NonEmpty.T [] a -> [a] -> T n a
cyclicVectorAppend ys xt =
   runOp0 $
   Unary.switchNat
      (Op0 end)
      (Op0 $
       case xt of
          [] -> cyclicVector ys
          x:xs -> x !: cyclicVectorAppend ys xs)

newtype Op0 a n = Op0 {runOp0 :: T n a}
