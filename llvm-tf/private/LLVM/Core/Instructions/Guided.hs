{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{- |
This module provides some functions from the "LLVM.Core.Instructions" module
in a way that enables easier type handling.
E.g. 'trunc' on vectors requires you to prove
that reducing the bitsize of the elements
reduces the bitsize of the whole vector.
We solve the problem by adding a 'Guide' parameter.
It can be either 'scalar' or 'vector'.
We impose the bitsize constraint only on the element type,
but not on the size of the whole value (scalar or vector).

Another example:
If you call 'trunc' on a Vector input,
GHC cannot infer that the result must be a 'Data.Vector' of the same size.
Using the guide, it can.
However, in practice this is not as useful as I thought initially.
-}
module LLVM.Core.Instructions.Guided (
    Guide,
    scalar,
    vector,
    getElementPtr,
    getElementPtr0,
    trunc,
    ext,
    extBool,
    zadapt,
    sadapt,
    adapt,
    fptrunc,
    fpext,
    fptoint,
    inttofp,
    ptrtoint,
    inttoptr,
    bitcast,
    select,
    cmp,
    icmp,
    pcmp,
    fcmp,
    ) where

import qualified LLVM.Core.Instructions.Private as Priv
import qualified LLVM.Core.Type as Type
import qualified LLVM.Core.Util as U
import qualified LLVM.Core.Proxy as LP
import LLVM.Core.Instructions.Private (ValueCons)
import LLVM.Core.CodeGenMonad (CodeGenFunction)
import LLVM.Core.CodeGen (ConstValue, zero)
import LLVM.Core.Type
         (IsArithmetic, IsInteger, IsIntegerOrPointer, IsFloating,
          IsFirstClass, IsPrimitive,
          Signed, Positive, IsType, IsSized, SizeOf,
          isFloating, sizeOf, typeDesc)

import qualified LLVM.FFI.Core as FFI

import Type.Data.Num.Decimal.Number ((:<:), (:>:))

import Foreign.Ptr (Ptr)

import qualified Control.Functor.HT as FuncHT

import Data.Word (Word32)


data Guide shape elem = Guide

instance Functor (Guide shape) where
    fmap _ Guide = Guide

scalar :: Guide Type.ScalarShape a
scalar = Guide

vector :: (Positive n) => Guide (Type.VectorShape n) a
vector = Guide

proxyFromGuide :: Guide shape elem -> LP.Proxy elem
proxyFromGuide Guide = LP.Proxy


type Type shape a = Type.ShapedType shape a
type VT value shape a = value (Type shape a)

getElementPtr ::
    (ValueCons value, Priv.GetElementPtr o i, Priv.IsIndexType i0) =>
    Guide shape (Ptr o, i0) ->
    VT value shape (Ptr o) ->
    (VT value shape i0, i) ->
    CodeGenFunction r (VT value shape (Ptr (Priv.ElementPtrType o i)))
getElementPtr guide vptr (a, ixs) =
    getElementPtrGen (fmap fst guide) vptr (Priv.unValue a, ixs)

getElementPtr0 ::
    (ValueCons value, Priv.GetElementPtr o i) =>
    Guide shape (Ptr o) ->
    VT value shape (Ptr o) -> i ->
    CodeGenFunction r (VT value shape (Ptr (Priv.ElementPtrType o i)))
getElementPtr0 guide vptr ixs =
    getElementPtrGen guide vptr
        (Priv.unConst (zero :: ConstValue Word32), ixs)

getElementPtrGen ::
    (ValueCons value, Priv.GetElementPtr o i) =>
    Guide shape (Ptr o) ->
    VT value shape (Ptr o) -> (FFI.ValueRef, i) ->
    CodeGenFunction r (VT value shape (Ptr (Priv.ElementPtrType o i)))
getElementPtrGen guide vptr (i0val,ixs) =
    let withArgs act =
            U.withArrayLen
                (i0val : Priv.getIxList (LP.element (proxyFromGuide guide)) ixs) $
            \ idxLen idxPtr ->
                act idxPtr (fromIntegral idxLen)
    in  Priv.unop
            (\ptr -> withArgs $ FFI.constGEP ptr)
            (\bldPtr ptr cstr ->
                withArgs $ \idxPtr idxLen ->
                    FFI.buildGEP bldPtr ptr idxPtr idxLen cstr)
            vptr


-- | Truncate a value to a shorter bit width.
trunc ::
    (ValueCons value, IsInteger av, IsInteger bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     IsSized a, IsSized b, SizeOf a :>: SizeOf b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
trunc = convert FFI.constTrunc FFI.buildTrunc

isSigned :: (IsArithmetic a) => Guide shape a -> Bool
isSigned = Type.isSigned . proxyFromGuide

-- | Extend a value to wider width.
-- If the target type is signed, then preserve the sign,
-- If the target type is unsigned, then extended by zeros.
ext ::
    (ValueCons value, IsInteger a, IsInteger b, IsType bv, Signed a ~ Signed b,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     IsSized a, IsSized b, SizeOf a :<: SizeOf b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
ext guide =
   if isSigned (fmap snd guide)
     then convert FFI.constSExt FFI.buildSExt guide
     else convert FFI.constZExt FFI.buildZExt guide

extBool ::
    (ValueCons value, IsInteger b, IsType bv,
     IsPrimitive b, Type shape Bool ~ av, Type shape b ~ bv) =>
    Guide shape (Bool,b) -> value av -> CodeGenFunction r (value bv)
extBool guide =
   if isSigned (fmap snd guide)
     then convert FFI.constSExt FFI.buildSExt guide
     else convert FFI.constZExt FFI.buildZExt guide


compareGuideSizes :: (IsType a, IsType b) => Guide shape (a,b) -> Ordering
compareGuideSizes guide =
   case FuncHT.unzip $ proxyFromGuide guide of
      (a,b) -> compare (sizeOf (typeDesc a)) (sizeOf (typeDesc b))

-- | It is 'zext', 'trunc' or nop depending on the relation of the sizes.
zadapt ::
    (ValueCons value, IsInteger a, IsInteger b, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
zadapt guide =
   case compareGuideSizes guide of
      LT -> convert FFI.constZExt FFI.buildZExt guide
      EQ -> convert FFI.constBitCast FFI.buildBitCast guide
      GT -> convert FFI.constTrunc FFI.buildTrunc guide

-- | It is 'sext', 'trunc' or nop depending on the relation of the sizes.
sadapt ::
    (ValueCons value, IsInteger a, IsInteger b, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
sadapt guide =
   case compareGuideSizes guide of
      LT -> convert FFI.constSExt FFI.buildSExt guide
      EQ -> convert FFI.constBitCast FFI.buildBitCast guide
      GT -> convert FFI.constTrunc FFI.buildTrunc guide

-- | It is 'sadapt' or 'zadapt' depending on the sign mode.
adapt ::
    (ValueCons value, IsInteger a, IsInteger b, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     Signed a ~ Signed b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
adapt guide =
   case compareGuideSizes guide of
      LT ->
         if isSigned (fmap snd guide)
           then convert FFI.constSExt FFI.buildSExt guide
           else convert FFI.constZExt FFI.buildZExt guide
      EQ -> convert FFI.constBitCast FFI.buildBitCast guide
      GT -> convert FFI.constTrunc FFI.buildTrunc guide

-- | Truncate a floating point value.
fptrunc ::
    (ValueCons value, IsFloating av, IsFloating bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     IsSized a, IsSized b, SizeOf a :>: SizeOf b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
fptrunc = convert FFI.constFPTrunc FFI.buildFPTrunc

-- | Extend a floating point value.
fpext ::
    (ValueCons value, IsFloating av, IsFloating bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     IsSized a, IsSized b, SizeOf a :<: SizeOf b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
fpext = convert FFI.constFPExt FFI.buildFPExt

-- | Convert a floating point value to an integer.
-- It is mapped to @fptosi@ or @fptoui@ depending on the type @a@.
fptoint ::
    (ValueCons value, IsFloating a, IsInteger b, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
fptoint guide =
   if isSigned (fmap snd guide)
     then convert FFI.constFPToSI FFI.buildFPToSI guide
     else convert FFI.constFPToUI FFI.buildFPToUI guide


-- | Convert an integer to a floating point value.
-- It is mapped to @sitofp@ or @uitofp@ depending on the type @a@.
inttofp ::
    (ValueCons value, IsInteger a, IsFloating b, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
inttofp guide =
   if isSigned (fmap fst guide)
     then convert FFI.constSIToFP FFI.buildSIToFP guide
     else convert FFI.constUIToFP FFI.buildUIToFP guide


-- | Convert a pointer to an integer.
ptrtoint ::
    (ValueCons value, IsType a, IsInteger b, IsType bv,
     IsPrimitive b, Type shape (Ptr a) ~ av, Type shape b ~ bv) =>
    Guide shape (Ptr a, b) -> value av -> CodeGenFunction r (value bv)
ptrtoint = convert FFI.constPtrToInt FFI.buildPtrToInt

-- | Convert an integer to a pointer.
inttoptr ::
    (ValueCons value, IsInteger a, IsType b, IsType bv,
     IsPrimitive a, Type shape a ~ av, Type shape (Ptr b) ~ bv) =>
    Guide shape (a, Ptr b) -> value av -> CodeGenFunction r (value bv)
inttoptr = convert FFI.constIntToPtr FFI.buildIntToPtr

-- | Convert between to values of the same size by just copying the bit pattern.
bitcast ::
    (ValueCons value, IsFirstClass a, IsFirstClass bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv,
     IsSized a, IsSized b, SizeOf a ~ SizeOf b) =>
    Guide shape (a,b) -> value av -> CodeGenFunction r (value bv)
bitcast = convert FFI.constBitCast FFI.buildBitCast


convert ::
    (ValueCons value, IsType bv,
     IsPrimitive a, IsPrimitive b, Type shape a ~ av, Type shape b ~ bv) =>
    Priv.FFIConstConvert -> Priv.FFIConvert -> Guide shape (a,b) ->
    value av -> CodeGenFunction r (value bv)
convert cnvConst cnv Guide = Priv.convert cnvConst cnv



select ::
    (ValueCons value, IsPrimitive a,
     Type shape a ~ av, Type shape Bool ~ bv) =>
    Guide shape a ->
    value bv -> value av -> value av -> CodeGenFunction r (value av)
select Guide = Priv.trinop FFI.constSelect FFI.buildSelect


cmp ::
    (ValueCons value, IsArithmetic a, IsPrimitive a,
     Type shape a ~ av, Type shape Bool ~ bv) =>
    Guide shape a ->
    Priv.CmpPredicate -> value av -> value av -> CodeGenFunction r (value bv)
cmp guide@Guide p =
    let cmpop constCmp buildCmp predi =
            Priv.binop (constCmp predi) (flip buildCmp predi)
    in  if isFloating (proxyFromGuide guide)
          then
            cmpop FFI.constFCmp FFI.buildFCmp $
            FFI.fromRealPredicate $ Priv.fpFromCmpPredicate p
          else
            cmpop FFI.constICmp FFI.buildICmp $
            FFI.fromIntPredicate $
            if isSigned guide
              then Priv.sintFromCmpPredicate p
              else Priv.uintFromCmpPredicate p

_cmp ::
    (ValueCons value, IsArithmetic a, IsPrimitive a,
     Type shape a ~ av, Type shape Bool ~ bv) =>
    Guide shape a ->
    Priv.CmpPredicate -> value av -> value av -> CodeGenFunction r (value bv)
_cmp guide@Guide p =
    if isFloating (proxyFromGuide guide)
      then
        let predi = FFI.fromRealPredicate $ Priv.fpFromCmpPredicate p
        in  Priv.binop
                (FFI.constFCmp predi)
                (flip FFI.buildFCmp predi)
      else
        let predi =
              FFI.fromIntPredicate $
              if isSigned guide
                then Priv.sintFromCmpPredicate p
                else Priv.uintFromCmpPredicate p
        in  Priv.binop
                (FFI.constICmp predi)
                (flip FFI.buildICmp predi)

{-# DEPRECATED icmp "use cmp or pcmp instead" #-}
-- | Compare integers.
icmp ::
    (ValueCons value, IsIntegerOrPointer a, IsPrimitive a,
     Type shape a ~ av, Type shape Bool ~ bv) =>
    Guide shape a ->
    FFI.IntPredicate -> value av -> value av -> CodeGenFunction r (value bv)
icmp Guide p =
    Priv.binop
        (FFI.constICmp (FFI.fromIntPredicate p))
        (flip FFI.buildICmp (FFI.fromIntPredicate p))

-- | Compare pointers.
pcmp :: (ValueCons value, Type shape (Ptr a) ~ av, Type shape Bool ~ bv) =>
    Guide shape (Ptr a) ->
    FFI.IntPredicate -> value av -> value av -> CodeGenFunction r (value bv)
pcmp Guide p =
    Priv.binop
        (FFI.constICmp (FFI.fromIntPredicate p))
        (flip FFI.buildICmp (FFI.fromIntPredicate p))

-- | Compare floating point values.
fcmp ::
    (ValueCons value, IsFloating a, IsPrimitive a,
     Type shape a ~ av, Type shape Bool ~ bv) =>
    Guide shape a ->
    FFI.FPPredicate -> value av -> value av -> CodeGenFunction r (value bv)
fcmp Guide p =
    Priv.binop
        (FFI.constFCmp (FFI.fromRealPredicate p))
        (flip FFI.buildFCmp (FFI.fromRealPredicate p))
