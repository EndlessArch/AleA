{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module LLVM.Core.Instructions.Private where

import qualified LLVM.Core.Util as U
import qualified LLVM.Core.Proxy as LP
import LLVM.Core.Type (IsType, IsPrimitive, typeRef)
import LLVM.Core.Data (Vector, Array, Struct, PackedStruct)
import LLVM.Core.CodeGenMonad (CodeGenFunction)
import LLVM.Core.CodeGen
            (ConstValue(ConstValue), constOf, Value(Value), withCurrentBuilder)

import qualified LLVM.FFI.Core as FFI
import LLVM.FFI.Core (IntPredicate(..), FPPredicate(..))

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Data.Num.Decimal.Number (Pred)
import Type.Base.Proxy (Proxy)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)

import Data.Typeable (Typeable)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64, Word)



type FFIConstConvert = FFI.ValueRef -> FFI.TypeRef -> IO FFI.ValueRef
type FFIConvert =
        FFI.BuilderRef -> FFI.ValueRef -> FFI.TypeRef ->
        U.CString -> IO FFI.ValueRef

type FFIConstUnOp = FFI.ValueRef -> IO FFI.ValueRef
type FFIUnOp = FFI.BuilderRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef

type FFIConstBinOp = FFI.ValueRef -> FFI.ValueRef -> IO FFI.ValueRef
type FFIBinOp =
        FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef ->
        U.CString -> IO FFI.ValueRef

type FFIConstTrinOp =
        FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef -> IO FFI.ValueRef
type FFITrinOp =
        FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef ->
        U.CString -> IO FFI.ValueRef


class ValueCons value where
    switchValueCons :: f ConstValue -> f Value -> f value

instance ValueCons ConstValue where
    switchValueCons f _ = f

instance ValueCons Value where
    switchValueCons _ f = f


convert :: (ValueCons value, IsType b) =>
    FFIConstConvert -> FFIConvert -> value a -> CodeGenFunction r (value b)
convert cop op =
    getUnOp $
    switchValueCons
        (UnOp $ convertConstValue LP.Proxy cop)
        (UnOp $ convertValue LP.Proxy op)

convertConstValue ::
    (IsType b) =>
    LP.Proxy b -> FFIConstConvert ->
    ConstValue a -> CodeGenFunction r (ConstValue b)
convertConstValue proxy conv (ConstValue a) =
    liftM ConstValue $ liftIO $ conv a =<< typeRef proxy

convertValue ::
    (IsType b) =>
    LP.Proxy b -> FFIConvert -> Value a -> CodeGenFunction r (Value b)
convertValue proxy conv (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr -> do
      typ <- typeRef proxy
      U.withEmptyCString $ conv bldPtr a typ


newtype UnValue a value = UnValue {getUnValue :: value a -> FFI.ValueRef}

unValue :: (ValueCons value) => value a -> FFI.ValueRef
unValue =
    getUnValue $
    switchValueCons
        (UnValue $ \(ConstValue a) -> a)
        (UnValue $ \(Value a) -> a)

newtype UnOp a b r value =
    UnOp {getUnOp :: value a -> CodeGenFunction r (value b)}

unop ::
    (ValueCons value) =>
    FFIConstUnOp -> FFIUnOp -> value a -> CodeGenFunction r (value b)
unop cop op =
    getUnOp $
    switchValueCons
        (UnOp $ \(ConstValue a) -> liftIO $ fmap ConstValue $ cop a)
        (UnOp $ \(Value a) ->
            liftM Value $
            withCurrentBuilder $ \ bld ->
                U.withEmptyCString $ op bld a)

newtype BinOp a b c r value =
    BinOp {getBinOp :: value a -> value b -> CodeGenFunction r (value c)}

binop ::
    (ValueCons value) =>
    FFIConstBinOp -> FFIBinOp ->
    value a -> value b -> CodeGenFunction r (value c)
binop cop op =
    getBinOp $
    switchValueCons
        (BinOp $ \(ConstValue a) (ConstValue b) ->
            liftIO $ fmap ConstValue $ cop a b)
        (BinOp $ \(Value a) (Value b) ->
            liftM Value $
            withCurrentBuilder $ \ bld ->
                U.withEmptyCString $ op bld a b)

newtype TrinOp a b c d r value =
    TrinOp {
        getTrinOp ::
            value a -> value b -> value c -> CodeGenFunction r (value d)
    }

trinop ::
    (ValueCons value) =>
    FFIConstTrinOp -> FFITrinOp ->
    value a -> value b -> value c -> CodeGenFunction r (value d)
trinop cop op =
    getTrinOp $
    switchValueCons
        (TrinOp $ \(ConstValue a) (ConstValue b) (ConstValue c) ->
            liftIO $ fmap ConstValue $ cop a b c)
        (TrinOp $ \(Value a) (Value b) (Value c) ->
            liftM Value $
            withCurrentBuilder $ \ bld ->
                U.withEmptyCString $ op bld a b c)



-- | Acceptable arguments to 'getElementPointer'.
class GetElementPtr optr ixs where
    type ElementPtrType optr ixs
    getIxList :: LP.Proxy optr -> ixs -> [FFI.ValueRef]

-- | Acceptable single index to 'getElementPointer'.
class IsIndexArg a where
    getArg :: a -> FFI.ValueRef

{- |
In principle we do not need the getValueArg method,
because we could just use 'unValue'.
However, we want to prevent users
from defining their own (disfunctional) IsIndexType instances.
-}
class (IsPrimitive i) => IsIndexType i where
    getValueArg :: (ValueCons value) => value i -> FFI.ValueRef

instance IsIndexType Word where
    getValueArg = unValue

instance IsIndexType Word32 where
    getValueArg = unValue

instance IsIndexType Word64 where
    getValueArg = unValue

instance IsIndexType Int where
    getValueArg = unValue

instance IsIndexType Int32 where
    getValueArg = unValue

instance IsIndexType Int64 where
    getValueArg = unValue

instance IsIndexType i => IsIndexArg (ConstValue i) where
    getArg = getValueArg

instance IsIndexType i => IsIndexArg (Value i) where
    getArg = getValueArg

instance IsIndexArg Word where
    getArg = unConst . constOf

instance IsIndexArg Word32 where
    getArg = unConst . constOf

instance IsIndexArg Word64 where
    getArg = unConst . constOf

instance IsIndexArg Int where
    getArg = unConst . constOf

instance IsIndexArg Int32 where
    getArg = unConst . constOf

instance IsIndexArg Int64 where
    getArg = unConst . constOf

unConst :: ConstValue a -> FFI.ValueRef
unConst (ConstValue v) = v

-- End of indexing
instance GetElementPtr a () where
    type ElementPtrType a () = a
    getIxList LP.Proxy () = []

-- Index in Array
instance
    (GetElementPtr o i, IsIndexArg a, Dec.Natural k) =>
        GetElementPtr (Array k o) (a, i) where
    type ElementPtrType (Array k o) (a, i) = ElementPtrType o i
    getIxList proxy (v, i) = getArg v : getIxList (LP.element proxy) i

-- Index in Vector
instance
    (GetElementPtr o i, IsIndexArg a, Dec.Positive k) =>
        GetElementPtr (Vector k o) (a, i) where
    type ElementPtrType (Vector k o) (a, i) = ElementPtrType o i
    getIxList proxy (v, i) = getArg v : getIxList (LP.element proxy) i

fieldProxy :: LP.Proxy (struct fs) -> Proxy a -> LP.Proxy (FieldType fs a)
fieldProxy LP.Proxy _proxy = LP.Proxy

-- Index in Struct and PackedStruct.
-- The index has to be a type level integer to statically determine the record field type
instance
    (GetElementPtr (FieldType fs a) i, Dec.Natural a) =>
        GetElementPtr (Struct fs) (Proxy a, i) where
    type ElementPtrType (Struct fs) (Proxy a, i) =
            ElementPtrType (FieldType fs a) i
    getIxList proxy (a, i) =
        unConst (constOf (Dec.integralFromProxy a :: Word32)) :
        getIxList (fieldProxy proxy a) i
instance
    (GetElementPtr (FieldType fs a) i, Dec.Natural a) =>
        GetElementPtr (PackedStruct fs) (Proxy a, i) where
    type ElementPtrType (PackedStruct fs) (Proxy a, i) =
            ElementPtrType (FieldType fs a) i
    getIxList proxy (a, i) =
        unConst (constOf (Dec.integralFromProxy a :: Word32)) :
        getIxList (fieldProxy proxy a) i

class GetField as i where type FieldType as i
instance GetField (a, as) Dec.Zero where
    type FieldType (a, as) Dec.Zero = a
instance
    (GetField as (Pred (Dec.Pos i0 i1))) =>
        GetField (a, as) (Dec.Pos i0 i1) where
    type FieldType (a,as) (Dec.Pos i0 i1) = FieldType as (Pred (Dec.Pos i0 i1))



data CmpPredicate =
    CmpEQ                       -- ^ equal
  | CmpNE                       -- ^ not equal
  | CmpGT                       -- ^ greater than
  | CmpGE                       -- ^ greater or equal
  | CmpLT                       -- ^ less than
  | CmpLE                       -- ^ less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

uintFromCmpPredicate :: CmpPredicate -> IntPredicate
uintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntUGT
      CmpGE -> IntUGE
      CmpLT -> IntULT
      CmpLE -> IntULE

sintFromCmpPredicate :: CmpPredicate -> IntPredicate
sintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntSGT
      CmpGE -> IntSGE
      CmpLT -> IntSLT
      CmpLE -> IntSLE

fpFromCmpPredicate :: CmpPredicate -> FPPredicate
fpFromCmpPredicate p =
   case p of
      CmpEQ -> FPOEQ
      CmpNE -> FPONE
      CmpGT -> FPOGT
      CmpGE -> FPOGE
      CmpLT -> FPOLT
      CmpLE -> FPOLE
