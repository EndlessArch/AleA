{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Core.Instructions(
    -- * ADT representation of IR
    BinOpDesc(..), InstrDesc(..), ArgDesc(..), getInstrDesc,
    -- * Terminator instructions
    ret,
    condBr,
    br,
    switch,
    invoke, invokeWithConv,
    invokeFromFunction, invokeWithConvFromFunction,
    unreachable,
    -- * Arithmetic binary operations
    -- | Arithmetic operations with the normal semantics.
    -- The u instructions are unsigned, the s instructions are signed.
    add, sub, mul, neg,
    iadd, isub, imul, ineg,
    iaddNoWrap, isubNoWrap, imulNoWrap, inegNoWrap,
    fadd, fsub, fmul, fneg,
    idiv, irem,
    udiv, sdiv, fdiv, urem, srem, frem,
    -- * Logical binary operations
    -- |Logical instructions with the normal semantics.
    shl, shr, lshr, ashr, and, or, xor, inv,
    -- * Vector operations
    extractelement,
    insertelement,
    shufflevector,
    -- * Aggregate operation
    extractvalue,
    insertvalue,
    -- * Memory access
    malloc, arrayMalloc,
    alloca, arrayAlloca,
    free,
    load,
    store,
    getElementPtr, getElementPtr0,
    -- * Conversions
    ValueCons,
    trunc, zext, sext, ext, zadapt, sadapt, adapt,
    fptrunc, fpext,
    fptoui, fptosi, fptoint,
    uitofp, sitofp, inttofp,
    ptrtoint, inttoptr,
    bitcast,
    -- * Comparison
    CmpPredicate(..), IntPredicate(..), FPPredicate(..),
    CmpRet, CmpResult, CmpValueResult,
    cmp, pcmp, icmp, fcmp,
    select,
    -- * Fast math
    setHasNoNaNs,
    setHasNoInfs,
    setHasNoSignedZeros,
    setHasAllowReciprocal,
    setFastMath,
    -- * Other
    phi, addPhiInputs,
    call, callWithConv,
    callFromFunction, callWithConvFromFunction,
    Call, applyCall, runCall,

    -- * Classes and types
    ValueCons2, BinOpValue,
    Terminate, Ret, Result, CallArgs,
    CodeGen.FunctionArgs, CodeGen.FunctionCodeGen, CodeGen.FunctionResult,
    AllocArg,
    GetElementPtr, ElementPtrType, IsIndexArg, IsIndexType,
    GetValue, ValueType,
    GetField, FieldType,
    ) where

import qualified LLVM.Core.Util as U
import qualified LLVM.Core.Proxy as LP
import qualified LLVM.Core.CodeGen as CodeGen
import LLVM.Core.Instructions.Private
            (ValueCons, unValue, convert, unop,
             FFIBinOp, FFIConstBinOp,
             GetField, FieldType, GetElementPtr, ElementPtrType,
             IsIndexArg, IsIndexType, getIxList, getArg,
             CmpPredicate(..),
             uintFromCmpPredicate, sintFromCmpPredicate, fpFromCmpPredicate)
import LLVM.Core.Data
import LLVM.Core.Type
import LLVM.Core.CodeGenMonad
import LLVM.Core.CodeGen
            (BasicBlock(BasicBlock), Function, withCurrentBuilder,
             ConstValue(ConstValue), zero,
             Value(Value), value, valueOf, UnValue, CodeResult)

import qualified LLVM.FFI.Core as FFI
import LLVM.FFI.Core (IntPredicate(..), FPPredicate(..))

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Data.Num.Decimal.Literal (d1)
import Type.Data.Num.Decimal.Number ((:<:), (:>:))
import Type.Base.Proxy (Proxy)

import qualified Foreign
import Foreign.Ptr (FunPtr)
import Foreign.C (CUInt, CInt)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)

import Prelude hiding (and, or)


-- TODO:
-- Add vector version of arithmetic
-- Add rest of instructions
-- Use Terminate to ensure bb termination (how?)
-- more intrinsics are needed to, e.g., create an empty vector

data ArgDesc = AV String | AI Int | AL String | AE

instance Show ArgDesc where
    -- show (AV s) = "V_" ++ s
    -- show (AI i) = "I_" ++ show i
    -- show (AL l) = "L_" ++ l
    show (AV s) = s
    show (AI i) = show i
    show (AL l) = l
    show AE = "voidarg?"

data BinOpDesc = BOAdd | BOAddNuw | BOAddNsw | BOAddNuwNsw | BOFAdd
               | BOSub | BOSubNuw | BOSubNsw | BOSubNuwNsw | BOFSub
               | BOMul | BOMulNuw | BOMulNsw | BOMulNuwNsw | BOFMul
               | BOUDiv | BOSDiv | BOSDivExact | BOFDiv | BOURem | BOSRem | BOFRem
               | BOShL | BOLShR | BOAShR | BOAnd | BOOr | BOXor
    deriving Show

-- FIXME: complete definitions for unimplemented instructions
data InstrDesc =
    -- terminators
    IDRet TypeDesc ArgDesc | IDRetVoid
  | IDBrCond ArgDesc ArgDesc ArgDesc | IDBrUncond ArgDesc
  | IDSwitch [(ArgDesc, ArgDesc)]
  | IDIndirectBr
  | IDInvoke
  | IDUnwind
  | IDUnreachable
    -- binary operators (including bitwise)
  | IDBinOp BinOpDesc TypeDesc ArgDesc ArgDesc
    -- memory access and addressing
  | IDAlloca TypeDesc Int Int | IDLoad TypeDesc ArgDesc | IDStore TypeDesc ArgDesc ArgDesc
  | IDGetElementPtr TypeDesc [ArgDesc]
    -- conversion
  | IDTrunc TypeDesc TypeDesc ArgDesc | IDZExt TypeDesc TypeDesc ArgDesc
  | IDSExt TypeDesc TypeDesc ArgDesc | IDFPtoUI TypeDesc TypeDesc ArgDesc
  | IDFPtoSI TypeDesc TypeDesc ArgDesc | IDUItoFP TypeDesc TypeDesc ArgDesc
  | IDSItoFP TypeDesc TypeDesc ArgDesc
  | IDFPTrunc TypeDesc TypeDesc ArgDesc | IDFPExt TypeDesc TypeDesc ArgDesc
  | IDPtrToInt TypeDesc TypeDesc ArgDesc | IDIntToPtr TypeDesc TypeDesc ArgDesc
  | IDBitcast TypeDesc TypeDesc ArgDesc
    -- other
  | IDICmp IntPredicate ArgDesc ArgDesc | IDFCmp FPPredicate ArgDesc ArgDesc
  | IDPhi TypeDesc [(ArgDesc, ArgDesc)] | IDCall TypeDesc ArgDesc [ArgDesc]
  | IDSelect TypeDesc ArgDesc ArgDesc | IDUserOp1 | IDUserOp2 | IDVAArg
    -- vector operators
  | IDExtractElement | IDInsertElement | IDShuffleVector
    -- aggregate operators
  | IDExtractValue | IDInsertValue
    -- invalid
  | IDInvalidOp
    deriving Show

-- TODO: overflow support for binary operations (add/sub/mul)
getInstrDesc :: FFI.ValueRef -> IO (String, InstrDesc)
getInstrDesc v = do
    valueName <- U.getValueNameU v
    opcode <- FFI.instGetOpcode v
    t <- FFI.typeOf v >>= typeDesc2
    -- FIXME: sizeof() does not work for types!
    --tsize <- FFI.typeOf v -- >>= FFI.sizeOf -- >>= FFI.constIntGetZExtValue >>= return . fromIntegral
    tsize <- return 1
    ovs <- U.getOperands v
    os <- mapM getArgDesc ovs
    os0 <- return $ case os of {o:_   -> o; _ -> AE}
    os1 <- return $ case os of {_:o:_ -> o; _ -> AE}
    instr <-
        case Map.lookup opcode binOpMap of -- binary arithmetic
          Just op -> return $ IDBinOp op t os0 os1
          Nothing ->
            case Map.lookup opcode convOpMap of
              Just op -> do
                t2 <-
                    case ovs of
                        (_name,ov):_ -> FFI.typeOf ov >>= typeDesc2
                        _ -> return TDVoid
                return $ op t2 t os0
              Nothing ->
                case opcode of
                  1 -> return $ if null os then IDRetVoid else IDRet t os0
                  2 -> return $ if length os == 1 then IDBrUncond os0 else IDBrCond os0 (os !! 2) os1
                  3 -> return $ IDSwitch $ toPairs os
                  -- TODO (can skip for now)
                  -- 4 -> return IndirectBr ; 5 -> return Invoke
                  6 -> return IDUnwind; 7 -> return IDUnreachable
                  26 -> return $ IDAlloca (getPtrType t) tsize (getImmInt os0)
                  27 -> return $ IDLoad t os0; 28 -> return $ IDStore t os0 os1
                  29 -> return $ IDGetElementPtr t os
                  42 -> do
                      pInt <- FFI.cmpInstGetIntPredicate v
                      return $ IDICmp (FFI.toIntPredicate pInt) os0 os1
                  43 -> do
                      pFloat <- FFI.cmpInstGetRealPredicate v
                      return $ IDFCmp (FFI.toRealPredicate pFloat) os0 os1
                  44 -> return $ IDPhi t $ toPairs os
                  -- FIXME: getelementptr arguments are not handled
                  45 -> return $ IDCall t (last os) (init os)
                  46 -> return $ IDSelect t os0 os1
                  -- TODO (can skip for now)
                  -- 47 -> return UserOp1 ; 48 -> return UserOp2 ; 49 -> return VAArg
                  -- 50 -> return ExtractElement ; 51 -> return InsertElement ; 52 -> return ShuffleVector
                  -- 53 -> return ExtractValue ; 54 -> return InsertValue
                  _ -> return IDInvalidOp
    return (valueName, instr)
    --if instr /= InvalidOp then return instr else fail $ "Invalid opcode: " ++ show opcode
        where toPairs xs = zip (stride 2 xs) (stride 2 (drop 1 xs))
              stride _ [] = []
              stride n (x:xs) = x : stride n (drop (n-1) xs)
              getPtrType (TDPtr t) = t
              getPtrType _ = TDVoid
              getImmInt (AI i) = i
              getImmInt _ = 0

binOpMap :: Map CInt BinOpDesc
binOpMap =
    Map.fromList
        [(8, BOAdd), (9, BOFAdd), (10, BOSub), (11, BOFSub),
         (12, BOMul), (13, BOFMul), (14, BOUDiv), (15, BOSDiv),
         (16, BOFDiv), (17, BOURem), (18, BOSRem), (19, BOFRem),
         (20, BOShL), (21, BOLShR), (22, BOAShR), (23, BOAnd),
         (24, BOOr), (25, BOXor)]

convOpMap :: Map CInt (TypeDesc -> TypeDesc -> ArgDesc -> InstrDesc)
convOpMap =
    Map.fromList
        [(30, IDTrunc), (31, IDZExt), (32, IDSExt), (33, IDFPtoUI),
         (34, IDFPtoSI), (35, IDUItoFP), (36, IDSItoFP), (37, IDFPTrunc),
         (38, IDFPExt), (39, IDPtrToInt), (40, IDIntToPtr), (41, IDBitcast)]

-- TODO: fix for non-int constants
getArgDesc :: (String, FFI.ValueRef) -> IO ArgDesc
getArgDesc (vname, v) = do
    isC <- U.isConstant v
    t <- FFI.typeOf v >>= typeDesc2
    if isC
      then case t of
             TDInt _ _ -> do
                          cV <- FFI.constIntGetSExtValue v
                          return $ AI $ fromIntegral cV
             _ -> return AE
      else case t of
             TDLabel -> return $ AL vname
             _ -> return $ AV vname

--------------------------------------

type Terminate = ()
terminate :: Terminate
terminate = ()

--------------------------------------

-- |Acceptable arguments to the 'ret' instruction.
class Ret a where
    type Result a
    ret' :: a -> CodeGenFunction (Result a) Terminate

-- | Return from the current function with the given value.  Use () as the return value for what would be a void function in C.
ret :: (Ret a) => a -> CodeGenFunction (Result a) Terminate
ret = ret'

-- overlaps with Ret () ()!
{-
instance (IsFirstClass a, IsConst a) => Ret a a where
    ret' = ret . valueOf
-}

instance Ret (Value a) where
    type Result (Value a) = a
    ret' (Value a) = do
        withCurrentBuilder_ $ \ bldPtr -> FFI.buildRet bldPtr a
        return terminate

instance Ret () where
    type Result () = ()
    ret' _ = do
        withCurrentBuilder_ $ FFI.buildRetVoid
        return terminate

withCurrentBuilder_ :: (FFI.BuilderRef -> IO a) -> CodeGenFunction r ()
withCurrentBuilder_ p = withCurrentBuilder p >> return ()

--------------------------------------

-- | Branch to the first basic block if the boolean is true, otherwise to the second basic block.
condBr :: Value Bool -- ^ Boolean to branch upon.
       -> BasicBlock -- ^ Target for true.
       -> BasicBlock -- ^ Target for false.
       -> CodeGenFunction r Terminate
condBr (Value b) (BasicBlock t1) (BasicBlock t2) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildCondBr bldPtr b t1 t2
    return terminate

--------------------------------------

-- | Unconditionally branch to the given basic block.
br :: BasicBlock  -- ^ Branch target.
   -> CodeGenFunction r Terminate
br (BasicBlock t) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildBr bldPtr t
    return terminate

--------------------------------------

-- | Branch table instruction.
switch :: (IsInteger a)
       => Value a                        -- ^ Value to branch upon.
       -> BasicBlock                     -- ^ Default branch target.
       -> [(ConstValue a, BasicBlock)]   -- ^ Labels and corresponding branch targets.
       -> CodeGenFunction r Terminate
switch (Value val) (BasicBlock dflt) arms = do
    withCurrentBuilder_ $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst c b | (ConstValue c, BasicBlock b) <- arms ]
    return terminate

--------------------------------------

-- |Inform the code generator that this code can never be reached.
unreachable :: CodeGenFunction r Terminate
unreachable = do
    withCurrentBuilder_ FFI.buildUnreachable
    return terminate

--------------------------------------


withArithmeticType ::
    (IsArithmetic c) =>
    (ArithmeticType c -> a -> CodeGenFunction r (v c)) ->
    (a -> CodeGenFunction r (v c))
withArithmeticType f = f arithmeticType


class (ValueCons value0, ValueCons value1) => ValueCons2 value0 value1 where
    type BinOpValue (value0 :: * -> *) (value1 :: * -> *) :: * -> *
    binop ::
        FFIConstBinOp -> FFIBinOp ->
        value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 b)

instance ValueCons2 Value Value where
    type BinOpValue Value Value = Value
    binop _ op (Value a1) (Value a2) = buildBinOp op a1 a2

instance ValueCons2 Value ConstValue where
    type BinOpValue Value ConstValue = Value
    binop _ op (Value a1) (ConstValue a2) = buildBinOp op a1 a2

instance ValueCons2 ConstValue Value where
    type BinOpValue ConstValue Value = Value
    binop _ op (ConstValue a1) (Value a2) = buildBinOp op a1 a2

instance ValueCons2 ConstValue ConstValue where
    type BinOpValue ConstValue ConstValue = ConstValue
    binop cop _ (ConstValue a1) (ConstValue a2) =
        liftIO $ fmap ConstValue $ cop a1 a2


add, sub, mul ::
    (ValueCons2 value0 value1, IsArithmetic a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
add =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> binop FFI.constAdd  FFI.buildAdd
      FloatingType -> binop FFI.constFAdd FFI.buildFAdd

sub =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> binop FFI.constSub  FFI.buildSub
      FloatingType -> binop FFI.constFSub FFI.buildFSub

mul =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> binop FFI.constMul  FFI.buildMul
      FloatingType -> binop FFI.constFMul FFI.buildFMul

iadd, isub, imul ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
iadd = binop FFI.constAdd FFI.buildAdd
isub = binop FFI.constSub FFI.buildSub
imul = binop FFI.constMul FFI.buildMul

iaddNoWrap, isubNoWrap, imulNoWrap ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
iaddNoWrap =
    sbinop FFI.constNSWAdd FFI.buildNSWAdd FFI.constNUWAdd FFI.buildNUWAdd
isubNoWrap =
    sbinop FFI.constNSWSub FFI.buildNSWSub FFI.constNUWSub FFI.buildNUWSub
imulNoWrap =
    sbinop FFI.constNSWMul FFI.buildNSWMul FFI.constNUWMul FFI.buildNUWMul

-- | signed or unsigned integer division depending on the type
idiv ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
idiv = sbinop FFI.constSDiv FFI.buildSDiv FFI.constUDiv FFI.buildUDiv
-- | signed or unsigned remainder depending on the type
irem ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
irem = sbinop FFI.constSRem FFI.buildSRem FFI.constURem FFI.buildURem

{-# DEPRECATED udiv "use idiv instead" #-}
{-# DEPRECATED sdiv "use idiv instead" #-}
{-# DEPRECATED urem "use irem instead" #-}
{-# DEPRECATED srem "use irem instead" #-}
udiv, sdiv, urem, srem ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
udiv = binop FFI.constUDiv FFI.buildUDiv
sdiv = binop FFI.constSDiv FFI.buildSDiv
urem = binop FFI.constURem FFI.buildURem
srem = binop FFI.constSRem FFI.buildSRem

fadd, fsub, fmul ::
    (ValueCons2 value0 value1, IsFloating a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
fadd = binop FFI.constFAdd FFI.buildFAdd
fsub = binop FFI.constFSub FFI.buildFSub
fmul = binop FFI.constFMul FFI.buildFMul

-- | Floating point division.
fdiv ::
    (ValueCons2 value0 value1, IsFloating a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
fdiv = binop FFI.constFDiv FFI.buildFDiv
-- | Floating point remainder.
frem ::
    (ValueCons2 value0 value1, IsFloating a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
frem = binop FFI.constFRem FFI.buildFRem

shl, lshr, ashr, and, or, xor ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
shl  = binop FFI.constShl  FFI.buildShl
lshr = binop FFI.constLShr FFI.buildLShr
ashr = binop FFI.constAShr FFI.buildAShr
and  = binop FFI.constAnd  FFI.buildAnd
or   = binop FFI.constOr   FFI.buildOr
xor  = binop FFI.constXor  FFI.buildXor

shr ::
    (ValueCons2 value0 value1, IsInteger a) =>
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 a)
shr = sbinop FFI.constAShr FFI.buildAShr FFI.constLShr FFI.buildLShr

sbinop ::
    forall value0 value1 a b r.
    (ValueCons2 value0 value1, IsInteger a) =>
    FFIConstBinOp -> FFIBinOp ->
    FFIConstBinOp -> FFIBinOp ->
    value0 a -> value1 a -> CodeGenFunction r (BinOpValue value0 value1 b)
sbinop scop sop ucop uop =
    if isSigned (LP.Proxy :: LP.Proxy a)
        then binop scop sop
        else binop ucop uop


buildBinOp ::
    FFIBinOp -> FFI.ValueRef -> FFI.ValueRef -> CodeGenFunction r (Value a)
buildBinOp op a1 a2 =
    liftM Value $
    withCurrentBuilder $ \ bld ->
      U.withEmptyCString $ op bld a1 a2

neg ::
    (ValueCons value, IsArithmetic a) =>
    value a -> CodeGenFunction r (value a)
neg =
    withArithmeticType $ \typ -> case typ of
      IntegerType  -> unop FFI.constNeg FFI.buildNeg
      FloatingType -> unop FFI.constFNeg FFI.buildFNeg

ineg ::
    (ValueCons value, IsInteger a) =>
    value a -> CodeGenFunction r (value a)
ineg = unop FFI.constNeg FFI.buildNeg

inegNoWrap ::
    forall value a r.
    (ValueCons value, IsInteger a) =>
    value a -> CodeGenFunction r (value a)
inegNoWrap =
   if isSigned (LP.Proxy :: LP.Proxy a)
     then unop FFI.constNSWNeg FFI.buildNSWNeg
     else unop FFI.constNUWNeg FFI.buildNUWNeg

fneg ::
    (ValueCons value, IsFloating a) =>
    value a -> CodeGenFunction r (value a)
fneg = unop FFI.constFNeg FFI.buildFNeg

inv ::
    (ValueCons value, IsInteger a) =>
    value a -> CodeGenFunction r (value a)
inv = unop FFI.constNot FFI.buildNot

--------------------------------------

-- | Get a value from a vector.
extractelement :: (Dec.Positive n, IsPrimitive a)
               => Value (Vector n a)               -- ^ Vector
               -> Value Word32                     -- ^ Index into the vector
               -> CodeGenFunction r (Value a)
extractelement (Value vec) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildExtractElement bldPtr vec i

-- | Insert a value into a vector, nondestructive.
insertelement :: (Dec.Positive n, IsPrimitive a)
              => Value (Vector n a)                -- ^ Vector
              -> Value a                           -- ^ Value to insert
              -> Value Word32                      -- ^ Index into the vector
              -> CodeGenFunction r (Value (Vector n a))
insertelement (Value vec) (Value e) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildInsertElement bldPtr vec e i

-- | Permute vector.
shufflevector :: (Dec.Positive n, Dec.Positive m, IsPrimitive a)
              => Value (Vector n a)
              -> Value (Vector n a)
              -> ConstValue (Vector m Word32)
              -> CodeGenFunction r (Value (Vector m a))
shufflevector (Value a) (Value b) (ConstValue mask) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildShuffleVector bldPtr a b mask


-- |Acceptable arguments to 'extractvalue' and 'insertvalue'.
class GetValue agg ix where
    type ValueType agg ix
    getIx :: proxy agg -> ix -> CUInt

instance (GetField as i, Dec.Natural i) => GetValue (Struct as) (Proxy i) where
    type ValueType (Struct as) (Proxy i) = FieldType as i
    getIx _ n = Dec.integralFromProxy n

instance (IsFirstClass a, Dec.Natural n) => GetValue (Array n a) Word where
    type ValueType (Array n a) Word = a
    getIx _ n = fromIntegral n

instance (IsFirstClass a, Dec.Natural n) => GetValue (Array n a) Word32 where
    type ValueType (Array n a) Word32 = a
    getIx _ n = fromIntegral n

instance (IsFirstClass a, Dec.Natural n) => GetValue (Array n a) Word64 where
    type ValueType (Array n a) Word64 = a
    getIx _ n = fromIntegral n


instance (IsFirstClass a, Dec.Natural n, Dec.Natural i, i :<: n) => GetValue (Array n a) (Proxy i) where
    type ValueType (Array n a) (Proxy i) = a
    getIx _ n = Dec.integralFromProxy n


-- | Get a value from an aggregate.
extractvalue :: forall r agg i.
                GetValue agg i
             => Value agg                   -- ^ Aggregate
             -> i                           -- ^ Index into the aggregate
             -> CodeGenFunction r (Value (ValueType agg i))
extractvalue v@(Value agg) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildExtractValue bldPtr agg (getIx v i)

-- | Insert a value into an aggregate, nondestructive.
insertvalue :: forall r agg i.
               GetValue agg i
            => Value agg                   -- ^ Aggregate
            -> Value (ValueType agg i)     -- ^ Value to insert
            -> i                           -- ^ Index into the aggregate
            -> CodeGenFunction r (Value agg)
insertvalue v@(Value agg) (Value e) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildInsertValue bldPtr agg e (getIx v i)


--------------------------------------

-- | Truncate a value to a shorter bit width.
trunc :: (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b, IsSized a, IsSized b, SizeOf a :>: SizeOf b)
      => value a -> CodeGenFunction r (value b)
trunc = convert FFI.constTrunc FFI.buildTrunc

-- | Zero extend a value to a wider width.
-- If possible, use 'ext' that chooses the right padding according to the types
zext :: (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b, IsSized a, IsSized b, SizeOf a :<: SizeOf b)
     => value a -> CodeGenFunction r (value b)
zext = convert FFI.constZExt FFI.buildZExt

-- | Sign extend a value to wider width.
-- If possible, use 'ext' that chooses the right padding according to the types
sext :: (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b, IsSized a, IsSized b, SizeOf a :<: SizeOf b)
     => value a -> CodeGenFunction r (value b)
sext = convert FFI.constSExt FFI.buildSExt

-- | Extend a value to wider width.
-- If the target type is signed, then preserve the sign,
-- If the target type is unsigned, then extended by zeros.
ext :: forall value a b r. (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b, Signed a ~ Signed b, IsSized a, IsSized b, SizeOf a :<: SizeOf b)
     => value a -> CodeGenFunction r (value b)
ext =
   if isSigned (LP.Proxy :: LP.Proxy b)
     then convert FFI.constSExt FFI.buildSExt
     else convert FFI.constZExt FFI.buildZExt

-- | It is 'zext', 'trunc' or nop depending on the relation of the sizes.
zadapt :: forall value a b r. (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b)
     => value a -> CodeGenFunction r (value b)
zadapt =
   case compare (sizeOf (typeDesc (LP.Proxy :: LP.Proxy a)))
                (sizeOf (typeDesc (LP.Proxy :: LP.Proxy b))) of
      LT -> convert FFI.constZExt FFI.buildZExt
      EQ -> convert FFI.constBitCast FFI.buildBitCast
      GT -> convert FFI.constTrunc FFI.buildTrunc

-- | It is 'sext', 'trunc' or nop depending on the relation of the sizes.
sadapt :: forall value a b r. (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b)
     => value a -> CodeGenFunction r (value b)
sadapt =
   case compare (sizeOf (typeDesc (LP.Proxy :: LP.Proxy a)))
                (sizeOf (typeDesc (LP.Proxy :: LP.Proxy b))) of
      LT -> convert FFI.constSExt FFI.buildSExt
      EQ -> convert FFI.constBitCast FFI.buildBitCast
      GT -> convert FFI.constTrunc FFI.buildTrunc

-- | It is 'sadapt' or 'zadapt' depending on the sign mode.
adapt :: forall value a b r. (ValueCons value, IsInteger a, IsInteger b, ShapeOf a ~ ShapeOf b, Signed a ~ Signed b)
     => value a -> CodeGenFunction r (value b)
adapt =
   case compare (sizeOf (typeDesc (LP.Proxy :: LP.Proxy a)))
                (sizeOf (typeDesc (LP.Proxy :: LP.Proxy b))) of
      LT ->
         if isSigned (LP.Proxy :: LP.Proxy b)
           then convert FFI.constSExt FFI.buildSExt
           else convert FFI.constZExt FFI.buildZExt
      EQ -> convert FFI.constBitCast FFI.buildBitCast
      GT -> convert FFI.constTrunc FFI.buildTrunc

-- | Truncate a floating point value.
fptrunc :: (ValueCons value, IsFloating a, IsFloating b, ShapeOf a ~ ShapeOf b, IsSized a, IsSized b, SizeOf a :>: SizeOf b)
        => value a -> CodeGenFunction r (value b)
fptrunc = convert FFI.constFPTrunc FFI.buildFPTrunc

-- | Extend a floating point value.
fpext :: (ValueCons value, IsFloating a, IsFloating b, ShapeOf a ~ ShapeOf b, IsSized a, IsSized b, SizeOf a :<: SizeOf b)
      => value a -> CodeGenFunction r (value b)
fpext = convert FFI.constFPExt FFI.buildFPExt

{-# DEPRECATED fptoui "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to an unsigned integer.
fptoui :: (ValueCons value, IsFloating a, IsInteger b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
fptoui = convert FFI.constFPToUI FFI.buildFPToUI

{-# DEPRECATED fptosi "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to a signed integer.
fptosi :: (ValueCons value, IsFloating a, IsInteger b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
fptosi = convert FFI.constFPToSI FFI.buildFPToSI

-- | Convert a floating point value to an integer.
-- It is mapped to @fptosi@ or @fptoui@ depending on the type @a@.
fptoint :: forall value a b r. (ValueCons value, IsFloating a, IsInteger b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
fptoint =
   if isSigned (LP.Proxy :: LP.Proxy b)
     then convert FFI.constFPToSI FFI.buildFPToSI
     else convert FFI.constFPToUI FFI.buildFPToUI


{- DEPRECATED uitofp "use inttofp since it is type-safe with respect to signs" -}
-- | Convert an unsigned integer to a floating point value.
-- Although 'inttofp' should be prefered, this function may be useful for conversion from Bool.
uitofp :: (ValueCons value, IsInteger a, IsFloating b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
uitofp = convert FFI.constUIToFP FFI.buildUIToFP

{- DEPRECATED sitofp "use inttofp since it is type-safe with respect to signs" -}
-- | Convert a signed integer to a floating point value.
-- Although 'inttofp' should be prefered, this function may be useful for conversion from Bool.
sitofp :: (ValueCons value, IsInteger a, IsFloating b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
sitofp = convert FFI.constSIToFP FFI.buildSIToFP

-- | Convert an integer to a floating point value.
-- It is mapped to @sitofp@ or @uitofp@ depending on the type @a@.
inttofp :: forall value a b r. (ValueCons value, IsInteger a, IsFloating b, ShapeOf a ~ ShapeOf b) => value a -> CodeGenFunction r (value b)
inttofp =
   if isSigned (LP.Proxy :: LP.Proxy a)
     then convert FFI.constSIToFP FFI.buildSIToFP
     else convert FFI.constUIToFP FFI.buildUIToFP


-- | Convert a pointer to an integer.
ptrtoint :: (ValueCons value, IsInteger b, IsPrimitive b) => value (Ptr a) -> CodeGenFunction r (value b)
ptrtoint = convert FFI.constPtrToInt FFI.buildPtrToInt

-- | Convert an integer to a pointer.
inttoptr :: (ValueCons value, IsInteger a, IsType b) => value a -> CodeGenFunction r (value (Ptr b))
inttoptr = convert FFI.constIntToPtr FFI.buildIntToPtr

-- | Convert between to values of the same size by just copying the bit pattern.
bitcast :: (ValueCons value, IsFirstClass a, IsFirstClass b, IsSized a, IsSized b, SizeOf a ~ SizeOf b)
        => value a -> CodeGenFunction r (value b)
bitcast = convert FFI.constBitCast FFI.buildBitCast


--------------------------------------

type CmpValueResult value0 value1 a = BinOpValue value0 value1 (CmpResult a)

type CmpResult c = ShapedType (ShapeOf c) Bool

class (IsFirstClass c) => CmpRet c where
    cmpBld :: LP.Proxy c -> CmpPredicate -> FFIBinOp
    cmpCnst :: LP.Proxy c -> CmpPredicate -> FFIConstBinOp

instance CmpRet Float   where cmpBld _ = fcmpBld ; cmpCnst _ = fcmpCnst
instance CmpRet Double  where cmpBld _ = fcmpBld ; cmpCnst _ = fcmpCnst
instance CmpRet FP128   where cmpBld _ = fcmpBld ; cmpCnst _ = fcmpCnst
instance CmpRet Bool    where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Word    where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Word8   where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Word16  where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Word32  where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Word64  where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance CmpRet Int     where cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst
instance CmpRet Int8    where cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst
instance CmpRet Int16   where cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst
instance CmpRet Int32   where cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst
instance CmpRet Int64   where cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst
instance CmpRet (Foreign.Ptr a)
                        where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance (IsType a) =>
         CmpRet (Ptr a) where cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst

instance (Dec.Positive n) => CmpRet (WordN n) where
    cmpBld _ = ucmpBld ; cmpCnst _ = ucmpCnst
instance (Dec.Positive n) => CmpRet (IntN n) where
    cmpBld _ = scmpBld ; cmpCnst _ = scmpCnst

instance (CmpRet a, IsPrimitive a, Dec.Positive n) => CmpRet (Vector n a) where
    cmpBld _ = cmpBld (LP.Proxy :: LP.Proxy a)
    cmpCnst _ = cmpCnst (LP.Proxy :: LP.Proxy a)


{- |
Compare values of ordered types
and choose predicates according to the compared types.
Floating point numbers are compared in \"ordered\" mode,
that is @NaN@ operands yields 'False' as result.
Pointers are compared unsigned.
These choices are consistent with comparison in plain Haskell.
-}
cmp :: forall value0 value1 a r.
   (ValueCons2 value0 value1, CmpRet a) =>
   CmpPredicate -> value0 a -> value1 a ->
   CodeGenFunction r (CmpValueResult value0 value1 a)
cmp p =
    binop
        (cmpCnst (LP.Proxy :: LP.Proxy a) p)
        (cmpBld (LP.Proxy :: LP.Proxy a) p)

ucmpBld :: CmpPredicate -> FFIBinOp
ucmpBld p = flip FFI.buildICmp (FFI.fromIntPredicate (uintFromCmpPredicate p))

scmpBld :: CmpPredicate -> FFIBinOp
scmpBld p = flip FFI.buildICmp (FFI.fromIntPredicate (sintFromCmpPredicate p))

fcmpBld :: CmpPredicate -> FFIBinOp
fcmpBld p = flip FFI.buildFCmp (FFI.fromRealPredicate (fpFromCmpPredicate p))


ucmpCnst :: CmpPredicate -> FFIConstBinOp
ucmpCnst p = FFI.constICmp (FFI.fromIntPredicate (uintFromCmpPredicate p))

scmpCnst :: CmpPredicate -> FFIConstBinOp
scmpCnst p = FFI.constICmp (FFI.fromIntPredicate (sintFromCmpPredicate p))

fcmpCnst :: CmpPredicate -> FFIConstBinOp
fcmpCnst p = FFI.constFCmp (FFI.fromRealPredicate (fpFromCmpPredicate p))


_ucmp ::
    (ValueCons2 value0 value1, CmpRet a, IsInteger a) =>
    CmpPredicate -> value0 a -> value1 a ->
    CodeGenFunction r (CmpValueResult value0 value1 a)
_ucmp p = binop (ucmpCnst p) (ucmpBld p)

_scmp ::
    (ValueCons2 value0 value1, CmpRet a, IsInteger a) =>
    CmpPredicate -> value0 a -> value1 a ->
    CodeGenFunction r (CmpValueResult value0 value1 a)
_scmp p = binop (scmpCnst p) (scmpBld p)

pcmp ::
    (ValueCons2 value0 value1, IsType a) =>
    IntPredicate -> value0 (Ptr a) -> value1 (Ptr a) ->
    CodeGenFunction r (BinOpValue value0 value1 (Ptr a))
pcmp p =
    binop
        (FFI.constICmp (FFI.fromIntPredicate p))
        (flip FFI.buildICmp (FFI.fromIntPredicate p))


{-# DEPRECATED icmp "use cmp or pcmp instead" #-}
-- | Compare integers.
icmp ::
    (ValueCons2 value0 value1, CmpRet a, IsIntegerOrPointer a) =>
    IntPredicate -> value0 a -> value1 a ->
    CodeGenFunction r (CmpValueResult value0 value1 a)
icmp p =
    binop
        (FFI.constICmp (FFI.fromIntPredicate p))
        (flip FFI.buildICmp (FFI.fromIntPredicate p))

-- | Compare floating point values.
fcmp ::
    (ValueCons2 value0 value1, CmpRet a, IsFloating a) =>
    FPPredicate -> value0 a -> value1 a ->
    CodeGenFunction r (CmpValueResult value0 value1 a)
fcmp p =
    binop
        (FFI.constFCmp (FFI.fromRealPredicate p))
        (flip FFI.buildFCmp (FFI.fromRealPredicate p))

--------------------------------------

setHasNoNaNs, setHasNoInfs, setHasNoSignedZeros, setHasAllowReciprocal,
    setFastMath :: (IsFloating a) => Bool -> Value a -> CodeGenFunction r ()
setHasNoNaNs          = fastMath FFI.setHasNoNaNs
setHasNoInfs          = fastMath FFI.setHasNoInfs
setHasNoSignedZeros   = fastMath FFI.setHasNoSignedZeros
setHasAllowReciprocal = fastMath FFI.setHasAllowReciprocal
setFastMath           = fastMath FFI.setHasUnsafeAlgebra

fastMath ::
    (IsFloating a) =>
    (FFI.ValueRef -> FFI.Bool -> IO ()) ->
    Bool -> Value a -> CodeGenFunction r ()
fastMath setter b (Value v) = liftIO $ setter v $ FFI.consBool b


--------------------------------------

-- XXX could do const song and dance
-- | Select between two values depending on a boolean.
select :: (CmpRet a) => Value (CmpResult a) -> Value a -> Value a -> CodeGenFunction r (Value a)
select (Value cnd) (Value thn) (Value els) =
    liftM Value $
      withCurrentBuilder $ \ bldPtr ->
        U.withEmptyCString $
          FFI.buildSelect bldPtr cnd thn els

--------------------------------------

type Caller = FFI.BuilderRef -> [FFI.ValueRef] -> IO FFI.ValueRef

{-
Function (a -> b -> IO c)
Value a -> Value b -> CodeGenFunction r c
-}

-- |Acceptable arguments to 'call'.
class (f ~ CalledFunction g, r ~ CodeResult g, g ~ CallerFunction r f) =>
         CallArgs r f g where
    type CalledFunction g
    type CallerFunction r f
    doCall :: Call f -> g

instance (Value a ~ a', CallArgs r b b') => CallArgs r (a -> b) (a' -> b') where
    type CalledFunction (a' -> b') = UnValue a' -> CalledFunction b'
    type CallerFunction r (a -> b) = Value a -> CallerFunction r b
    doCall f a = doCall (applyCall f a)

instance
    (r ~ r', Value a ~ a') =>
        CallArgs r (IO a) (CodeGenFunction r' a') where
    type CalledFunction (CodeGenFunction r' a') = IO (UnValue a')
    type CallerFunction r (IO a) = CodeGenFunction r (Value a)
    doCall = runCall

doCallDef :: Caller -> [FFI.ValueRef] -> b -> CodeGenFunction r (Value a)
doCallDef mkCall args _ =
    withCurrentBuilder $ \ bld ->
      liftM Value $ mkCall bld (reverse args)

-- | Call a function with the given arguments.  The 'call' instruction is variadic, i.e., the number of arguments
-- it takes depends on the type of /f/.
call :: (CallArgs r f g) => Function f -> g
call = doCall . callFromFunction

data Call a = Call Caller [FFI.ValueRef]

callFromFunction :: Function a -> Call a
callFromFunction (Value f) = Call (U.makeCall f) []

-- like Applicative.<*>
infixl 4 `applyCall`

applyCall :: Call (a -> b) -> Value a -> Call b
applyCall (Call mkCall args) (Value arg) = Call mkCall (arg:args)

runCall :: Call (IO a) -> CodeGenFunction r (Value a)
runCall (Call mkCall args) = doCallDef mkCall args ()


invokeFromFunction ::
          BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function f         -- ^Function to call.
       -> Call f
invokeFromFunction (BasicBlock norm) (BasicBlock expt) (Value f) =
    Call (U.makeInvoke norm expt f) []

-- | Call a function with exception handling.
invoke :: (CallArgs r f g)
       => BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function f         -- ^Function to call.
       -> g
invoke norm expt f = doCall $ invokeFromFunction norm expt f

callWithConvFromFunction :: FFI.CallingConvention -> Function f -> Call f
callWithConvFromFunction cc (Value f) =
    Call (U.makeCallWithCc cc f) []

-- | Call a function with the given arguments.  The 'call' instruction
-- is variadic, i.e., the number of arguments it takes depends on the
-- type of /f/.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
callWithConv :: (CallArgs r f g) => FFI.CallingConvention -> Function f -> g
callWithConv cc f = doCall $ callWithConvFromFunction cc f

invokeWithConvFromFunction ::
          FFI.CallingConvention -- ^Calling convention
       -> BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function f         -- ^Function to call.
       -> Call f
invokeWithConvFromFunction cc (BasicBlock norm) (BasicBlock expt) (Value f) =
    Call (U.makeInvokeWithCc cc norm expt f) []

-- | Call a function with exception handling.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
invokeWithConv :: (CallArgs r f g)
               => FFI.CallingConvention -- ^Calling convention
               -> BasicBlock         -- ^Normal return point.
               -> BasicBlock         -- ^Exception return point.
               -> Function f         -- ^Function to call.
               -> g
invokeWithConv cc norm expt f =
    doCall $ invokeWithConvFromFunction cc norm expt f

--------------------------------------

-- XXX could do const song and dance
-- |Join several variables (virtual registers) from different basic blocks into one.
-- All of the variables in the list are joined.  See also 'addPhiInputs'.
phi :: forall a r . (IsFirstClass a) => [(Value a, BasicBlock)] -> CodeGenFunction r (Value a)
phi incoming =
    liftM Value $
      withCurrentBuilder $ \ bldPtr -> do
        inst <- U.buildEmptyPhi bldPtr =<< typeRef (LP.Proxy :: LP.Proxy a)
        U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]
        return inst

-- |Add additional inputs to an existing phi node.
-- The reason for this instruction is that sometimes the structure of the code
-- makes it impossible to have all variables in scope at the point where you need the phi node.
addPhiInputs :: forall a r . (IsFirstClass a)
             => Value a                      -- ^Must be a variable from a call to 'phi'.
             -> [(Value a, BasicBlock)]      -- ^Variables to add.
             -> CodeGenFunction r ()
addPhiInputs (Value inst) incoming =
    liftIO $ U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]


--------------------------------------

-- | Acceptable argument to array memory allocation.
class AllocArg a where
    getAllocArg :: a -> Value Word
instance (i ~ Word) => AllocArg (Value i) where
    getAllocArg = id
instance (i ~ Word) => AllocArg (ConstValue i) where
    getAllocArg = value
instance AllocArg Word where
    getAllocArg = valueOf

-- could be moved to Util.Memory
-- FFI.buildMalloc deprecated since LLVM-2.7
-- XXX What's the type returned by malloc
-- | Allocate heap memory.
malloc :: forall a r . (IsSized a) => CodeGenFunction r (Value (Ptr a))
malloc = arrayMalloc (1::Word)

type BytePtr = Ptr Word8

{-
I use a pointer type as size parameter of 'malloc'.
This way I hope that the parameter has always the correct size (32 or 64 bit).
A side effect is that we can convert the result of 'getelementptr' using 'bitcast',
that does not suffer from the slow assembly problem. (bug #8281)
-}
foreign import ccall "&aligned_malloc_sizeptr"
   alignedMalloc :: FunPtr (BytePtr -> BytePtr -> IO BytePtr)

foreign import ccall "&aligned_free"
   alignedFree :: FunPtr (BytePtr -> IO ())


{-
There is a bug in LLVM-2.7 and LLVM-2.8
(http://llvm.org/bugs/show_bug.cgi?id=8281)
that causes huge assembly times for expressions like
ptrtoint(getelementptr(zero,..)).
If you break those expressions into two statements
at separate lines, everything is fine.
But the C interface is too clever,
and rewrites two separate statements into a functional expression on a single line.
Such code is generated whenever you call
buildMalloc, buildArrayMalloc, sizeOf (called by buildMalloc), or alignOf.
One possible way is to write a getelementptr expression
containing a nullptr in a way
that hides the constant nature of nullptr.

    ptr <- alloca
    store (value zero) ptr
    z <- load ptr
    size <- bitcast =<<
       getElementPtr (z :: Value (Ptr a)) (getAllocArg s, ())

However, I found that bitcast on pointers causes no problems.
Thus I switched to using pointers for size quantities.
This still allows for optimizations involving pointers.
-}

-- XXX What's the type returned by arrayMalloc?
-- | Allocate heap (array) memory.
arrayMalloc :: forall a r s . (IsSized a, AllocArg s) =>
               s -> CodeGenFunction r (Value (Ptr a)) -- XXX
arrayMalloc s = do
    func <- CodeGen.staticNamedFunction "alignedMalloc" alignedMalloc
--    func <- externFunction "malloc"

    size <- sizeOfArray (LP.Proxy :: LP.Proxy a) (getAllocArg s)
    alignment <- alignOf (LP.Proxy :: LP.Proxy a)
    bitcast =<< call func size alignment

-- XXX What's the type returned by malloc
-- | Allocate stack memory.
alloca :: forall a r . (IsSized a) => CodeGenFunction r (Value (Ptr a))
alloca =
    liftM Value $
    withCurrentBuilder $ \ bldPtr -> do
      typ <- typeRef (LP.Proxy :: LP.Proxy a)
      U.withEmptyCString $ FFI.buildAlloca bldPtr typ

-- XXX What's the type returned by arrayAlloca?
-- | Allocate stack (array) memory.
arrayAlloca :: forall a r s . (IsSized a, AllocArg s) =>
               s -> CodeGenFunction r (Value (Ptr a))
arrayAlloca s =
    liftM Value $
    withCurrentBuilder $ \ bldPtr -> do
      typ <- typeRef (LP.Proxy :: LP.Proxy a)
      U.withEmptyCString $
        FFI.buildArrayAlloca bldPtr typ (case getAllocArg s of Value v -> v)

-- FFI.buildFree deprecated since LLVM-2.7
-- XXX What's the type of free?
-- | Free heap memory.
free :: (IsType a) => Value (Ptr a) -> CodeGenFunction r ()
free ptr = do
    func <- CodeGen.staticNamedFunction "alignedFree" alignedFree
--    func <- externFunction "free"
    _ <- call func =<< bitcast ptr
    return ()


-- | If we want to export that, then we should have a Size type
-- This is the official implementation,
-- but it suffers from the ptrtoint(gep) bug.
_sizeOf ::
    forall a r.
    (IsSized a) => LP.Proxy a -> CodeGenFunction r (Value Word)
_sizeOf a =
    liftIO $ liftM Value $
    FFI.sizeOf =<< typeRef a

_alignOf ::
    forall a r.
    (IsSized a) => LP.Proxy a -> CodeGenFunction r (Value Word)
_alignOf a =
    liftIO $ liftM Value $
    FFI.alignOf =<< typeRef a


-- Here are reimplementation from Constants.cpp that avoid the ptrtoint(gep) bug #8281.
-- see ConstantExpr::getSizeOf
sizeOfArray ::
    forall a r . (IsSized a) =>
    LP.Proxy a -> Value Word -> CodeGenFunction r (Value BytePtr)
sizeOfArray _ len =
    bitcast =<<
       getElementPtr (value zero :: Value (Ptr a)) (len, ())

-- see ConstantExpr::getAlignOf
alignOf ::
    forall a r . (IsSized a) =>
    LP.Proxy a -> CodeGenFunction r (Value BytePtr)
alignOf _ =
    bitcast =<<
       getElementPtr0 (value zero :: Value (Ptr (Struct (Bool, (a, ()))))) (d1, ())


-- | Load a value from memory.
load :: Value (Ptr a)                   -- ^ Address to load from.
     -> CodeGenFunction r (Value a)
load (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildLoad bldPtr p

-- | Store a value in memory
store :: Value a                        -- ^ Value to store.
      -> Value (Ptr a)                  -- ^ Address to store to.
      -> CodeGenFunction r ()
store (Value v) (Value p) = do
    withCurrentBuilder_ $ \ bldPtr ->
      FFI.buildStore bldPtr v p
    return ()

-- | Address arithmetic.  See LLVM description.
-- (The type isn't as accurate as it should be.)
_getElementPtrDynamic :: (IsInteger i) =>
    Value (Ptr a) -> [Value i] -> CodeGenFunction r (Value (Ptr b))
_getElementPtrDynamic (Value ptr) ixs =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen [ v | Value v <- ixs ] $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)

-- | Address arithmetic.  See LLVM description.
-- The index is a nested tuple of the form @(i1,(i2,( ... ())))@.
-- (This is without a doubt the most confusing LLVM instruction, but the types help.)
getElementPtr :: forall a o i r . (GetElementPtr o i, IsIndexArg a) =>
                 Value (Ptr o) -> (a, i) -> CodeGenFunction r (Value (Ptr (ElementPtrType o i)))
getElementPtr (Value ptr) (a, ixs) =
    let ixl = getArg a : getIxList (LP.Proxy :: LP.Proxy o) ixs in
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen ixl $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)

-- | Like getElementPtr, but with an initial index that is 0.
-- This is useful since any pointer first need to be indexed off the pointer, and then into
-- its actual value.  This first indexing is often with 0.
getElementPtr0 :: (GetElementPtr o i) =>
                  Value (Ptr o) -> i -> CodeGenFunction r (Value (Ptr (ElementPtrType o i)))
getElementPtr0 p i = getElementPtr p (0::Word32, i)

_getElementPtr :: forall value o i i0 r.
    (ValueCons value, GetElementPtr o i, IsIndexType i0) =>
    value (Ptr o) -> (value i0, i) ->
    CodeGenFunction r (value (Ptr (ElementPtrType o i)))
_getElementPtr vptr (a, ixs) =
    let withArgs act =
            U.withArrayLen
                (unValue a : getIxList (LP.Proxy :: LP.Proxy o) ixs) $
            \ idxLen idxPtr ->
                act idxPtr (fromIntegral idxLen)
    in  unop
            (\ptr -> withArgs $ FFI.constGEP ptr)
            (\bldPtr ptr cstr ->
                withArgs $ \idxPtr idxLen ->
                    FFI.buildGEP bldPtr ptr idxPtr idxLen cstr)
            vptr

--------------------------------------
{-
instance (IsConst a) => Show (ConstValue a) -- XXX
instance (IsConst a) => Eq (ConstValue a)

{-
instance (IsConst a) => Eq (ConstValue a) where
    ConstValue x == ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPOEQ) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntEQ) x y)
    ConstValue x /= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPONE) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntNE) x y)

instance (IsConst a) => Ord (ConstValue a) where
    ConstValue x <  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPOLT) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntLT) x y)
    ConstValue x <= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPOLE) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntLE) x y)
    ConstValue x >  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPOGT) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntGT) x y)
    ConstValue x >= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (FFI.fromRealPredicate  FPOGE) x y)
                        else ConstValue (FFI.constICmp (FFI.fromIntPredicate IntGE) x y)
-}

instance (Num a, IsConst a) => Num (ConstValue a) where
    ConstValue x + ConstValue y  =  ConstValue (FFI.constAdd x y)
    ConstValue x - ConstValue y  =  ConstValue (FFI.constSub x y)
    ConstValue x * ConstValue y  =  ConstValue (FFI.constMul x y)
    negate (ConstValue x)        =  ConstValue (FFI.constNeg x)
    fromInteger x                =  constOf (fromInteger x :: a)
-}
