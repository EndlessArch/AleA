{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module LLVM.Util.Arithmetic(
    TValue,
    (%==), (%/=), (%<), (%<=), (%>), (%>=),
    (%&&), (%||),
    (?), (??),
    retrn, set,
    ArithFunction, arithFunction, Return,
    ToArithFunction, toArithFunction, recursiveFunction,
    CallIntrinsic,
    ) where

import qualified LLVM.Util.Intrinsic as Intrinsic
import qualified LLVM.Core as LLVM
import LLVM.Util.Loop (mapVector, mapVector2)
import LLVM.Core.CodeGen (UnValue, CodeValue, CodeResult)
import LLVM.Core

import qualified Type.Data.Num.Decimal.Number as Dec

import Control.Monad (liftM2)

-- |Synonym for @CodeGenFunction r (Value a)@.
type TValue r a = CodeGenFunction r (Value a)


infix  4  %==, %/=, %<, %<=, %>=, %>
-- |Comparison functions.
(%==), (%/=), (%<), (%<=), (%>), (%>=) :: (CmpRet a) => TValue r a -> TValue r a -> TValue r (CmpResult a)
(%==) = binop $ LLVM.cmp CmpEQ
(%/=) = binop $ LLVM.cmp CmpNE
(%>)  = binop $ LLVM.cmp CmpGT
(%>=) = binop $ LLVM.cmp CmpGE
(%<)  = binop $ LLVM.cmp CmpLT
(%<=) = binop $ LLVM.cmp CmpLE

infixr 3  %&&
infixr 2  %||
-- |Lazy and.
(%&&) :: TValue r Bool -> TValue r Bool -> TValue r Bool
a %&& b = a ? (b, return (valueOf False))
-- |Lazy or.
(%||) :: TValue r Bool -> TValue r Bool -> TValue r Bool
a %|| b = a ? (return (valueOf True), b)

infix  0 ?
-- |Conditional, returns first element of the pair when condition is true, otherwise second.
(?) :: (IsFirstClass a) => TValue r Bool -> (TValue r a, TValue r a) -> TValue r a
c ? (t, f) = do
    lt <- newBasicBlock
    lf <- newBasicBlock
    lj <- newBasicBlock
    c' <- c
    condBr c' lt lf
    defineBasicBlock lt
    rt <- t
    lt' <- getCurrentBasicBlock
    br lj
    defineBasicBlock lf
    rf <- f
    lf' <- getCurrentBasicBlock
    br lj
    defineBasicBlock lj
    phi [(rt, lt'), (rf, lf')]

infix 0 ??
(??) :: (IsFirstClass a, CmpRet a) => TValue r (CmpResult a) -> (TValue r a, TValue r a) -> TValue r a
c ?? (t, f) = do
    c' <- c
    t' <- t
    f' <- f
    select c' t' f'

-- | Return a value from an 'arithFunction'.
retrn :: TValue a a -> CodeGenFunction a ()
retrn x = x >>= ret

-- | Use @x <- set $ ...@ to make a binding.
set :: TValue r a -> CodeGenFunction r (TValue r a)
set x = do x' <- x; return (return x')

instance Eq (CodeGenFunction r av) where
    (==) = error "CodeGenFunction Value: (==)"
instance Ord (CodeGenFunction r av) where
    compare = error "CodeGenFunction Value: compare"

instance
    (IsArithmetic a, CmpRet a, Num a, IsConst a, Value a ~ av) =>
        Num (CodeGenFunction r av) where
    (+) = binop add
    (-) = binop sub
    (*) = binop mul
    negate = (>>= neg)
    abs x = x %< 0 ?? (-x, x)
    signum x = x %< 0 ?? (-1, x %> 0 ?? (1, 0))
    fromInteger = return . valueOf . fromInteger

instance
    (IsArithmetic a, CmpRet a, Num a, IsConst a, Value a ~ av) =>
        Enum (CodeGenFunction r av) where
    succ x = x + 1
    pred x = x - 1
    fromEnum _ = error "CodeGenFunction Value: fromEnum"
    toEnum = fromIntegral

instance
    (IsArithmetic a, CmpRet a, Num a, IsConst a, Value a ~ av) =>
        Real (CodeGenFunction r av) where
    toRational _ = error "CodeGenFunction Value: toRational"

instance
    (CmpRet a, Num a, IsConst a, IsInteger a, Value a ~ av) =>
        Integral (CodeGenFunction r av) where
    quot = binop idiv
    rem  = binop irem
    quotRem x y = (quot x y, rem x y)
    toInteger _ = error "CodeGenFunction Value: toInteger"

instance
    (CmpRet a, Fractional a, IsConst a, IsFloating a, Value a ~ av) =>
        Fractional (CodeGenFunction r av) where
    (/) = binop fdiv
    fromRational = return . valueOf . fromRational

instance
    (CmpRet a, Fractional a, IsConst a, IsFloating a, Value a ~ av) =>
        RealFrac (CodeGenFunction r av) where
    properFraction _ = error "CodeGenFunction Value: properFraction"

instance
    (CmpRet a, CallIntrinsic a, Floating a, IsConst a, IsFloating a, Value a ~ av) =>
        Floating (CodeGenFunction r av) where
    pi = return $ valueOf pi
    sqrt = callIntrinsic1 "sqrt"
    sin = callIntrinsic1 "sin"
    cos = callIntrinsic1 "cos"
    (**) = callIntrinsic2 "pow"
    exp = callIntrinsic1 "exp"
    log = callIntrinsic1 "log"

    asin _ = error "LLVM missing intrinsic: asin"
    acos _ = error "LLVM missing intrinsic: acos"
    atan _ = error "LLVM missing intrinsic: atan"

    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2

instance
    (CmpRet a, CallIntrinsic a, RealFloat a, IsConst a, IsFloating a, Value a ~ av) =>
        RealFloat (CodeGenFunction r av) where
    floatRadix _ = floatRadix (undefined :: a)
    floatDigits _ = floatDigits (undefined :: a)
    floatRange _ = floatRange (undefined :: a)
    decodeFloat _ = error "CodeGenFunction Value: decodeFloat"
    encodeFloat _ _ = error "CodeGenFunction Value: encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "CodeGenFunction Value: scaleFloat"
    isNaN _ = error "CodeGenFunction Value: isNaN"
    isInfinite _ = error "CodeGenFunction Value: isInfinite"
    isDenormalized _ = error "CodeGenFunction Value: isDenormalized"
    isNegativeZero _ = error "CodeGenFunction Value: isNegativeZero"
    isIEEE _ = isIEEE (undefined :: a)

binop :: (Value a -> Value b -> TValue r c) ->
         TValue r a -> TValue r b -> TValue r c
binop op x y = do
    x' <- x
    y' <- y
    op x' y'

-------------------------------------------

{- |
Turn
@(a -> b -> CodeGenFunction r c)@
into
@(a -> b -> CodeGenFunction r ())@
for @r ~ Result c@
-}
class (RetB a ~ b, CodeValue a ~ z, RetA z b ~ a) => Return z a b where
    type RetA z b
    type RetB a
    addRet :: a -> b

instance
    (Ret z, Result z ~ r, r ~ ra, r ~ rb, z ~ a, unit ~ ()) =>
        Return z (CodeGenFunction ra a) (CodeGenFunction rb unit) where
    type RetA z (CodeGenFunction rb unit) = CodeGenFunction (Result z) z
    type RetB (CodeGenFunction ra a) = CodeGenFunction ra ()
    addRet code = ret =<< code

instance (Return z b0 b1, a0 ~ a1) => Return z (a0 -> b0) (a1 -> b1) where
    type RetA z (a1 -> b1) = a1 -> RetA z b1
    type RetB (a0 -> b0) = a0 -> RetB b0
    addRet f = addRet . f


class (FunA r b ~ a, FunB a ~ b, CodeResult a ~ r) => ArithFunction r a b where
    type FunA r b
    type FunB a
    arithFunction' :: a -> b

instance
    (r ~ ra, r ~ rb, a ~ b) =>
        ArithFunction r (CodeGenFunction ra a) (CodeGenFunction rb b) where
    type FunA r (CodeGenFunction rb b) = CodeGenFunction r b
    type FunB (CodeGenFunction ra a) = CodeGenFunction ra a
    arithFunction' x = x

instance
    (ArithFunction r b0 b1, a0 ~ CodeGenFunction r a1) =>
        ArithFunction r (a0 -> b0) (a1 -> b1) where
    type FunA r (a1 -> b1) = CodeGenFunction r a1 -> FunA r b1
    type FunB (a0 -> b0) = CodeValue a0 -> FunB b0
    arithFunction' f = arithFunction' . f . return

-- |Unlift a function with @TValue@ to have @Value@ arguments.
arithFunction :: (ArithFunction r a b, r ~ Result z, Return z b c) => a -> c
arithFunction = addRet . arithFunction'


class
    (TFunB r a ~ b, TFunA b ~ a, CodeResult b ~ r) =>
        ToArithFunction r a b where
    type TFunA b
    type TFunB r a
    toArithFunction' :: CodeGenFunction r (Call a) -> b

instance (Value a ~ b) => ToArithFunction r (IO a) (CodeGenFunction r b) where
    type TFunA (CodeGenFunction r b) = IO (UnValue b)
    type TFunB r (IO a) = TValue r a
    toArithFunction' cl = runCall =<< cl

instance
    (ToArithFunction r b0 b1, CodeGenFunction r (Value a0) ~ a1) =>
        ToArithFunction r (a0 -> b0) (a1 -> b1) where
    type TFunA (a1 -> b1) = UnValue (CodeValue a1) -> TFunA b1
    type TFunB r (a0 -> b0) = TValue r a0 -> TFunB r b0
    toArithFunction' cl x =
        toArithFunction' (liftM2 applyCall cl x)


_toArithFunction2 ::
    Function (a -> b -> IO c) -> TValue r a -> TValue r b -> TValue r c
_toArithFunction2 f tx ty = do
    x <- tx
    y <- ty
    runCall $ callFromFunction f `applyCall` x `applyCall` y

-- |Lift a function from having @Value@ arguments to having @TValue@ arguments.
toArithFunction :: (ToArithFunction r f g) => Function f -> g
toArithFunction = toArithFunction' . return . callFromFunction

-------------------------------------------

-- |Define a recursive 'arithFunction', gets passed itself as the first argument.
recursiveFunction ::
    (IsFunction f, FunctionArgs f, code ~ FunctionCodeGen f,
     ArithFunction r arith open, r ~ Result z, Return z open code,
     ToArithFunction r f g) =>
    (g -> arith) -> CodeGenModule (Function f)
recursiveFunction af = do
    f <- newFunction ExternalLinkage
    defineFunction f $ arithFunction $ af $ toArithFunction f
    return f


-------------------------------------------

class CallIntrinsic a where
    callIntrinsic1' :: String -> Value a -> TValue r a
    callIntrinsic2' :: String -> Value a -> Value a -> TValue r a

instance CallIntrinsic Float where
    callIntrinsic1' = Intrinsic.call1
    callIntrinsic2' = Intrinsic.call2

instance CallIntrinsic Double where
    callIntrinsic1' = Intrinsic.call1
    callIntrinsic2' = Intrinsic.call2

{-
I think such a special case for certain systems
would be better handled as in LLVM.Extra.Extension.
(lemming)
-}
macOS :: Bool
#if defined(__MACOS__)
macOS = True
#else
macOS = False
#endif

instance (Dec.Positive n, IsPrimitive a, CallIntrinsic a) => CallIntrinsic (Vector n a) where
    callIntrinsic1' s x =
       if macOS && Dec.integerFromSingleton (Dec.singleton :: Dec.Singleton n) == 4 &&
          elem s ["sqrt", "log", "exp", "sin", "cos", "tan"]
         then do
            op <- externFunction ("v" ++ s ++ "f")
            call op x
         else mapVector (callIntrinsic1' s) x
    callIntrinsic2' s = mapVector2 (callIntrinsic2' s)

callIntrinsic1 :: (CallIntrinsic a) => String -> TValue r a -> TValue r a
callIntrinsic1 s x = do x' <- x; callIntrinsic1' s x'

callIntrinsic2 :: (CallIntrinsic a) => String -> TValue r a -> TValue r a -> TValue r a
callIntrinsic2 s = binop (callIntrinsic2' s)
