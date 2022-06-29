module LLVM.Util.Intrinsic (
   min, max, abs,
   truncate, floor,
   maybeUAddSat, maybeSAddSat, maybeUSubSat, maybeSSubSat,

   call1, call2,
   ) where

import qualified LLVM.Core.Proxy as LP
import qualified LLVM.Core as LLVM
import LLVM.Core
   (CodeGenFunction, Value, IsType, IsFirstClass,
    IsArithmetic, IsInteger, IsFloating)

import qualified LLVM.FFI.Core as FFI

import Data.Maybe.HT (toMaybe)

import Prelude hiding (min, max, abs, truncate, floor)


valueTypeName :: (IsType a) => Value a -> String
valueTypeName =
   LLVM.intrinsicTypeName . ((\_ -> LP.Proxy) :: Value a -> LP.Proxy a)

functionName :: (IsType a) => String -> Value a -> String
functionName fn x = "llvm." ++ fn ++ "." ++ valueTypeName x

call1 ::
   (IsFirstClass a) =>
   String -> Value a -> CodeGenFunction r (Value a)
call1 fn x = do
   op <- LLVM.externFunction $ functionName fn x
   LLVM.call op x

call2 ::
   (IsFirstClass a) =>
   String -> Value a -> Value a -> CodeGenFunction r (Value a)
call2 fn x y = do
   op <- LLVM.externFunction $ functionName fn x
   LLVM.call op x y



min, max ::
   (IsArithmetic a) => Value a -> Value a -> CodeGenFunction r (Value a)
min = call2 "minnum"
max = call2 "maxnum"

abs :: (IsArithmetic a) => Value a -> CodeGenFunction r (Value a)
abs = call1 "fabs"

truncate, floor :: (IsFloating a) => Value a -> CodeGenFunction r (Value a)
truncate = call1 "trunc"
floor = call1 "floor"


{- |
Available since LLVM-8.
-}
maybeUAddSat, maybeSAddSat, maybeUSubSat, maybeSSubSat ::
   (IsInteger a) => Maybe (Value a -> Value a -> CodeGenFunction r (Value a))
maybeUAddSat = opsat "uadd"
maybeSAddSat = opsat "sadd"
maybeUSubSat = opsat "usub"
maybeSSubSat = opsat "ssub"

opsat ::
   (IsFirstClass a) =>
   String -> Maybe (Value a -> Value a -> CodeGenFunction r (Value a))
opsat name = toMaybe (FFI.version >= 800) $ call2 (name++".sat")
