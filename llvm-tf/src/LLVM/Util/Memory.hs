{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Util.Memory (
    memcpy,
    memmove,
    memset,
    IsLengthType,
    ) where

import LLVM.Core.Proxy (Proxy(Proxy))
import LLVM.Core

import Data.Word (Word8, Word32, Word64, Word)

import Control.Functor.HT (void, )


class IsFirstClass len => IsLengthType len where

instance IsLengthType Word where
instance IsLengthType Word32 where
instance IsLengthType Word64 where


memcpyFunc ::
   forall len.
   IsLengthType len =>
   TFunction (Ptr Word8 -> Ptr Word8 -> len -> Word32 -> Bool -> IO ())
memcpyFunc =
   newNamedFunction ExternalLinkage $
      "llvm.memcpy.p0i8.p0i8." ++ intrinsicTypeName (Proxy :: Proxy len)

memcpy ::
   IsLengthType len =>
   CodeGenModule
      (Value (Ptr Word8) ->
       Value (Ptr Word8) ->
       Value len ->
       Value Word32 ->
       Value Bool ->
       CodeGenFunction r ())
memcpy =
   fmap
      (\f dest src len align volatile ->
          void $ call f dest src len align volatile)
      memcpyFunc


memmoveFunc ::
   forall len.
   IsLengthType len =>
   TFunction (Ptr Word8 -> Ptr Word8 -> len -> Word32 -> Bool -> IO ())
memmoveFunc =
   newNamedFunction ExternalLinkage $
      "llvm.memmove.p0i8.p0i8." ++ intrinsicTypeName (Proxy :: Proxy len)

memmove ::
   IsLengthType len =>
   CodeGenModule
      (Value (Ptr Word8) ->
       Value (Ptr Word8) ->
       Value len ->
       Value Word32 ->
       Value Bool ->
       CodeGenFunction r ())
memmove =
   fmap
      (\f dest src len align volatile ->
          void $ call f dest src len align volatile)
      memmoveFunc


memsetFunc ::
   forall len.
   IsLengthType len =>
   TFunction (Ptr Word8 -> Word8 -> len -> Word32 -> Bool -> IO ())
memsetFunc =
   newNamedFunction ExternalLinkage $
      "llvm.memset.p0i8." ++ intrinsicTypeName (Proxy :: Proxy len)

memset ::
   IsLengthType len =>
   CodeGenModule
      (Value (Ptr Word8) ->
       Value Word8 ->
       Value len ->
       Value Word32 ->
       Value Bool ->
       CodeGenFunction r ())
memset =
   fmap
      (\f dest val len align volatile ->
          void $ call f dest val len align volatile)
      memsetFunc
