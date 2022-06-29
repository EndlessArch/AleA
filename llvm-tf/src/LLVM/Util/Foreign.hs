{-# LANGUAGE ScopedTypeVariables #-}
-- These are replacements for the broken equivalents in Foreign.*.
-- The functions in Foreign.* do not obey the required alignment.
module LLVM.Util.Foreign where

import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Util.Proxy as LP
import qualified LLVM.Core as LLVM

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (alignPtr)


with :: (EE.Marshal a) => a -> (LLVM.Ptr a -> IO b) -> IO b
with x act =
    alloca $ \ p -> do
    EE.poke p x
    act p

alloca :: forall a b. (EE.Marshal a) => (LLVM.Ptr a -> IO b) -> IO b
alloca act =
    allocaBytes (2 * EE.sizeOf (LP.Proxy :: LP.Proxy a)) $ \ p ->
        act $ LLVM.uncheckedFromPtr $
        alignPtr p (EE.alignment (LP.Proxy :: LP.Proxy a))

withArrayLen :: (EE.Marshal a) => [a] -> (Int -> LLVM.Ptr a -> IO b) -> IO b
withArrayLen xs act =
    let l = length xs in
    allocaBytes ((l+1) * EE.sizeOf (proxyFromList xs)) $ \ p -> do
    let p' =
            LLVM.uncheckedFromPtr $
            alignPtr p $ EE.alignment $ proxyFromList xs
    EE.pokeList p' xs
    act l p'

proxyFromList :: [a] -> LP.Proxy a
proxyFromList _ = LP.Proxy
