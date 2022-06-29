{-# LANGUAGE TypeFamilies #-}
 -- |An 'ExecutionEngine' is JIT compiler that is used to generate code for an LLVM module.
module LLVM.ExecutionEngine(
    -- * Execution engine
    EngineAccess,
    ExecutionEngine,
    getEngine,
    runEngineAccess,
    runEngineAccessWithModule,
    addModule,
    ExecutionFunction,
    Importer,
    getExecutionFunction,
    getPointerToFunction,
    addFunctionValue,
    addGlobalMappings,
    -- * Translation
    Translatable, Generic,
    generateFunction,
    -- * Unsafe type conversion
    Unsafe,
    unsafeRemoveIO,
    -- * Simplified interface.
    simpleFunction,
    unsafeGenerateFunction,
    -- * Target information
    module LLVM.ExecutionEngine.Target,
    -- * Exchange data with JIT code in memory
    Marshal.Marshal(..),
    Marshal.MarshalVector(..),
    Marshal.sizeOf,
    Marshal.alignment,
    Marshal.StructFields,
    Marshal.sizeOfArray,
    Marshal.pokeList,
    Marshal.with,
    Marshal.alloca,
    Marshal.Stored(..),
    Marshal.castToStoredPtr,
    Marshal.castFromStoredPtr,
    ) where

import qualified LLVM.ExecutionEngine.Marshal as Marshal
import LLVM.ExecutionEngine.Engine
import LLVM.ExecutionEngine.Target
import LLVM.Core.CodeGen (Value(..))
import LLVM.Core
         (CodeGenModule, Function, newModule, defineModule, getGlobalMappings,
          setTarget, hostTriple)

import LLVM.FFI.Core (ValueRef)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (liftM2, )


-- |Class of LLVM function types that can be translated to the corresponding
-- Haskell type.
class Translatable f where
    translate :: (ValueRef -> [GenericValue] -> IO GenericValue) -> [GenericValue] -> ValueRef -> f

instance (Generic a, Translatable b) => Translatable (a -> b) where
    translate run args f = \ arg -> translate run (toGeneric arg : args) f

instance (Generic a) => Translatable (IO a) where
    translate run args f = fmap fromGeneric $ run f $ reverse args

-- |Generate a Haskell function from an LLVM function.
--
-- Note that the function is compiled for every call (Just-In-Time compilation).
-- If you want to compile the function once and call it a lot of times
-- then you should better use 'getPointerToFunction'.
generateFunction ::
    (Translatable f) =>
    Function f -> EngineAccess f
generateFunction (Value f) = do
    run <- getRunFunction
    return $ translate run [] f

class Unsafe a where
    type RemoveIO a
    unsafeRemoveIO :: a -> RemoveIO a  -- ^Remove the IO from a function return type.  This is unsafe in general.

instance (Unsafe b) => Unsafe (a->b) where
    type RemoveIO (a -> b) = a -> RemoveIO b
    unsafeRemoveIO f = unsafeRemoveIO . f

instance Unsafe (IO a) where
    type RemoveIO (IO a) = a
    unsafeRemoveIO = unsafePerformIO

-- |Translate a function to Haskell code.  This is a simplified interface to
-- the execution engine and module mechanism.
-- It is based on 'generateFunction', so see there for limitations.
simpleFunction :: (Translatable f) => CodeGenModule (Function f) -> IO f
simpleFunction bld = do
    m <- newModule
    (func, mappings) <-
        defineModule m $
            setTarget hostTriple >> liftM2 (,) bld getGlobalMappings
    runEngineAccessInterpreterWithModule m $ do
        addGlobalMappings mappings
        generateFunction func

-- | Combine 'simpleFunction' and 'unsafeRemoveIO'.
unsafeGenerateFunction :: (Unsafe t, Translatable t) =>
                          CodeGenModule (Function t) -> RemoveIO t
unsafeGenerateFunction bld = unsafePerformIO $ do
    fun <- simpleFunction bld
    return $ unsafeRemoveIO fun
