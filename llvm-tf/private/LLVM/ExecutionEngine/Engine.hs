{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLVM.ExecutionEngine.Engine(
       EngineAccess,
       ExecutionEngine(..),
       getEngine,
       runEngineAccess, runEngineAccessWithModule,
       runEngineAccessInterpreterWithModule,
       getExecutionEngineTargetData,
       ExecutionFunction,
       Importer,
       getExecutionFunction,
       getPointerToFunction,
       addModule,
       addFunctionValue, addGlobalMappings,
       runFunction, getRunFunction,
       GenericValue, Generic(..)
       ) where

import qualified LLVM.Core.Proxy as Proxy
import qualified LLVM.Core.Data as Data
import qualified LLVM.Core.Util as U

import LLVM.Core.CodeGen (Value(..), Function)
import LLVM.Core.CodeGenMonad (GlobalMappings(..))
import LLVM.Core.Util (Module, withModule, createModule)
import LLVM.Core.Type (IsFirstClass, typeRef)
import LLVM.Core.Proxy (Proxy(Proxy))

import qualified LLVM.FFI.ExecutionEngine as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.FFI.Core as FFI (consBool, deconsBool, )

import qualified Control.Monad.Trans.Reader as MR
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Monad (liftM, )
import Control.Applicative (Applicative, pure, (<*>), (<$>), )

import qualified Data.EnumBitSet as EnumSet
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)

import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.ForeignPtr
         (ForeignPtr, newForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr, FunPtr, )
import Foreign.Storable (peek)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, castPtrToStablePtr, )
import System.IO.Unsafe (unsafePerformIO)


newtype
    ExecutionEngine = ExecutionEngine {
        fromEngine :: ForeignPtr FFI.ExecutionEngine
    }

withEngine :: ExecutionEngine -> (FFI.ExecutionEngineRef -> IO a) -> IO a
withEngine = withForeignPtr . fromEngine

createExecutionEngineForModule ::
    Bool -> FFI.EngineKindSet -> Module -> IO ExecutionEngine
createExecutionEngineForModule hostCPU kind m =
    alloca $ \eePtr ->
        alloca $ \errPtr -> do
          success <-
            withModule m $ \mPtr ->
              if hostCPU
                then
                  FFI.createExecutionEngineKindForModuleCPU
                    eePtr kind mPtr errPtr
                else
                  if EnumSet.get FFI.JIT kind
                    then FFI.createExecutionEngineForModule eePtr mPtr errPtr
                    else FFI.createInterpreterForModule eePtr mPtr errPtr
          if FFI.deconsBool success
            then ioError . userError =<< bracket (peek errPtr) free peekCString
            else
                liftM ExecutionEngine $
                    newForeignPtr FFI.ptrDisposeExecutionEngine =<<
                    peek eePtr

getTheEngine :: FFI.EngineKindSet -> Module -> IO ExecutionEngine
getTheEngine = createExecutionEngineForModule True

newtype EngineAccess a = EA (MR.ReaderT ExecutionEngine IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- |The LLVM execution engine is encapsulated so it cannot be accessed directly.
-- The reason is that (currently) there must only ever be one engine,
-- so access to it is wrapped in a monad.
runEngineAccess :: EngineAccess a -> IO a
runEngineAccess (EA body) = do
    MR.runReaderT body =<< getTheEngine FFI.kindEither =<< createModule "__empty__"

runEngineAccessWithModule :: Module -> EngineAccess a -> IO a
runEngineAccessWithModule m (EA body) = do
    MR.runReaderT body =<< getTheEngine FFI.kindEither m

runEngineAccessInterpreterWithModule :: Module -> EngineAccess a -> IO a
runEngineAccessInterpreterWithModule m (EA body) = do
    MR.runReaderT body =<< getTheEngine FFI.kindInterpreter m


getEngine :: EngineAccess ExecutionEngine
getEngine = EA MR.ask

accessEngine :: (FFI.ExecutionEngineRef -> IO a) -> EngineAccess a
accessEngine act = do
    engine <- getEngine
    liftIO $ withEngine engine act

getExecutionEngineTargetData :: EngineAccess FFI.TargetDataRef
getExecutionEngineTargetData =
    accessEngine FFI.getExecutionEngineTargetData

{- |
In contrast to 'generateFunction' this compiles a function once.
Thus it is faster for many calls to the same function.
See @examples\/Vector.hs@.

If the function calls back into Haskell code,
you also have to set the function addresses
using 'addFunctionValue' or 'addGlobalMappings'.

You must keep the execution engine alive
as long as you want to call the function.
Better use 'getExecutionFunction' which handles this for you.
-}
getPointerToFunction :: Function f -> EngineAccess (FunPtr f)
getPointerToFunction (Value f) =
    accessEngine $ \eePtr -> FFI.getPointerToFunction eePtr f

class ExecutionFunction f where
    keepAlive :: ExecutionEngine -> f -> f

instance ExecutionFunction (IO a) where
    keepAlive engine act = do
        a <- act
        touchForeignPtr (fromEngine engine)
        return a

instance ExecutionFunction f => ExecutionFunction (a -> f) where
    keepAlive engine act = keepAlive engine . act

type Importer f = FunPtr f -> f

getExecutionFunction ::
    (ExecutionFunction f) => Importer f -> Function f -> EngineAccess f
getExecutionFunction importer (Value f) = do
    engine <- getEngine
    liftIO $ withEngine engine $ \eePtr ->
        keepAlive engine . importer <$> FFI.getPointerToFunction eePtr f

{- |
Tell LLVM the address of an external function
if it cannot resolve a name automatically.
Alternatively you may declare the function
with 'staticFunction' instead of 'externFunction'.
-}
addFunctionValue :: Function f -> FunPtr f -> EngineAccess ()
addFunctionValue (Value g) f =
    accessEngine $ \eePtr -> FFI.addFunctionMapping eePtr g f

{- |
Pass a list of global mappings to LLVM
that can be obtained from 'LLVM.Core.getGlobalMappings'.
-}
addGlobalMappings :: GlobalMappings -> EngineAccess ()
addGlobalMappings (GlobalMappings gms) = accessEngine gms

addModule :: Module -> EngineAccess ()
addModule m =
    accessEngine $ \eePtr -> U.withModule m $ FFI.addModule eePtr


--------------------------------------

newtype GenericValue = GenericValue {
      fromGenericValue :: ForeignPtr FFI.GenericValue
    }

withGenericValue :: GenericValue -> (FFI.GenericValueRef -> IO a) -> IO a
withGenericValue = withForeignPtr . fromGenericValue

createGenericValueWith :: IO FFI.GenericValueRef -> IO GenericValue
createGenericValueWith f = do
  ptr <- f
  liftM GenericValue $ newForeignPtr FFI.ptrDisposeGenericValue ptr

withAll :: [GenericValue] -> (Int -> Ptr FFI.GenericValueRef -> IO a) -> IO a
withAll ps a = go [] ps
    where go ptrs (x:xs) = withGenericValue x $ \ptr -> go (ptr:ptrs) xs
          go ptrs _ = withArrayLen (reverse ptrs) a

runFunction :: U.Function -> [GenericValue] -> EngineAccess GenericValue
runFunction func args =
    liftIO =<< getRunFunction <*> pure func <*> pure args

getRunFunction :: EngineAccess (U.Function -> [GenericValue] -> IO GenericValue)
getRunFunction = do
    engine <- getEngine
    return $ \ func args ->
             withAll args $ \argLen argPtr ->
             withEngine engine $ \eePtr ->
                 createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr

class Generic a where
    toGeneric :: a -> GenericValue
    fromGeneric :: GenericValue -> a

instance Generic () where
    toGeneric _ = error "toGeneric ()"
    fromGeneric _ = ()

toGenericInt :: (Integral a, IsFirstClass a) => Bool -> a -> GenericValue
toGenericInt signed val = unsafePerformIO $ createGenericValueWith $ do
    typ <- typeRef $ Proxy.fromValue val
    FFI.createGenericValueOfInt
        typ (fromIntegral val) (FFI.consBool signed)

fromGenericInt :: (Integral a, IsFirstClass a) => Bool -> GenericValue -> a
fromGenericInt signed val = unsafePerformIO $
    withGenericValue val $ \ref ->
        fmap fromIntegral $ FFI.genericValueToInt ref (FFI.consBool signed)

--instance Generic Bool where
--    toGeneric = toGenericInt False . FFI.consBool
--    fromGeneric = toBool . fromGenericInt False

instance Generic Int where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int8 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int16 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int32 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int64 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Word where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word8 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word16 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word32 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word64 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

toGenericReal :: (Real a, IsFirstClass a) => a -> GenericValue
toGenericReal val = unsafePerformIO $ createGenericValueWith $ do
    typ <- typeRef $ Proxy.fromValue val
    FFI.createGenericValueOfFloat typ (realToFrac val)

fromGenericReal :: forall a . (Fractional a, IsFirstClass a) => GenericValue -> a
fromGenericReal val = unsafePerformIO $
    withGenericValue val $ \ ref -> do
        typ <- typeRef (Proxy :: Proxy a)
        fmap realToFrac $ FFI.genericValueToFloat typ ref

instance Generic Float where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic Double where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic (Data.Ptr a) where
    toGeneric =
        unsafePerformIO . createGenericValueWith .
        FFI.createGenericValueOfPointer . Data.uncheckedToPtr
    fromGeneric val =
        Data.uncheckedFromPtr . unsafePerformIO . withGenericValue val $
        FFI.genericValueToPointer

instance Generic (Ptr a) where
    toGeneric =
        unsafePerformIO . createGenericValueWith .
        FFI.createGenericValueOfPointer
    fromGeneric val =
        unsafePerformIO . withGenericValue val $ FFI.genericValueToPointer

instance Generic (StablePtr a) where
    toGeneric =
        unsafePerformIO . createGenericValueWith .
        FFI.createGenericValueOfPointer . castStablePtrToPtr
    fromGeneric val =
        unsafePerformIO . fmap castPtrToStablePtr . withGenericValue val $
        FFI.genericValueToPointer
