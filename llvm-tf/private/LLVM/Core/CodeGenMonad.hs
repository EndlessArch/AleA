{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    GlobalMappings(..), addGlobalMapping, getGlobalMappings,
    addFunctionMapping,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, liftCodeGenModule, genFSym,
    getFunction, getBuilder, getFunctionModule, lookupExtern, addExtern,
    ) where

import qualified LLVM.Core.Data as Data
import qualified LLVM.Core.Type as Type
import LLVM.Core.Util (Module, Builder, Function, getValueNameU, withModule, )

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.ExecutionEngine as EE

import Foreign.C.String (withCString, )
import Foreign.Ptr (FunPtr, nullPtr, )

import Control.Monad.Trans.State (StateT, runStateT, evalStateT, get, gets, put, modify, )
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Monad (when, )
import Control.Applicative (Applicative, )

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import Data.Typeable (Typeable)

--------------------------------------

data CGMState = CGMState {
    cgm_module :: Module,
    cgm_externs :: Map String Function,
    cgm_global_mappings :: GlobalMappings,
    cgm_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenModule a = CGM (StateT CGMState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

genMSym :: String -> CodeGenModule String
genMSym prefix = do
    s <- CGM get
    let n = cgm_next s
    CGM $ put (s { cgm_next = n + 1 })
    return $ "_" ++ prefix ++ show n

getModule :: CodeGenModule Module
getModule = CGM $ gets cgm_module

runCodeGenModule :: Module -> CodeGenModule a -> IO a
runCodeGenModule m (CGM body) =
    evalStateT body $
    CGMState {
        cgm_module = m, cgm_next = 1,
        cgm_externs = Map.empty, cgm_global_mappings = mempty
    }

--------------------------------------

data CGFState r = CGFState {
    cgf_module :: CGMState,
    cgf_builder :: Builder,
    cgf_function :: Function,
    cgf_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenFunction r a = CGF (StateT (CGFState r) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

genFSym :: CodeGenFunction a String
genFSym = do
    s <- CGF get
    let n = cgf_next s
    CGF $ put (s { cgf_next = n + 1 })
    return $ "_L" ++ show n

getFunction :: CodeGenFunction a Function
getFunction = CGF $ gets cgf_function

getBuilder :: CodeGenFunction a Builder
getBuilder = CGF $ gets cgf_builder

getFunctionModule :: CodeGenFunction a Module
getFunctionModule = CGF $ gets (cgm_module . cgf_module)

lookupExtern :: String -> CodeGenFunction a (Maybe Function)
lookupExtern name = CGF $ gets (Map.lookup name . cgm_externs . cgf_module)

addExtern :: String -> Function -> CodeGenFunction a ()
addExtern name func = CGF $ modify $ \cgf ->
    cgf {cgf_module = (cgf_module cgf)
            {cgm_externs =
                Map.insert name func (cgm_externs $ cgf_module cgf) } }


type Value = FFI.ValueRef

addGlobalMapping :: (Type.IsType a) => Value -> Data.Ptr a -> CodeGenModule ()
addGlobalMapping value ptr = CGM $
    addMappingToState $
        GlobalMappings (\ee ->
            EE.addGlobalMapping ee value $ Data.uncheckedToPtr ptr)

addFunctionMapping :: Function -> FunPtr f -> CodeGenModule ()
addFunctionMapping value func = CGM $ do
    {-
    We need to fetch the name from the value
    since it might have been disambiguized after adding.
    -}
    name <- liftIO $ getValueNameU value
    modul <- gets cgm_module
    addMappingToState $
        GlobalMappings $ \ee -> do
            {-
            Between adding and application
            the program may have been restructured by optimization passes.
            I have not seen that the optimizer alters a Function Value pointer,
            but the optimizer can remove an unused function.
            That would render the original value invalid.
            -}
            currentValue <-
                liftIO $
                    withCString name $ \cname ->
                    withModule modul $ \cmodule ->
                        FFI.getNamedFunction cmodule cname
            -- the optimizer could have removed the function
            when (currentValue/=nullPtr) $
                EE.addFunctionMapping ee currentValue func

addMappingToState :: GlobalMappings -> StateT CGMState IO ()
addMappingToState gm =
    modify $ \cgm ->
        cgm { cgm_global_mappings = cgm_global_mappings cgm <> gm }

newtype GlobalMappings =
    GlobalMappings (EE.ExecutionEngineRef -> IO ())

instance Show GlobalMappings where
    show _ = "GlobalMappings"

instance Semigroup GlobalMappings where
    GlobalMappings x <> GlobalMappings y =
        GlobalMappings (\ee -> x ee >> y ee)

instance Monoid GlobalMappings where
    mempty = GlobalMappings $ const $ return ()
    mappend = (<>)


{- |
Get a list created by calls to 'staticFunction'
that must be passed to the execution engine
via 'LLVM.ExecutionEngine.addGlobalMappings'.
-}
getGlobalMappings :: CodeGenModule GlobalMappings
getGlobalMappings = CGM $ gets cgm_global_mappings

runCodeGenFunction ::
    Builder -> Function -> CodeGenFunction r a -> CodeGenModule a
runCodeGenFunction bld fn (CGF body) = do
    cgm <- CGM get
    let cgf = CGFState { cgf_module = cgm,
                         cgf_builder = bld,
                         cgf_function = fn,
                         cgf_next = 1 }
    (a, cgf') <- liftIO $ runStateT body cgf
    CGM $ put (cgf_module cgf')
    return a

--------------------------------------

-- | Allows you to define part of a module while in the middle of defining a function.
liftCodeGenModule :: CodeGenModule a -> CodeGenFunction r a
liftCodeGenModule (CGM act) = do
    cgf <- CGF get
    (a, cgm') <- liftIO $ runStateT act (cgf_module cgf)
    CGF $ put (cgf { cgf_module = cgm' })
    return a
