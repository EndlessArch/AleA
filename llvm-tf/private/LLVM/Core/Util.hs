{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLVM.Core.Util(
    -- * Module handling
    Module(..), withModule, createModule, destroyModule, writeBitcodeToFile, readBitcodeFromFile,
    getModuleValues, getFunctions, getGlobalVariables, valueHasType,
    -- * Pass manager handling
    PassManager(..), withPassManager, createPassManager, createFunctionPassManager,
    runFunctionPassManager, initializeFunctionPassManager, finalizeFunctionPassManager,
    -- * Instruction builder
    Builder(..), withBuilder, createBuilder, positionAtEnd, getInsertBlock,
    -- * Basic blocks
    BasicBlock,
    appendBasicBlock, getBasicBlocks,
    -- * Functions
    Function,
    addFunction, getParam, getParams,
    -- * Structs
    structType,
    -- * Globals
    addGlobal,
    constString, constStringNul, constVector, constArray, constStruct,
    -- * Instructions
    makeCall, makeInvoke,
    makeCallWithCc, makeInvokeWithCc,
    withValue, getInstructions, getOperands,
    -- * Uses and Users
    hasUsers, getUsers, getUses, getUser, isChildOf, getDep,
    -- * Misc
    CString, withArrayLen,
    withEmptyCString,
    functionType, buildEmptyPhi, addPhiIns,
    showTypeOf, getValueNameU, getObjList, annotateValueList,
    isConstant, isIntrinsic,
    -- * Transformation passes
    addCFGSimplificationPass, addDemoteMemoryToRegisterPass,
    addGVNPass, addInstructionCombiningPass, addPromoteMemoryToRegisterPass, addReassociatePass,
    ) where

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitWriter as FFI
import qualified LLVM.FFI.BitReader as FFI
import qualified LLVM.FFI.Transforms.Scalar as FFI

import Foreign.C.String (withCString, withCStringLen, CString, peekCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Typeable (Typeable)
import Data.List (intercalate)
import Control.Monad (liftM, when)


type Type = FFI.TypeRef

functionType :: Bool -> Type -> [Type] -> IO Type
functionType varargs retType paramTypes =
    withArrayLen paramTypes $ \ len ptr ->
        FFI.functionType retType ptr (fromIntegral len) (FFI.consBool varargs)

structType :: [Type] -> Bool -> IO Type
structType types packed =
    withArrayLen types $ \ len ptr ->
        FFI.structType ptr (fromIntegral len) (FFI.consBool packed)

--------------------------------------
-- Handle modules

-- Don't use a finalizer for the module, but instead provide an
-- explicit destructor.  This is because handing a module to
-- a module provider changes ownership of the module to the provider,
-- and we don't want to free it by mistake.

-- | Type of top level modules.
newtype Module = Module {
      fromModule :: FFI.ModuleRef
    }
    deriving (Show, Typeable)

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule modul f = f (fromModule modul)

createModule :: String -> IO Module
createModule name =
    withCString name $ \ namePtr -> do
      liftM Module $ FFI.moduleCreateWithName namePtr

-- | Free all storage related to a module.  *Note*, this is a dangerous call, since referring
-- to the module after this call is an error.  The reason for the explicit call to free
-- the module instead of an automatic lifetime management is that modules have a
-- somewhat complicated ownership.  Handing a module to a module provider changes
-- the ownership of the module, and the module provider will free the module when necessary.
destroyModule :: Module -> IO ()
destroyModule = FFI.disposeModule . fromModule

-- |Write a module to a file.
writeBitcodeToFile :: String -> Module -> IO ()
writeBitcodeToFile name mdl =
    withCString name $ \ namePtr ->
      withModule mdl $ \ mdlPtr -> do
        rc <- FFI.writeBitcodeToFile mdlPtr namePtr
        when (rc /= 0) $
          ioError $ userError $ "writeBitcodeToFile: return code " ++ show rc

-- |Read a module from a file.
readBitcodeFromFile :: String -> IO Module
readBitcodeFromFile name =
    withCString name $ \ namePtr ->
      alloca $ \ bufPtr ->
      alloca $ \ modPtr ->
      alloca $ \ errStr -> do
        rrc <- FFI.createMemoryBufferWithContentsOfFile namePtr bufPtr errStr
        if FFI.deconsBool rrc then do
            msg <- peek errStr >>= peekCString
            ioError $ userError $ "readBitcodeFromFile: read return code " ++ show rrc ++ ", " ++ msg
         else do
            buf <- peek bufPtr
            prc <- FFI.parseBitcode buf modPtr errStr
            if FFI.deconsBool prc then do
                msg <- peek errStr >>= peekCString
                ioError $ userError $ "readBitcodeFromFile: parse return code " ++ show prc ++ ", " ++ msg
             else do
                ptr <- peek modPtr
                return $ Module ptr
{-
                liftM Module $ newForeignPtr FFI.ptrDisposeModule ptr
-}

getModuleValues :: Module -> IO [(String, Value)]
getModuleValues mdl = do
  fs <- getFunctions mdl
  gs <- getGlobalVariables mdl
  return (fs ++ gs)

getFunctions :: Module -> IO [(String, Value)]
getFunctions mdl =
    getObjList withModule FFI.getFirstFunction FFI.getNextFunction mdl
      >>= annotateValueList

getGlobalVariables :: Module -> IO [(String, Value)]
getGlobalVariables mdl =
    getObjList withModule FFI.getFirstGlobal FFI.getNextGlobal mdl
      >>= annotateValueList

-- This is safe because we just ask for the type of a value.
valueHasType :: Value -> Type -> Bool
valueHasType v t = unsafePerformIO $ do
    vt <- FFI.typeOf v
    return $ vt == t  -- LLVM uses hash consing for types, so pointer equality works.

showTypeOf :: Value -> IO String
showTypeOf v = FFI.typeOf v >>= showType'

showType' :: Type -> IO String
showType' p = do
    pk <- FFI.getTypeKind p
    case pk of
        FFI.VoidTypeKind -> return "()"
        FFI.FloatTypeKind -> return "Float"
        FFI.DoubleTypeKind -> return "Double"
        FFI.X86_FP80TypeKind -> return "X86_FP80"
        FFI.FP128TypeKind -> return "FP128"
        FFI.PPC_FP128TypeKind -> return "PPC_FP128"
        FFI.LabelTypeKind -> return "Label"
        FFI.IntegerTypeKind -> do w <- FFI.getIntTypeWidth p; return $ "(IntN " ++ show w ++ ")"
        FFI.FunctionTypeKind -> do
            r <- FFI.getReturnType p
            c <- FFI.countParamTypes p
            let n = fromIntegral c
            as <- allocaArray n $ \ args -> do
                     FFI.getParamTypes p args
                     peekArray n args
            ts <- mapM showType' (as ++ [r])
            return $ "(" ++ intercalate " -> " ts ++ ")"
        FFI.StructTypeKind -> return "(Struct ...)"
        FFI.ArrayTypeKind -> do n <- FFI.getArrayLength p; t <- FFI.getElementType p >>= showType'; return $ "(Array " ++ show n ++ " " ++ t ++ ")"
        FFI.PointerTypeKind -> do t <- FFI.getElementType p >>= showType'; return $ "(Ptr " ++ t ++ ")"
        FFI.OpaqueTypeKind -> return "Opaque"
        FFI.VectorTypeKind -> do n <- FFI.getVectorSize p; t <- FFI.getElementType p >>= showType'; return $ "(Vector " ++ show n ++ " " ++ t ++ ")"

--------------------------------------
-- Handle instruction builders

newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }
    deriving (Show, Typeable)

withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . fromBuilder

createBuilder :: IO Builder
createBuilder = do
    ptr <- FFI.createBuilder
    liftM Builder $ newForeignPtr FFI.ptrDisposeBuilder ptr

positionAtEnd :: Builder -> FFI.BasicBlockRef -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \ bldPtr ->
      FFI.positionAtEnd bldPtr bblk

getInsertBlock :: Builder -> IO FFI.BasicBlockRef
getInsertBlock bld =
    withBuilder bld $ \ bldPtr ->
      FFI.getInsertBlock bldPtr

--------------------------------------

type BasicBlock = FFI.BasicBlockRef

appendBasicBlock :: Function -> String -> IO BasicBlock
appendBasicBlock func name =
    withCString name $ \ namePtr ->
      FFI.appendBasicBlock func namePtr

getBasicBlocks :: Value -> IO [(String, BasicBlock)]
getBasicBlocks v =
    getObjList withValue FFI.getFirstBasicBlock FFI.getNextBasicBlock v
      >>= annotateBasicBlockList

--------------------------------------

type Function = FFI.ValueRef

addFunction :: Module -> FFI.Linkage -> String -> Type -> IO Function
addFunction modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        f <- FFI.addFunction modulPtr namePtr typ
        FFI.setLinkage f (FFI.fromLinkage linkage)
        return f

getParam :: Function -> Int -> Value
getParam f = unsafePerformIO . FFI.getParam f . fromIntegral

getParams :: Value -> IO [(String, Value)]
getParams v =
    getObjList withValue FFI.getFirstParam FFI.getNextParam v
      >>= annotateValueList

--------------------------------------

addGlobal :: Module -> FFI.Linkage -> String -> Type -> IO Value
addGlobal modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        v <- FFI.addGlobal modulPtr typ namePtr
        FFI.setLinkage v (FFI.fromLinkage linkage)
        return v

-- unsafePerformIO is safe because it's only used for the withCStringLen conversion
constStringInternal :: Bool -> String -> Value
constStringInternal nulTerm s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      FFI.constString sPtr (fromIntegral sLen) (FFI.consBool (not nulTerm))

constString :: String -> Value
constString = constStringInternal False

constStringNul :: String -> Value
constStringNul = constStringInternal True

--------------------------------------

type Value = FFI.ValueRef

withValue :: Value -> (Value -> IO a) -> IO a
withValue v f = f v

withBasicBlock :: FFI.BasicBlockRef -> (FFI.BasicBlockRef -> IO a) -> IO a
withBasicBlock v f = f v

makeCall :: Function -> FFI.BuilderRef -> [Value] -> IO Value
makeCall = makeCallWithCc FFI.C

makeCallWithCc :: FFI.CallingConvention -> Function -> FFI.BuilderRef -> [Value] -> IO Value
makeCallWithCc cc func bldPtr args = do
{-
      print "makeCall"
      FFI.dumpValue func
      mapM_ FFI.dumpValue args
      print "----------------------"
-}
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ \cstr -> do
          i <- FFI.buildCall bldPtr func argPtr
                             (fromIntegral argLen) cstr
          FFI.setInstructionCallConv i (FFI.fromCallingConvention cc)
          return i

makeInvoke :: BasicBlock -> BasicBlock -> Function -> FFI.BuilderRef ->
              [Value] -> IO Value
makeInvoke = makeInvokeWithCc FFI.C

makeInvokeWithCc :: FFI.CallingConvention -> BasicBlock -> BasicBlock -> Function -> FFI.BuilderRef ->
              [Value] -> IO Value
makeInvokeWithCc cc norm expt func bldPtr args =
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ \cstr -> do
          i <- FFI.buildInvoke bldPtr func argPtr (fromIntegral argLen) norm expt cstr
          FFI.setInstructionCallConv i (FFI.fromCallingConvention cc)
          return i

getInstructions :: BasicBlock -> IO [(String, Value)]
getInstructions bb =
    getObjList withBasicBlock FFI.getFirstInstruction FFI.getNextInstruction bb
      >>= annotateValueList

getOperands :: Value -> IO [(String, Value)]
getOperands ii = geto ii >>= annotateValueList
    where geto i = do
            num <- FFI.getNumOperands i
            let oloop instr number total = if number >= total then return [] else do
                    o <- FFI.getOperand instr number
                    os <- oloop instr (number + 1) total
                    return (o : os)
            oloop i 0 num

--------------------------------------

buildEmptyPhi :: FFI.BuilderRef -> Type -> IO Value
buildEmptyPhi bldPtr typ = do
    withEmptyCString $ FFI.buildPhi bldPtr typ

withEmptyCString :: (CString -> IO a) -> IO a
withEmptyCString = withCString ""

addPhiIns :: Value -> [(Value, BasicBlock)] -> IO ()
addPhiIns inst incoming = do
    let (vals, bblks) = unzip incoming
    withArrayLen vals $ \ count valPtr ->
      withArray bblks $ \ bblkPtr ->
        FFI.addIncoming inst valPtr bblkPtr (fromIntegral count)

--------------------------------------

-- | Manage compile passes.
newtype PassManager = PassManager {
      fromPassManager :: ForeignPtr FFI.PassManager
    }
    deriving (Show, Typeable)

withPassManager :: PassManager -> (FFI.PassManagerRef -> IO a)
                   -> IO a
withPassManager = withForeignPtr . fromPassManager

-- | Create a pass manager.
createPassManager :: IO PassManager
createPassManager = do
    ptr <- FFI.createPassManager
    liftM PassManager $ newForeignPtr FFI.ptrDisposePassManager ptr

-- | Create a pass manager for a module.
createFunctionPassManager :: Module -> IO PassManager
createFunctionPassManager modul =
    withModule modul $ \modulPtr -> do
        ptr <- FFI.createFunctionPassManagerForModule modulPtr
        liftM PassManager $ newForeignPtr FFI.ptrDisposePassManager ptr

-- | Add a control flow graph simplification pass to the manager.
addCFGSimplificationPass :: PassManager -> IO ()
addCFGSimplificationPass pm = withPassManager pm FFI.addCFGSimplificationPass

addDemoteMemoryToRegisterPass :: PassManager -> IO ()
addDemoteMemoryToRegisterPass pm = withPassManager pm FFI.addDemoteMemoryToRegisterPass

-- | Add a global value numbering pass to the manager.
addGVNPass :: PassManager -> IO ()
addGVNPass pm = withPassManager pm FFI.addGVNPass

addInstructionCombiningPass :: PassManager -> IO ()
addInstructionCombiningPass pm = withPassManager pm FFI.addInstructionCombiningPass

addPromoteMemoryToRegisterPass :: PassManager -> IO ()
addPromoteMemoryToRegisterPass pm = withPassManager pm FFI.addPromoteMemoryToRegisterPass

addReassociatePass :: PassManager -> IO ()
addReassociatePass pm = withPassManager pm FFI.addReassociatePass

runFunctionPassManager :: PassManager -> Function -> IO FFI.Bool
runFunctionPassManager pm fcn = withPassManager pm $ \ pmref -> FFI.runFunctionPassManager pmref fcn

initializeFunctionPassManager :: PassManager -> IO FFI.Bool
initializeFunctionPassManager pm = withPassManager pm FFI.initializeFunctionPassManager

finalizeFunctionPassManager :: PassManager -> IO FFI.Bool
finalizeFunctionPassManager pm = withPassManager pm FFI.finalizeFunctionPassManager

--------------------------------------

constVector :: [Value] -> IO Value
constVector xs = do
    withArrayLen xs $ \ len ptr ->
        FFI.constVector ptr (fromIntegral len)

constArray :: Type -> [Value] -> IO Value
constArray t xs = do
    withArrayLen xs $ \ len ptr ->
        FFI.constArray t ptr (fromIntegral len)

constStruct :: [Value] -> Bool -> IO Value
constStruct xs packed = do
    withArrayLen xs $ \ len ptr ->
        FFI.constStruct ptr (fromIntegral len) (FFI.consBool packed)

--------------------------------------

getValueNameU :: Value -> IO String
getValueNameU a = do
    -- sometimes void values need explicit names too
    str <- peekCString =<< FFI.getValueName a
    if str == "" then return (show a) else return str

getBasicBlockNameU :: BasicBlock -> IO String
getBasicBlockNameU a = do
    str <- peekCString =<< FFI.getBasicBlockName a
    if str == "" then return (show a) else return str

getObjList ::
    (obj -> (objPtr -> IO [Ptr a]) -> io) -> (objPtr -> IO (Ptr a)) ->
    (Ptr a -> IO (Ptr a)) -> obj -> io
getObjList withF firstF nextF obj =
    withF obj $ \ objPtr -> do
      let oloop p =
            if p == nullPtr
              then return []
              else fmap (p:) $ oloop =<< nextF p
      oloop =<< firstF objPtr

annotateValueList :: [Value] -> IO [(String, Value)]
annotateValueList vs = do
  names <- mapM getValueNameU vs
  return $ zip names vs

annotateBasicBlockList :: [BasicBlock] -> IO [(String, BasicBlock)]
annotateBasicBlockList vs = do
  names <- mapM getBasicBlockNameU vs
  return $ zip names vs

isConstant :: Value -> IO Bool
isConstant v = fmap FFI.deconsBool $ FFI.isConstant v

isIntrinsic :: Value -> IO Bool
isIntrinsic v = fmap (/=0) $ FFI.getIntrinsicID v

--------------------------------------

type Use = FFI.UseRef

hasUsers :: Value -> IO Bool
hasUsers v = fmap (>0) $ FFI.getNumUses v

getUses :: Value -> IO [Use]
getUses = getObjList withValue FFI.getFirstUse FFI.getNextUse

getUsers :: [Use] -> IO [(String, Value)]
getUsers us = mapM FFI.getUser us >>= annotateValueList

getUser :: Use -> IO Value
getUser = FFI.getUser

isChildOf :: BasicBlock -> Value -> IO Bool
isChildOf bb v = do
  bb2 <- FFI.getInstructionParent v
  return $ bb == bb2

getDep :: Use -> IO (String, String)
getDep u = do
  producer <- FFI.getUsedValue u >>= getValueNameU
  consumer <- FFI.getUser u >>= getValueNameU
  return (producer, consumer)
