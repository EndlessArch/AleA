{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module LLVM.Core.CodeGen(
    -- * Module creation
    newModule, newNamedModule, defineModule, createModule, createNamedModule,
    getModuleValues, ModuleValue, castModuleValue, setTarget, setDataLayout,
    -- * Globals
    Linkage(..),
    Visibility(..),
    -- * Function creation
    Function, newFunction, newNamedFunction, defineFunction,
    createFunction, createNamedFunction, setFuncCallConv, functionParameter,
    addAttributes,
    FFI.AttributeIndex(..), Attribute(..),
    externFunction, staticFunction, staticNamedFunction,
    FunctionArgs, FunctionCodeGen, FunctionResult,
    TFunction,
    CodeValue, CodeResult,
    -- * Global variable creation
    Global, newGlobal, newNamedGlobal,
    defineGlobal, createGlobal, createNamedGlobal, TGlobal,
    externGlobal, staticGlobal,
    -- * Values
    Value(..), ConstValue(..), UnValue,
    IsConst(..), valueOf, value,
    IsConstFields,
    zero, allOnes, undef,
    createString, createStringNul,
    withString, withStringNul,
    constVector, constArray, constStruct, constPackedStruct,
    constCyclicVector, constCyclicArray,
    -- * Basic blocks
    BasicBlock(..), newBasicBlock, newNamedBasicBlock,
    defineBasicBlock, createBasicBlock, getCurrentBasicBlock,
    fromLabel, toLabel,
    -- * Misc
    withCurrentBuilder
    ) where

import qualified LLVM.Core.UnaryVector as UnaryVector
import qualified LLVM.Core.Util as U
import qualified LLVM.Core.Data as Data
import qualified LLVM.Core.Proxy as LP
import LLVM.Core.CodeGenMonad
import LLVM.Core.Type
import LLVM.Core.Data hiding (Ptr)

import qualified LLVM.FFI.Core.Attribute as Attr
import qualified LLVM.FFI.Core as FFI
import LLVM.FFI.Core(Linkage(..), Visibility(..))

import qualified Type.Data.Num.Decimal.Proof as DecProof
import qualified Type.Data.Num.Decimal.Number as Dec
import qualified Type.Data.Num.Unary as Un
import Type.Base.Proxy (Proxy)

import qualified Foreign
import Foreign.C.String (withCString, withCStringLen)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr)
import Foreign.Ptr (FunPtr, castFunPtrToPtr)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, when)
import Control.Applicative ((<*>))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64, Word)
import Data.Tuple.HT (mapSnd)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)

import Text.Printf (printf)

--------------------------------------

-- | Create a new module.
newModule :: IO U.Module
newModule = newNamedModule "_module"  -- XXX should generate a name

-- | Create a new explicitely named module.
newNamedModule :: String              -- ^ module name
               -> IO U.Module
newNamedModule = U.createModule

-- | Give the body for a module.
defineModule :: U.Module              -- ^ module that is defined
             -> CodeGenModule a       -- ^ module body
             -> IO a
defineModule = runCodeGenModule

-- | Create a new module with the given body.
createModule :: CodeGenModule a       -- ^ module body
             -> IO a
createModule cgm = newModule >>= \ m -> defineModule m cgm

-- | Create a new explicitly named module with the given body.
createNamedModule :: String              -- ^ module name
                  -> CodeGenModule a     -- ^ module body
                  -> IO a
createNamedModule name cgm = newNamedModule name >>= \ m -> defineModule m cgm

setTarget :: String -> CodeGenModule ()
setTarget triple = do
    modul <- getModule
    liftIO $ U.withModule modul $ \m -> withCString triple $ FFI.setTarget m

setDataLayout :: String -> CodeGenModule ()
setDataLayout layout = do
    modul <- getModule
    liftIO $ U.withModule modul $ \m -> withCString layout $ FFI.setDataLayout m


--------------------------------------

newtype ModuleValue = ModuleValue FFI.ValueRef
    deriving (Show, Typeable)

getModuleValues :: U.Module -> IO [(String, ModuleValue)]
getModuleValues =
    liftM (map (\ (s,p) -> (s, ModuleValue p))) . U.getModuleValues

castModuleValue :: forall a . (IsType a) => ModuleValue -> Maybe (Value a)
castModuleValue (ModuleValue f) =
    toMaybe (U.valueHasType f (unsafeTypeRef (LP.Proxy :: LP.Proxy a))) (Value f)

--------------------------------------

newtype Value a = Value { unValue :: FFI.ValueRef }
    deriving (Show, Typeable)

newtype ConstValue a = ConstValue { unConstValue :: FFI.ValueRef }
    deriving (Show, Typeable)

-- XXX merge with IsArithmetic?
class IsConst a where
    constOf :: a -> ConstValue a

instance IsConst Bool   where constOf = constEnum (typeRef (LP.Proxy :: LP.Proxy Bool))
--instance IsConst Char   where constOf = constEnum (typeRef (0::Word8)) -- XXX Unicode
instance IsConst Word   where constOf = constI
instance IsConst Word8  where constOf = constI
instance IsConst Word16 where constOf = constI
instance IsConst Word32 where constOf = constI
instance IsConst Word64 where constOf = constI
instance IsConst Int    where constOf = constI
instance IsConst Int8   where constOf = constI
instance IsConst Int16  where constOf = constI
instance IsConst Int32  where constOf = constI
instance IsConst Int64  where constOf = constI
instance IsConst Float  where constOf = constF
instance IsConst Double where constOf = constF
--instance IsConst FP128  where constOf = constF

instance (Dec.Positive n) => IsConst (WordN n) where
    constOf (WordN i) = constInteger i
instance (Dec.Positive n) => IsConst (IntN n) where
    constOf (IntN i) = constInteger i

constOfPtr :: (IsType ptr) => ptr -> Foreign.Ptr b -> ConstValue ptr
constOfPtr proto p =
    let ip = p `Foreign.minusPtr` Foreign.nullPtr
        inttoptrC :: ConstValue int -> ConstValue ptr
        inttoptrC (ConstValue v) =
           unsafeConstValue $
           FFI.constIntToPtr v $ unsafeTypeRef $ LP.fromValue proto
    in  inttoptrC $ constOf ip

-- This instance doesn't belong here, but mutually recursive modules are painful.
instance IsConst (Foreign.Ptr a) where
    constOf p = constOfPtr p p

instance (IsType a) => IsConst (Data.Ptr a) where
    constOf p = constOfPtr p (Data.uncheckedToPtr p)

instance (IsFunction a) => IsConst (FunPtr a) where
    constOf p = constOfPtr p (castFunPtrToPtr p)

instance IsConst (StablePtr a) where
    constOf p = constOfPtr p (castStablePtrToPtr p)

instance (IsPrimitive a, IsConst a, Dec.Positive n) => IsConst (Vector n a) where
    constOf (Vector x) = constVectorGen constOf x

instance (IsConst a, IsSized a, Dec.Natural n) => IsConst (Array n a) where
    constOf (Array xs) = constArray (map constOf xs)

instance (IsConstFields a) => IsConst (Struct a) where
    constOf (Struct a) =
        unsafeConstValue $ U.constStruct (constFieldsOf a) False
instance (IsConstFields a) => IsConst (PackedStruct a) where
    constOf (PackedStruct a) =
        unsafeConstValue $ U.constStruct (constFieldsOf a) True

class IsConstFields a where
    constFieldsOf :: a -> [FFI.ValueRef]

instance (IsConst a, IsConstFields as) => IsConstFields (a, as) where
    constFieldsOf (a, as) = unConstValue (constOf a) : constFieldsOf as
instance IsConstFields () where
    constFieldsOf _ = []


unsafeConstValue :: IO FFI.ValueRef -> ConstValue a
unsafeConstValue =
    ConstValue . unsafePerformIO

unsafeWithConstValue ::
    forall a.
    (IsType a) =>
    (FFI.TypeRef -> IO FFI.ValueRef) ->
    ConstValue a
unsafeWithConstValue f =
    unsafePerformIO $ fmap ConstValue $
        f =<< typeRef (LP.Proxy :: LP.Proxy a)

constEnum :: (Enum a) => IO FFI.TypeRef -> a -> ConstValue a
constEnum mt i =
    unsafeConstValue $ mt >>= \t ->
        FFI.constInt t (fromIntegral $ fromEnum i) FFI.false

{-
ToDo:
Passes a BigInt as decimal number string.
Not very efficient but quite generic.
Maybe Hex is better?
-}
constInteger :: (IsType (intN n)) => Integer -> ConstValue (intN n)
constInteger i =
    unsafeWithConstValue $ \typ ->
    withCString (show i) $ \cstr ->
    FFI.constIntOfString typ cstr 10

constI :: (IsInteger a, Integral a) => a -> ConstValue a
constI i =
    unsafeWithConstValue $ \typ ->
    FFI.constInt typ (fromIntegral i) (FFI.consBool $ isSigned $ LP.fromValue i)

constF :: (IsFloating a, Real a) => a -> ConstValue a
constF i =
    unsafeWithConstValue $ \typ -> FFI.constReal typ (realToFrac i)

valueOf :: (IsConst a) => a -> Value a
valueOf = value . constOf

value :: ConstValue a -> Value a
value (ConstValue a) = Value a

zero :: forall a . (IsType a) => ConstValue a
zero = unsafeWithConstValue FFI.constNull

allOnes :: forall a . (IsInteger a) => ConstValue a
allOnes = unsafeWithConstValue FFI.constAllOnes

undef :: forall a . (IsType a) => ConstValue a
undef = unsafeWithConstValue FFI.getUndef

{-
createString :: String -> ConstValue (DynamicArray Word8)
createString = ConstValue . U.constString

constStringNul :: String -> ConstValue (DynamicArray Word8)
constStringNul = ConstValue . U.constStringNul
-}

--------------------------------------


-- |A function is simply a pointer to the function.
type Function a = Value (FunPtr a)

-- | Create a new named function.
newNamedFunction :: forall a . (IsFunction a)
                 => Linkage
                 -> String   -- ^ Function name
                 -> CodeGenModule (Function a)
newNamedFunction linkage name = do
    modul <- getModule
    typ <- liftIO $ typeRef (LP.Proxy :: LP.Proxy a)
    liftIO $ liftM Value $ U.addFunction modul linkage name typ

-- | Create a new function.  Use 'newNamedFunction' to create a function with external linkage, since
-- it needs a known name.
newFunction :: forall a . (IsFunction a)
            => Linkage
            -> CodeGenModule (Function a)
newFunction linkage = genMSym "fun" >>= newNamedFunction linkage

-- | Define a function body.  The basic block returned by the function is the function entry point.
defineFunction :: forall f . (FunctionArgs f)
               => Function f        -- ^ Function to define (created by 'newFunction').
               -> FunctionCodeGen f -- ^ Function body.
               -> CodeGenModule ()
defineFunction fn body = do
    bld <- liftIO $ U.createBuilder
    let body' = do
            newBasicBlock >>= defineBasicBlock
            paramFunc (unValue fn) (proxyFromFunction fn) body 0
    runCodeGenFunction bld (unValue fn) body'

proxyFromFunction :: Function f -> LP.Proxy f
proxyFromFunction _ = LP.Proxy

-- | Create a new function with the given body.
createFunction :: (FunctionArgs f)
               => Linkage
               -> FunctionCodeGen f  -- ^ Function body.
               -> CodeGenModule (Function f)
createFunction linkage body = do
    f <- newFunction linkage
    defineFunction f body
    return f

-- | Create a new function with the given body.
createNamedFunction :: (FunctionArgs f)
               => Linkage
               -> String
               -> FunctionCodeGen f  -- ^ Function body.
               -> CodeGenModule (Function f)
createNamedFunction linkage name body = do
    f <- newNamedFunction linkage name
    defineFunction f body
    return f

-- | Set the calling convention of a function. By default it is the
-- C calling convention.
setFuncCallConv :: Function a
                -> FFI.CallingConvention
                -> CodeGenModule ()
setFuncCallConv (Value f) cc = do
  liftIO $ FFI.setFunctionCallConv f (FFI.fromCallingConvention cc)

data Attribute = Attribute Attr.Name Word64

-- | Add attributes to a value.  Beware, what attributes are allowed depends on
-- what kind of value it is.
addAttributes ::
    Value a -> FFI.AttributeIndex -> [Attribute] -> CodeGenFunction r ()
addAttributes (Value f) i as =
    liftIO $ do
        context <- FFI.getGlobalContext
        Fold.forM_ as $ \(Attribute (Attr.Name name) val) -> do
            attrKind <-
                withCStringLen name $
                    uncurry FFI.getEnumAttributeKindForName .
                    mapSnd fromIntegral
            FFI.addCallSiteAttribute f i =<<
                FFI.createEnumAttribute context attrKind val

{- |
Convert a function @f@ of type @t1->t2->...-> IO r@ to
@Value t1 -> Value t2 -> ... CodeGenFunction r ()@.
-}
class IsFunction f => FunctionArgs f where
    type FunctionCodeGen f
    type FunctionResult  f
    paramFunc ::
        FFI.ValueRef -> LP.Proxy f -> FunctionCodeGen f ->
        Int -> CodeGenFunction (FunctionResult f) ()

instance (FunctionArgs b, IsFirstClass a) => FunctionArgs (a -> b) where
    type FunctionCodeGen (a -> b) = Value a -> FunctionCodeGen b
    type FunctionResult  (a -> b) = FunctionResult b
    paramFunc f proxy g n =
        paramFunc f (proxy<*>LP.Proxy) (g $ Value $ U.getParam f n) (n+1)

instance IsFirstClass a => FunctionArgs (IO a) where
    type FunctionCodeGen (IO a) = CodeGenFunction a ()
    type FunctionResult (IO a) = a
    paramFunc _ LP.Proxy code = const code


type family UnaryParameter f i
type instance UnaryParameter (a -> b) Un.Zero = a
type instance UnaryParameter (a -> b) (Un.Succ i) = UnaryParameter b i

type FunctionParameter f i = UnaryParameter f (Dec.ToUnary i)

{- |
Preferably you use the parameter values provided by
'createFunction' or 'defineFunction',
but sometimes you need to access a parameter
after 'newFunction' and before 'defineFunction'.
In this case you can obtain a function parameter using this accessor.
-}
functionParameter ::
    (Dec.Natural i) => Function f -> Proxy i -> Value (FunctionParameter f i)
functionParameter (Value f) n =
    Value $ U.getParam f $ Dec.integralFromProxy n


type family UnValue a
type instance UnValue (Value a) = a

type family CodeValue code
type instance CodeValue (CodeGenFunction r a) = a
type instance CodeValue (a -> b) = CodeValue b

type family CodeResult code
type instance CodeResult (CodeGenFunction r a) = r
type instance CodeResult (a -> b) = CodeResult b


--------------------------------------

-- |A basic block is a sequence of non-branching instructions, terminated by a control flow instruction.
newtype BasicBlock = BasicBlock FFI.BasicBlockRef
    deriving (Show, Typeable)

createBasicBlock :: CodeGenFunction r BasicBlock
createBasicBlock = do
    b <- newBasicBlock
    defineBasicBlock b
    return b

newBasicBlock :: CodeGenFunction r BasicBlock
newBasicBlock = genFSym >>= newNamedBasicBlock

newNamedBasicBlock :: String -> CodeGenFunction r BasicBlock
newNamedBasicBlock name = do
    fn <- getFunction
    liftIO $ liftM BasicBlock $ U.appendBasicBlock fn name

defineBasicBlock :: BasicBlock -> CodeGenFunction r ()
defineBasicBlock (BasicBlock l) = do
    bld <- getBuilder
    liftIO $ U.positionAtEnd bld l

getCurrentBasicBlock :: CodeGenFunction r BasicBlock
getCurrentBasicBlock = do
    bld <- getBuilder
    liftIO $ liftM BasicBlock $ U.getInsertBlock bld

toLabel :: BasicBlock -> Value Label
toLabel (BasicBlock ptr) =
    Value (unsafePerformIO $ FFI.basicBlockAsValue ptr)

fromLabel :: Value Label -> BasicBlock
fromLabel (Value ptr) =
    BasicBlock (unsafePerformIO $ FFI.valueAsBasicBlock ptr)

--------------------------------------

--- XXX: the functions in this section (and addGlobalMapping) don't actually use any
-- Function state so should really be in the CodeGenModule monad

{- |
Create a reference to an external function while code generating for a function.
Functions are not redefined, that is,
all functions with the same name must have the same type.
If LLVM cannot resolve the function name, then you may try 'staticFunction'.
-}
externFunction :: forall a r.
    (IsFunction a) => String -> CodeGenFunction r (Function a)
externFunction name =
    externCore name $
        fmap (unValue :: Function a -> FFI.ValueRef) .
        newNamedFunction ExternalLinkage

-- | As 'externFunction', but for 'Global's rather than 'Function's
externGlobal :: forall a r . (IsType a) => Bool -> String -> CodeGenFunction r (Global a)
externGlobal isConst name =
    externCore name $
        fmap (unValue :: Global a -> FFI.ValueRef) .
        newNamedGlobal isConst ExternalLinkage

externCore ::
    String -> (String -> CodeGenModule FFI.ValueRef) ->
    CodeGenFunction r (Value ptr)
externCore name act = do
    mf <- lookupExtern name
    case mf of
        Just f -> return $ Value f
        Nothing -> do
            f <- liftCodeGenModule $ act name
            addExtern name f
            return $ Value f

{- |
Make an external C function with a fixed address callable from LLVM code.
This callback function can also be a Haskell function,
that was imported like

> foreign import ccall "&nextElement"
>    nextElementFunPtr :: FunPtr (StablePtr (IORef [Word32]) -> IO Word32)

See @examples\/List.hs@.

When you only use 'externFunction', then LLVM cannot resolve the name.
(However, I do not know why.)
Thus 'staticFunction' manages a list of static functions.
This list is automatically installed by 'ExecutionEngine.simpleFunction'
and can be manually obtained by 'getGlobalMappings'
and installed by 'ExecutionEngine.addGlobalMappings'.
\"Installing\" means calling LLVM's @addGlobalMapping@ according to
<http://old.nabble.com/jit-with-external-functions-td7769793.html>.
-}
staticFunction :: forall f r.
    (IsFunction f) => FunPtr f -> CodeGenFunction r (Function f)
staticFunction = staticNamedFunction ""

{- |
Due to <https://llvm.org/bugs/show_bug.cgi?id=20656>
this will fail with MCJIT of LLVM-3.6.
-}
staticNamedFunction :: forall f r.
    (IsFunction f) => String -> FunPtr f -> CodeGenFunction r (Function f)
staticNamedFunction name func = liftCodeGenModule $ do
    val <- newNamedFunction ExternalLinkage name
    addFunctionMapping (unValue (val :: Function f)) func
    return val

-- | As 'staticFunction', but for 'Global's rather than 'Function's
staticGlobal :: forall a r.
    (IsType a) => Bool -> Data.Ptr a -> CodeGenFunction r (Global a)
staticGlobal isConst gbl = liftCodeGenModule $ do
    val <- newNamedGlobal isConst ExternalLinkage ""
    addGlobalMapping (unValue (val :: Global a)) gbl
    return val

--------------------------------------

withCurrentBuilder :: (FFI.BuilderRef -> IO a) -> CodeGenFunction r a
withCurrentBuilder body = do
    bld <- getBuilder
    liftIO $ U.withBuilder bld body

--------------------------------------

-- Mark all block terminating instructions.  Not used yet.
--data Terminate = Terminate

--------------------------------------

type Global a = Value (Data.Ptr a)

-- | Create a new named global variable.
newNamedGlobal :: forall a . (IsType a)
               => Bool         -- ^Constant?
               -> Linkage      -- ^Visibility
               -> String       -- ^Name
               -> TGlobal a
newNamedGlobal isConst linkage name = do
    modul <- getModule
    typ <- liftIO $ typeRef (LP.Proxy :: LP.Proxy a)
    liftIO $ liftM Value $ do
        g <- U.addGlobal modul linkage name typ
        when isConst $ FFI.setGlobalConstant g FFI.true
        return g

-- | Create a new global variable.
newGlobal :: forall a . (IsType a) => Bool -> Linkage -> TGlobal a
newGlobal isConst linkage = genMSym "glb" >>= newNamedGlobal isConst linkage

-- | Give a global variable a (constant) value.
defineGlobal :: Global a -> ConstValue a -> CodeGenModule ()
defineGlobal (Value g) (ConstValue v) =
    liftIO $ FFI.setInitializer g v

-- | Create and define a global variable.
createGlobal :: (IsType a) => Bool -> Linkage -> ConstValue a -> TGlobal a
createGlobal isConst linkage con = do
    g <- newGlobal isConst linkage
    defineGlobal g con
    return g

-- | Create and define a named global variable.
createNamedGlobal :: (IsType a) => Bool -> Linkage -> String -> ConstValue a -> TGlobal a
createNamedGlobal isConst linkage name con = do
    g <- newNamedGlobal isConst linkage name
    defineGlobal g con
    return g

type TFunction a = CodeGenModule (Function a)
type TGlobal a = CodeGenModule (Global a)

-- Special string creators
{-# DEPRECATED createString "use withString instead" #-}
createString :: String -> TGlobal (Array n Word8)
createString s = string (length s) (U.constString s)

{-# DEPRECATED createStringNul "use withStringNul instead" #-}
createStringNul :: String -> TGlobal (Array n Word8)
createStringNul s = string (length s + 1) (U.constStringNul s)

withString ::
    String ->
    (forall n. (Dec.Natural n) => Global (Array n Word8) -> CodeGenModule a) ->
    CodeGenModule a
withString s act =
    let n = length s
    in  fromMaybe (error "withString: length must always be non-negative") $
        Dec.reifyNatural (fromIntegral n) (\tn -> do
            arr <- string n (U.constString s)
            act (fixArraySize tn arr))

withStringNul ::
    String ->
    (forall n. (Dec.Natural n) => Global (Array n Word8) -> CodeGenModule a) ->
    CodeGenModule a
withStringNul s act =
    let n = length s + 1
    in  fromMaybe (error "withStringNul: length must always be non-negative") $
        Dec.reifyNatural (fromIntegral n) (\tn -> do
            arr <- string n (U.constStringNul s)
            act (fixArraySize tn arr))

fixArraySize :: Proxy n -> Global (Array n a) -> Global (Array n a)
fixArraySize _ = id

string :: Int -> FFI.ValueRef -> TGlobal (Array n Word8)
string n s = do
    modul <- getModule
    name <- genMSym "str"
    elemTyp <- liftIO $ typeRef (LP.Proxy :: LP.Proxy Word8)
    typ <- liftIO $ FFI.arrayType elemTyp (fromIntegral n)
    liftIO $ liftM Value $ do g <- U.addGlobal modul InternalLinkage name typ
                              FFI.setGlobalConstant g FFI.true
                              FFI.setInitializer g s
                              return g

--------------------------------------

-- |Make a constant vector.
constVector ::
    forall a n u.
    (Dec.Positive n, Dec.ToUnary n ~ u,
     UnaryVector.Length (FixedList u) ~ u) =>
    UnaryVector.FixedList u (ConstValue a) ->
    ConstValue (Vector n a)
constVector =
    constVectorGen id

constVectorGen ::
    forall a b n u.
    (Dec.Positive n, Dec.ToUnary n ~ u) =>
    (b -> ConstValue a) ->
    UnaryVector.FixedList u b ->
    ConstValue (Vector n a)
constVectorGen f xs =
    unsafeConstValue $
    U.constVector
        (case DecProof.unaryNat :: DecProof.UnaryNat n of
             DecProof.UnaryNat ->
                 map (unConstValue . f) $
                 Fold.toList
                     (UnaryVector.fromFixedList xs :: UnaryVector.T u b))

{- |
Make a constant vector.
Replicates or truncates the list to get length @n@.
-}
constCyclicVector ::
    forall a n.
    (Dec.Positive n) =>
    NonEmpty.T [] (ConstValue a) ->
    ConstValue (Vector n a)
constCyclicVector xs =
    unsafeConstValue $
    U.constVector
        (take (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n)) $
         map unConstValue $ NonEmpty.flatten $ NonEmpty.cycle xs)


constArray ::
    forall a n . (IsSized a, Dec.Natural n) =>
    [ConstValue a] -> ConstValue (Array n a)
constArray xs = unsafeConstValue $ do
    let m = length xs
        n = Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n)
    when (m /= n) $
        error $
            printf "LLVM.constArray: number of array elements (%d) mismatches typed array length (%d)"
                m n
    typ <- typeRef (LP.Proxy :: LP.Proxy a)
    U.constArray typ $ map unConstValue xs

{- |
Make a constant array.
Replicates or truncates the list to get length @n@.
-}
constCyclicArray ::
    forall a n.
    (IsSized a, Dec.Natural n) =>
    NonEmpty.T [] (ConstValue a) ->
    ConstValue (Vector n a)
constCyclicArray xs = unsafeConstValue $ do
    typ <- typeRef (LP.Proxy :: LP.Proxy a)
    U.constArray typ
        (take (Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton n)) $
         map unConstValue $ NonEmpty.flatten $ NonEmpty.cycle xs)

-- |Make a constant struct.
constStruct ::
    (IsConstStruct c) => c -> ConstValue (Struct (ConstStructOf c))
constStruct struct =
    unsafeConstValue $ U.constStruct (constValueFieldsOf struct) False

-- |Make a constant packed struct.
constPackedStruct ::
    (IsConstStruct c) => c -> ConstValue (PackedStruct (ConstStructOf c))
constPackedStruct struct =
    unsafeConstValue $ U.constStruct (constValueFieldsOf struct) True

class IsConstStruct c where
    type ConstStructOf c
    constValueFieldsOf :: c -> [FFI.ValueRef]

instance (IsConst a, IsConstStruct cs) => IsConstStruct (ConstValue a, cs) where
    type ConstStructOf (ConstValue a, cs) = (a, ConstStructOf cs)
    constValueFieldsOf (a, as) = unConstValue a : constValueFieldsOf as
instance IsConstStruct () where
    type ConstStructOf () = ()
    constValueFieldsOf _ = []
