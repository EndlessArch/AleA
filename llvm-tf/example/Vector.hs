{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Convert

import LLVM.ExecutionEngine
          (runEngineAccessWithModule, generateFunction, getExecutionFunction)
import LLVM.Util.Optimize (optimizeModule, )
import LLVM.Util.Loop (forLoop, )
import LLVM.Core

import qualified Type.Data.Num.Decimal.Number as Dec
import Type.Data.Num.Decimal.Literal (D16, )

import Control.Monad.IO.Class (liftIO, )
import Control.Monad (liftM2, when, )
import Data.Word (Word32, )

-- Type of vector elements.
type T = Float

-- Number of vector elements.
type N = D16

retAccName, fName :: String
retAccName = "retacc"
fName = "vectest"

cgvec :: CodeGenModule (Function (T -> IO T))
cgvec = do
    -- A global variable that vectest messes with.
    acc <- createNamedGlobal False ExternalLinkage "acc" (constOf (0 :: T))

    -- Return the global variable.
    retAcc <- createNamedFunction ExternalLinkage retAccName $ do
        vacc <- load acc
        ret vacc
    let _ = retAcc :: Function (IO T)  -- Force the type of retAcc.

    -- A function that tests vector operations.
    f <- createNamedFunction ExternalLinkage fName $ \ x -> do

        let v = value (zero :: ConstValue (Vector N T))
            n = Dec.integralFromSingleton (Dec.singleton :: Dec.Singleton N) :: Word32

        -- Fill the vector with x, x+1, x+2, ...
        (_, v1) <- forLoop (valueOf 0) (valueOf n) (x, v) $ \ i (x1, v1) -> do
            x1' <- add x1 (valueOf (1::T))
            v1' <- insertelement v1 x1 i
            return (x1', v1')

        -- Elementwise cubing of the vector.
        vcb <- mul v1 =<< mul v1 v1

        -- Sum the elements of the vector.
        s <- forLoop (valueOf 0) (valueOf n) (valueOf 0) $ \ i s -> do
            y <- extractelement vcb i
            add s (y :: Value T)

        -- Update the global variable.
        vacc <- load acc
        vacc' <- add vacc s
        store vacc' acc

        ret (s :: Value T)

    when False $ liftIO $ dumpValue f
    return f

createFuncModule :: IO (Module, Function (T -> IO T))
createFuncModule =
    createModule $ setTarget hostTriple >> liftM2 (,) getModule cgvec

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    -- First run standard code.
    do  (m, iovec) <- createFuncModule
        fvec <- runEngineAccessWithModule m $ getExecutionFunction convert iovec
        fvec 10 >>= print

    do  (m, iovec) <- createFuncModule
        vec <- runEngineAccessWithModule m $ generateFunction iovec
        vec 10 >>= print

    -- And then optimize and run.
    do  m <- fmap fst createFuncModule
        _ <- optimizeModule 1 m

        funcs <- getModuleValues m
        print $ map fst funcs

        let iovec' :: Function (T -> IO T)
            Just iovec' = castModuleValue =<< lookup fName funcs
            ioretacc' :: Function (IO T)
            Just ioretacc' = castModuleValue =<< lookup retAccName funcs

        (vec', retacc') <-
            runEngineAccessWithModule m $
            liftM2 (,) (generateFunction iovec') (generateFunction ioretacc')

        when False $ dumpValue iovec'

        vec' 10 >>= print
        vec' 0 >>= print
        retacc' >>= print
