module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Util.Optimize (optimizeModule)
import LLVM.Core

import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.Word (Word32)

import Prelude hiding(and, or)


-- Our module will have these two functions.
data Mod = Mod {
    mfib :: Function (Word32 -> IO Word32),
    _mplus :: Function (Word32 -> Word32 -> IO Word32)
    }

main :: IO ()
main = do
    args <- getArgs
    let args' = if null args then ["10"] else args

    -- Initialize jitter
    initializeNativeTarget
    -- Create a module,
    m <- newNamedModule "fib"
    -- and define its contents.
    td <- EE.getTargetData
    fns <-
        defineModule m $ do
            setTarget hostTriple
            setDataLayout (EE.dataLayoutStr td)
            buildMod

    -- Show the code for the two functions, just for fun.
    --dumpValue $ mfib fns
    --dumpValue $ mplus fns
    -- Write the code to a file for later perusal.
    -- Can be disassembled with llvm-dis.
    writeBitcodeToFile "Fibonacci.bc" m

    _ <- optimizeModule 3 m
    writeBitcodeToFile "Fibonacci-opt.bc" m

    -- Generate code for mfib, and then throw away the IO in the type.
    -- The result is an ordinary Haskell function.
    iofib <- EE.runEngineAccessWithModule m $
                 EE.generateFunction $ mfib fns
    let fib = EE.unsafeRemoveIO iofib

    -- Run fib for the arguments.
    forM_ args' $ \num -> do
        putStrLn $ "fib " ++ num ++ " = " ++ show (fib (read num))

buildMod :: CodeGenModule Mod
buildMod = do
    -- Add two numbers in a cumbersome way.
    plus <- createFunction InternalLinkage $ \ x y -> do
        -- Create three additional basic blocks, need to be created before being referred to.
        l1 <- newBasicBlock
        l2 <- newBasicBlock
        l3 <- newBasicBlock

        -- Test if x is even/odd.
        a <- and x (valueOf (1 :: Word32))
        c <- cmp CmpEQ a (valueOf (0 :: Word32))
        condBr c l1 l2

        -- Do x+y if even.
        defineBasicBlock l1
        r1 <- add x y
        br l3

        -- Do y+x if odd.
        defineBasicBlock l2
        r2 <- add y x
        br l3

        defineBasicBlock l3
        -- Join the two execution paths with a phi instruction.
        r <- phi [(r1, l1), (r2, l2)]
        ret r

    -- The usual doubly recursive Fibonacci.
    -- Use new&define so the name fib is defined in the body for recursive calls.
    fib <- newNamedFunction ExternalLinkage "fib"
    defineFunction fib $ \ arg -> do
        -- Create the two basic blocks.
        recurse <- newBasicBlock
        exit <- newBasicBlock

        -- Test if arg > 2
        test <- cmp CmpGT arg (valueOf (2::Word32))
        condBr test recurse exit

        -- Just return 1 if not > 2
        defineBasicBlock exit
        ret (valueOf (1::Word32))

        -- Recurse if > 2, using the cumbersome plus to add the results.
        defineBasicBlock recurse
        x1 <- sub arg (valueOf (1::Word32))
        fibx1 <- call fib x1
        x2 <- sub arg (valueOf (2::Word32))
        fibx2 <- call fib x2
        r <- call plus fibx1 fibx2
        ret r

    -- Return the two functions.
    return $ Mod fib plus
