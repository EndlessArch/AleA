module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Core

import LLVM.Util.Loop (forLoop)
import LLVM.Util.File (writeCodeGenModule)
import LLVM.Util.Foreign (withArrayLen)

import qualified Type.Data.Num.Decimal.Number as Dec
import qualified Type.Data.Num.Decimal.Literal as TypeNum
import Type.Base.Proxy (Proxy(Proxy))

import qualified Data.Traversable as Trav
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe)
import Data.Tuple.HT (swap)
import Data.Word (Word32)

import Control.Applicative (pure)


mDotProd ::
    (Dec.Positive n,
     IsPrimitive a, IsArithmetic a, IsFirstClass a, IsConst a, Num a) =>
    CodeGenModule
        (Function (Word32 -> Ptr (Vector n a) -> Ptr (Vector n a) -> IO a))
mDotProd =
  createFunction ExternalLinkage $ \ size aPtr bPtr -> do
    s <- forLoop (valueOf 0) size (value zero) $ \ i s -> do

        ap <- getElementPtr aPtr (i, ()) -- index into aPtr
        bp <- getElementPtr bPtr (i, ()) -- index into bPtr
        a <- load ap                     -- load element from a vector
        b <- load bp                     -- load element from b vector
        ab <- mul a b                    -- multiply them
        add s ab                         -- accumulate sum

    r <-
        forLoop
            (valueOf (0::Word32))
            (valueOf (Dec.integralFromProxy (vectorSize aPtr)))
            (valueOf 0)
            (\ i r -> add r =<< extractelement s i)
    ret r

vectorSize :: Value (Ptr (Vector n a)) -> Proxy n
vectorSize _ = Proxy


type R = Float
type T = Vector TypeNum.D4 R

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget
    let mDotProd' = mDotProd
    writeCodeGenModule "DotProd.bc" mDotProd'

    ioDotProd <- EE.simpleFunction mDotProd'
    let dotProd :: [T] -> [T] -> R
        dotProd a b =
         EE.unsafeRemoveIO $
         withArrayLen a $ \ aLen aPtr ->
         withArrayLen b $ \ bLen bPtr ->
         ioDotProd (fromIntegral (aLen `min` bLen)) aPtr bPtr


    let a = [1 .. 8]
        b = [4 .. 11]
    print $ dotProd (vectorize 0 a) (vectorize 0 b)
    print $ sum $ zipWith (*) a b

vectorize :: (Positive n) => a -> [a] -> [Vector n a]
vectorize deflt =
    List.unfoldr (\xs -> toMaybe (not $ null xs) (vectorizeHead deflt xs))

vectorizeHead :: (Positive n) => a -> [a] -> (Vector n a, [a])
vectorizeHead deflt ys =
    swap $
    Trav.mapAccumL
        (\xt () -> swap $ fromMaybe (deflt,[]) $ ListHT.viewL xt)
        ys (pure ())
