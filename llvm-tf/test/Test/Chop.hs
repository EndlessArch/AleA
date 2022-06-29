module Test.Chop where

import qualified LLVM.ExecutionEngine.Marshal as Marshal

import Data.Bits (shiftL)
import Data.Word (Word8)

import qualified Test.QuickCheck as QC


divUp :: Integral a => a -> a -> a
divUp a b = - div (-a) b

expandBits :: [Word8] -> Bool
expandBits xs  =  xs == Marshal.gatherBits (Marshal.expandBits xs)

gatherBits :: [Bool] -> Bool
gatherBits xs =
    Marshal.gatherBits xs
    ==
    (take (divUp (length xs) 8) $ map fromIntegral $
     Marshal.chop 1 8 $ map (toInteger . fromEnum) xs)

forAllBitWidth :: (Int -> QC.Property) -> QC.Property
forAllBitWidth = QC.forAll (QC.choose (1,100))

chopBig :: QC.NonNegative Int -> QC.Property
chopBig (QC.NonNegative k) =
    forAllBitWidth $ \m ->
    forAllBitWidth $ \n ->
    QC.forAll (QC.listOf $ QC.choose (0, shiftL 1 m - 1)) $ \xs ->
        take k (Marshal.chop m n xs)
        ==
        take k (Marshal.split n $ Marshal.merge m xs)

chop :: QC.NonNegative Int -> QC.Property
chop (QC.NonNegative k) =
    forAllBitWidth $ \m ->
    forAllBitWidth $ \n ->
    QC.forAll (QC.listOf $ QC.choose (0, shiftL 1 m - 1)) $ \xs ->
        take k (Marshal.chop n m $ Marshal.chop m n xs)
        ==
        take k (xs ++ repeat 0)

chopSigned :: QC.NonNegative Int -> QC.Property
chopSigned (QC.NonNegative k) =
    forAllBitWidth $ \m ->
    forAllBitWidth $ \n ->
    QC.forAll (QC.listOf $ QC.choose (- shiftL 1 m, shiftL 1 m - 1)) $ \xs ->
        take k (map (Marshal.adjustSign (m+1)) $ Marshal.chop n (m+1) $
                Marshal.chop (m+1) n $ map (Marshal.cut (m+1)) xs)
        ==
        take k (xs ++ repeat 0)


tests :: [(String, QC.Property)]
tests =
    ("expandBits", QC.property expandBits) :
    ("gatherBits", QC.property gatherBits) :
    ("chopBig",  QC.property chopBig) :
    ("chop", QC.property chop) :
    ("chopSigned", QC.property chopSigned) :
    []
