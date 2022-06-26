module ListUtil where

import Data.Text
import Data.List.Split
import Data.List

isSublistOf :: (Eq a) => [a] -> [a] -> Bool
isSublistOf _ [] = False
isSublistOf [] _ = True
isSublistOf (x:xs) (y:ys)
  | x == y = isSublistOf xs ys
  | otherwise = isSublistOf xs (y:ys)

sublistAt :: (Eq a) => [a] -> [a] -> Maybe Int
sublistAt _ [] = Nothing
sublistAt [] _ = Just 0
sublistAt (x:xs) (y:ys)
  | x == y = sublistAt xs ys
  | otherwise
  = case sublistAt (x:xs) ys of
    Just z  -> Just $ 1 + z
    Nothing -> Nothing

-- splitPrd :: (a -> Bool) -> [a] -> ([a], [a])
-- splitPrd f [] = ([], [])
-- splitPrd f (x:xs)
--   | f x = ([], xs)
--   | otherwise = Data.Bifunctor.first (x :) $ splitPrd f xs

-- splitWith :: (Eq a) => [a] -> [a] -> ([a], [a])
-- splitWith x y =
--   let at = sublistAt x y
--   in case at of
--     Just at'  ->
--       let (t1, t2) = splitAt at' y
--       in (t1, drop (1 + length x) t2)
--     Nothing   -> ([], y)

-- splitWithAll :: (Eq a) => [a] -> [a] -> [[a]]
-- splitWithAll x y
--   = let (a, b) = splitWith x y
--     in
--       if null a || null b
--         then [a ++ b]
--       else
--         a:splitWithAll x b