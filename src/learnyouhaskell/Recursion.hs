{-# LANGUAGE TemplateHaskell #-}

module Recursion where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list cannot have maximum!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

prop_maximum' (NonEmpty xs) = maximum xs == (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

prop_replicate' n x = replicate n x == replicate' n x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- mine
--take' n xs
--  | n <= 0 = []
--  | null xs = []
--  | otherwise = head' : take' (n - 1) tail'
--  where size = length xs
--        (head', tail') = (head xs, tail xs)

prop_take' n xs = take n xs == take' n xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

prop_reverse' xs = reverse xs == reverse' xs

repeat' :: a -> [a]
repeat' x = x : repeat x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

prop_zip' xs ys = zip xs ys == zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

prop_elem' x xs = elem x xs == elem' x xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

prop_quicksort xs = sort xs == quicksort xs

return []
runTests :: IO Bool
runTests = $quickCheckAll
