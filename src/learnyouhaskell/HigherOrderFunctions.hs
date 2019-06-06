{-# LANGUAGE TemplateHaskell #-}

module HigherOrderFunctions where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.List

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

divideTen :: (Floating a) => a -> a
divideTen = (10/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'.. 'Z'])

prop_isUpperAlphanum c = (c `elem` ['A'.. 'Z']) == isUpperAlphanum c

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

prop_flip'Plus = (+) 2 3 == flip' (+) 2 3
prop_flip'Concat = (++) "hello " "there " == flip' (++) "there " "hello "


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)

prop_quicksort xs = sort xs == quicksort xs


largestDivisible :: Integral a => a
largestDivisible = head (filter isDivisible [100000, 99999 ..])
  where isDivisible x = (x `mod` 3829) == 0

sumOfSquares :: Integral a => a
sumOfSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

sumOfSquares' :: Integral a => a
sumOfSquares' = sum (takeWhile (< 10000) [x ^ 2 | x <- [1..], odd (x^2)])

prop_sumSquares = sumOfSquares == sumOfSquares'

-- Collatz
chain :: Integral a => a -> [a]
chain 1 = [1]
chain x
  | odd x = x : chain (x * 3 + 1)
  | otherwise = x : chain (x `div` 2)

prop_chain13 = [13, 40, 20, 10, 5, 16, 8, 4, 2, 1] == chain 13

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (> 15) (map getChainLength [1..100]))
  where getChainLength n = length (chain n)

prop_numLongChains' = numLongChains == numLongChains'

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

prop_sum' xs = sum xs == sum' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y xs = foldl (\acc x -> acc || y == x) False xs

prop_elem' y xs = elem y xs == elem' y xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if acc > x then acc else x)

prop_maximum' (NonEmpty xs) = maximum xs == maximum' xs

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

prop_reverse' xs = reverse' (reverse' xs) == xs

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

prop_product' (NonEmpty xs) = product xs == product' xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 const

prop_head' (NonEmpty xs) = head xs == head' xs

last' :: [a] -> a
last' = foldl1 (flip const)

prop_last' (NonEmpty xs) = last xs == last' xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

prop_sqrtSums = 131 == sqrtSums

return []
runTests :: IO Bool
runTests = $quickCheckAll
