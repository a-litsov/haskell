{-# LANGUAGE TemplateHaskell #-}

module HigherOrderFunctions where

import Test.QuickCheck
import Test.QuickCheck.All

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

return []
runTests :: IO Bool
runTests = $quickCheckAll
