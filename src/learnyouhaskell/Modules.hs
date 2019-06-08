{-# LANGUAGE TemplateHaskell #-}

module Modules where

import Test.QuickCheck
import Test.QuickCheck.All

import Data.List
import Data.Function
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

prop_numUniques1 = 5 == numUniques [1, 2, 3, 5, 4, 5]
prop_numUniques2 = 2 == numUniques [2, 3, 2]

thirdPowers :: Integral a => a
thirdPowers = sum $ takeWhile (< 10000) $ map (^3) [1..]

prop_thirdPowers = 53361 == thirdPowers

stockVals = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
stock :: (Double, Integer, Integer, Integer)
stock = head $ dropWhile (\(val, y, m, d) -> val < 1000) stockVals

prop_stock = (1001.4,2008,9,4) == stock

searchSubList :: (Eq a) => [a] -> [a] -> Bool
searchSubList needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

prop_searchSubList = True == searchSubList [2, 3] [3, 4, 2, 3]

groupBySign :: (Num a, Ord a) => [a] -> [[a]]
groupBySign = groupBy (\x y -> (x > 0) == (y > 0))

prop_groupBySign =
  [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
    == groupBySign [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

groupBySign' :: (Num a, Ord a) => [a] -> [[a]]
groupBySign' = groupBy ((==) `on` (>0))

prop_groupBySign' xs = groupBySign xs == groupBySign' xs

isAllAreDigit :: String -> Bool
isAllAreDigit = all isDigit

prop_isAllAreDigitTrue = isAllAreDigit "12341235212340"
prop_isAllAreDigitFalse = not $ isAllAreDigit "1234123h5212340"

-- encodes by shifting character on some value in ascii table
encode :: Int -> String -> String
encode shift = map (chr . (+ shift) . ord)

prop_encode1 =  "Khhhhh|" == encode 3 "Heeeeey"
prop_encode2 =  "Liiiii}" == encode 3 "Heeeeey"
prop_encode3 =  "bcde" == encode 1 "abcd"

decode :: Int -> String -> String
decode shift = map (chr . (shift `subtract`) . ord)
--decode = encode . negate

prop_decode (NonNegative shift) str = str == (decode shift . encode shift $ str)

return []
runTests :: IO Bool
runTests = $quickCheckAll
