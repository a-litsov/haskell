{-# LANGUAGE TemplateHaskell #-}

module TypesAndTypeclasses where

import Test.QuickCheck
import Test.QuickCheck.All

addThree :: Int -> Int -> Int -> Int
addThree x y z = sum [x, y, z]

prop_addThree x y z = x + y + z == addThree x y z


factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

return []
runTests :: IO Bool
runTests = $quickCheckAll