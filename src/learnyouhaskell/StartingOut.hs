{-# LANGUAGE TemplateHaskell #-}

module StartingOut where

import Test.QuickCheck
import Test.QuickCheck.All

doubleMe :: Num a => a -> a
doubleMe x = x + x

prop_doubleMe x = doubleMe x == 2 * x
prop_doubleMe_5 = doubleMe 5 == 10


doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

prop_DoubleUs x y = doubleUs x y == x*2 + y*2


doubleUsRemade x y = doubleMe x + doubleMe y

prop_doubleUsRemade x y = doubleUsRemade x y == doubleUs x y


doubleSmallNumber x = if x > 100 then
                        x
                      else
                        doubleMe x

prop_doubleSmallNumber_15 = doubleSmallNumber 15 == 30
prop_doubleSmallNumber_139 = doubleSmallNumber 139 == 139


doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1

prop_doubleSmallNumber'_15 = doubleSmallNumber' 15 == 31
prop_doubleSmallNumber'_139 = doubleSmallNumber' 139 == 140

conanO'Brien = "It's me!"
--ConanO'Brien = "functions cannot begin from uppercase letter"

boomBang :: [Int] -> [String]
boomBang xs = [if x > 10 then "BANG!" else "BOOM!" | x <- xs, odd x]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

prop_length' xs = length xs == length' xs

keepUppercaseOnly :: String -> String
keepUppercaseOnly str = [char | char <- str, char `elem` ['A'..'Z']]

prop_keepUppercaseOnly1 = "HO" == keepUppercaseOnly "HellO"
prop_keepUppercaseOnly2 = "TASET" == keepUppercaseOnly "TrAnSiEnT"

keepEvenInner :: [[Int]] -> [[Int]]
keepEvenInner xxs = [[x | x <- xs, even x] | xs <- xxs]

prop_keepEvenInner = [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]   ==
  keepEvenInner [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

triangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10]]
getSomeRightTriangle :: [(Int, Int, Int)]
getSomeRightTriangle = [(a, b, c) | (a, b, c) <- triangles, a + b + c == 24, a*a + b*b == c*c]

prop_getSomeRightTriangle = (6, 8, 10) `elem` getSomeRightTriangle

return []
runTests :: IO Bool
runTests = $quickCheckAll