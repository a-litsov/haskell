{-# LANGUAGE TemplateHaskell #-}

module SyntaxInFunctions where

import Test.QuickCheck
import Test.QuickCheck.All

lucky :: (Integral a) => a -> String
lucky 7 = "lucky number seven"
lucky x = "sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

prop_factorial5 = 120 == factorial 5


charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectorsPatterns :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectorsPatterns (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

prop_addVectorPatterns a b = addVectors a b == addVectorsPatterns a b


-- analogues for fst and snd but for triples
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

prop_first (x, y, z) = x == first (x, y, z)
prop_second (x, y, z) = y == second (x, y, z)
prop_third (x, y, z) = z == third (x, y, z)


head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

prop_head' (NonEmpty (x:xs)) = x == head' (x:xs)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
-- Better to write pattern like this:
--tell [x] = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
-- Better to write pattern like this:
--tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is too long. First two elements are: " ++ show x ++ " and " ++ show y

tellFirstLetterIsA :: String -> Bool
tellFirstLetterIsA ('A':_) = True
tellFirstLetterIsA _ = False

prop_tellFirstLetterIsA (NonEmpty list) = (head list == 'A') == tellFirstLetterIsA list


length' :: (Num b) => [a] -> b
length' [] = 0 -- edge condition
length' (_:xs) = length' xs + 1

prop_length' list = length list == length' list

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

prop_sum' list = sum list == sum' list

capital :: String -> String
capital [] = error "Oops, empty list!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: Ord a => a -> a -> a
max' x y
  | x < y = y
  | otherwise = x

prop_max' x y = max x y == max' x y

-- don't do this - non-readable
max'' :: (Ord a) => a -> a -> a
max'' a b | a > b = a | otherwise = b

myCompare :: Ord a => a -> a -> Ordering
x `myCompare` y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

prop_myCompare x y = compare x y == myCompare x y


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [s] ++ "."
  where (f:_) = firstname
        (s:_) = lastname

prop_testInitialsLenin = "V. I." == initials "Vladimir" "Ilyich"

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

return []
runTests :: IO Bool
runTests = $quickCheckAll
