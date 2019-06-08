{-# LANGUAGE TemplateHaskell #-}

module Test1Solutions9 where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char

-- 1
-- Point-free style 8)
sumDigits :: String -> Int
sumDigits = sum . map digitToInt . filter isDigit

prop_sumDigits1 = 20 == sumDigits "IN 47405"
prop_sumDigits2 = 0 == sumDigits "No digits here!"

-- 2
abbrev :: [String] -> [String]
abbrev = map (buildAbbrev . words)

-- Builds an abbrevition for a full name given as string list
buildAbbrev :: [String] -> String
buildAbbrev wordsInName
  | [familyName] <- wordsInName = familyName -- Note this syntax - mix of guards and pattern-matching!
  | otherwise = buildInitialsString (init wordsInName) ++ " " ++ last wordsInName

-- Takes string list - 'first' names - and builds initials string
buildInitialsString :: [String] -> String
--buildInitialsString fullName = concatMap ((: ".") . head) fullName
buildInitialsString = foldr (\word acc -> head word : '.' : acc) []

abbrevTestData =  ["Синицин", "Сергей Есенин", "Игорь Федорович Поддубный",
                  "Иоганн Хризостом Вольфганг Амадей Моцарт"]
abbrevTestExpected = ["Синицин", "С. Есенин", "И.Ф. Поддубный", "И.Х.В.А. Моцарт"]
prop_abbrev = abbrevTestExpected == abbrev abbrevTestData


return []
runTests :: IO Bool
runTests = $quickCheckAll