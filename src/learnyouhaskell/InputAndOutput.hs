module InputAndOutput where

import Control.Monad
import System.Random
import Data.Char

--main = do
--  putStrLn "Hello, what's your name?"
--  name <- getLine
--  putStrLn("Hey " ++ name ++ ", you rock!")

--main = do
--    line <- getLine
--    if null line
--        then return ()
--        else do
--            putStrLn $ reverseWords line
--            main
--
--reverseWords :: String -> String
--reverseWords = unwords . map reverse . words

--main = forever $ do
--    putStr "Give me some input: "
--    l <- getLine
--    putStrLn $ map toUpper l

-- randomness

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g =
  let (value, newGen) = random g
  in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num i, Eq i) => g -> i -> ([a], g)
finiteRandoms g 0 = ([], g)
finiteRandoms g n =
  (newValue : restOfList, finalGen)
  where (newValue, newGen) = random g
        (restOfList, finalGen) = finiteRandoms newGen (n-1)

--main = do
--  gen <- getStdGen
--  let randomChars = randomRs ('a', 'z') gen
--      (first20, rest) = splitAt 20 randomChars
--      (second20, _) = splitAt 20 rest
--  putStr first20
--  putStrLn second20

main = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen)
  gen <- newStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
