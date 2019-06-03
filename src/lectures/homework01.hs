-- multLastThreeDigits :: Integer -> Integer
-- multLastThreeDigits x
-- | x < 0 = multLastThreeDigits (abs x)
-- | x > 1000 = multLastThreeDigits (x `rem` 1000)
-- | x > 0 && x < 10 = x
-- | x == 0 = 0
-- | otherwise = (x `rem` 10) * multLastThreeDigits (x `quot` 10)

multLastThreeDigits :: Integer -> Integer
multLastThreeDigits x
 | d < 3 = (x `rem` 10) * multLastThreeDigitsstThreeDigits (x `quot` 10); d (+1)
 where
  d = 0