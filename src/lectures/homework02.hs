sign :: (Num a, Ord a) => a -> a
sign x
 | x > 0 = 1
 | x < 0 = (-1)
 | otherwise = 0
 
solveSquareEq :: (Ord a, Floating a) => a -> a -> a -> [a]
solveSquareEq a b c
 | d < 0 = []
 | otherwise = [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a + y]
 where
  d = b^2 - 4*a*c;
  y = 22