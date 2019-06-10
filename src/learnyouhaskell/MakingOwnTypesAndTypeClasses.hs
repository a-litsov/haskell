{-# LANGUAGE TemplateHaskell #-}

module MakingOwnTypesAndTypeClasses where

import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Map as Map

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = abs $ (x2-x1) * (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) shiftX shiftY = Circle (Point (x+shiftX) (y+shiftY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) shiftX shiftY =
  Rectangle (Point (x1+shiftX) (y1+shiftY)) (Point (x2+shiftX) (y2+shiftY))

-- base methods for create (0, 0)-based shapes, to get arbitrary shape we need to apply
-- nudge to built with the help of this method shapes
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

-- record syntax
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
myCar = Car {company="Ford", model="Mustang", year=1967}

tellCar :: Car -> String
tellCar Car {company = c, model = m, year = y} = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
--tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- Sample with Car that may have arbitrary type of 'company' field

--data Car a = Car {company :: a, model :: String, year :: Int} deriving (Show)
--myCar = Car {company = "Ford", model = "Mustang", year = 1987}
--myIntCar = Car {company = 228, model = "Mustang", year = 1987}
--
--tellCar :: (Show a) => Car a -> String
--tellCar Car {company = c, model = m, year = y} = "This " ++ show c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- lockers sample
data LockerState = Taken | Free deriving (Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
    Just (state, code) ->
      if state == Taken then
        Left $ "Locker " ++ show lockerNumber ++ " is already taken"
      else
        Right code

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- recursive structures
--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List a = Empty | a :-: List a deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ xs = xs
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = Leaf | Node { value :: a, left :: Tree a, right :: Tree a} deriving (Eq, Show, Read)

singleton :: (Ord a) => a -> Tree a
singleton x = Node x Leaf Leaf

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Leaf = singleton x
treeInsert x current@(Node val left right)
  | x == val = current
  | x < val  = Node val (treeInsert x left) right
  | x > val  = Node val left $ treeInsert x right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Leaf = False
treeElem x (Node val left right)
  | x == val = True
  | otherwise = treeElem x nextTree
  where nextTree = if x < val then left else right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert Leaf nums

-- typeclasses
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno _ = True

instance YesNo (Tree a) where
  yesno Leaf = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- Functor

--class Functor f where
--  fmap :: (a -> b) -> f a -> f b

--instance Functor Maybe where
--  fmap f (Just x) = Just (f x)
--  fmap f Nothing = Nothing

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)


-- self-made type class
class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankfield :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

return []
runTests :: IO Bool
runTests = $quickCheckAll