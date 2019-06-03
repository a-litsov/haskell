{-# LANGUAGE GADTs #-}

import Prelude hiding (Monoid)

-- Функции show, read и error описаны на с. 43-45 в книге Макеева
-- «Основы функционального программирования на языке Haskell».

-- Функции для преобразования значений в строчки и обратно

-- :t show
-- :t read
-- read "54"
-- read "54" + 1
-- read "54" :: Integer

-- Алгебраические типы данных (algebraic datatypes)
-- С. 66–74 в книге Макеева

-- Стандартный тип
-- data Bool = False | True

-- Унарное представление натуральных чисел (как в арифметике Пеано)
data Nat = Zero | Succ Nat deriving (Show)

eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat (Succ x) (Succ y) = eqNat x y
eqNat _ _ = False

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

one :: Nat
one = Succ Zero

two :: Nat
two = Succ (Succ Zero)

three :: Nat
three = Succ (Succ (Succ Zero))

-- Каждый тип имеет конструкторы, служащие для создания значений этого типа,
-- и деструкторы, служащие для анализа значений

-- False и True -- конструкторы типа Bool

-- Типы и конструкторы пишутся с большой буквы.

-- Иногда типы и конструкторы имеют одинаковое имя

data SameName = SameName

data Day = Day Int

-- Здесь SameName :: SameName
-- Пустой кортеж () :: ()

-- Конструкторы могут использоваться как образцы в определении функции
-- Так реализуются деструкторы

not' :: Bool -> Bool
not' True = False
not' False = True

-- Еще пример (аналог enum в С и Java)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King

-- Конструктор с аргументами: точка в двумерном пространстве

data Point = Pt Float Float

-- Pt :: Float -> Float -> Point

px :: Point -> Float
px (Pt x _) = x

-- Именованные поля (записи)

data Point2D = P2D {p1 :: Float, p2 :: Float} deriving Show
-- То же, что data Point2D = P2D Float Float
-- но автоматически генерируются проекторы
-- p1 :: Point2D -> Float
-- p2 :: Point2D -> Float

point1 :: Point2D
point1 = P2D 1.0 2.0

point2 :: Point2D
point2 = P2D {p2 = 2.0, p1 = 1.0} -- в любом порядке

-- Создание новой записи, которая отличается значением p1
point3 :: Point2D
point3 = point1 {p1 = 3.0}

-- Попытка напечатать значение типа Point раньше вело к ошибке
-- Можно добавить deriving (Eq, Show) к объявлению типа
-- для автоматического добавления вновь созданного типа к классам Eq и Show

data Point'' a = Pt'' a a deriving (Eq, Show)

-- Тип Maybe в Prelude для функций, которые могут не возвращать значение

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' key [] = Nothing
lookup' key ((k,v) : dict)
  | key == k = Just v
  | otherwise = lookup' key dict

-- Рекурсивные (индуктивные) типы

data List a = Nil | Cons a (List a) deriving Show

-- Cons 'a' (Cons 'b' (Cons 'c' Nil)) :: List Char

-- Если команде ghci дать опцию -XGADTs в командной строке или добавить
-- первую строчку, как этом файле, то можно записывать алгебраические типы
-- в следующем виде

data List' a where
  Nil' :: List' a
  Cons' :: a -> List' a -> List' a

data Tree a = Empty | Leaf a | Branches (Tree a) (Tree a) deriving Show

-- Деревья с произвольным количеством детей в каждом узле

-- data Tree a = Node a [Tree a]

-- Как написать деревья со счетным числом детей?

data InfTree a where
  Leaf' :: a -> InfTree a
  Children :: Integer -> InfTree a

-- Пример из книги Холомьёва, с. 112

-- Синонимы типа
type Velocity = Double
type Time = Double
type Distance = Double

velocity :: Distance -> Time -> Velocity
velocity dist time = dist / time

d :: Distance
d = 10

t :: Time
t = 5

v :: Velocity
v = velocity t d -- не вызывает ошибку несмотря на неправильные единицы

-- Обёртка вокруг типа. Имеет один конструктор, у которого один аргумент.
-- Конструктор имеет значение только при проверке типов и прозрачен во
-- время исполнения.

newtype Velocity' = Velocity' Double
newtype Time' = Time' Double
newtype Distance' = Distance' Double

velocity' :: Distance' -> Time' -> Velocity'
velocity' (Distance' dist) (Time' time) = Velocity' $ dist / time

d' :: Distance'
d' = Distance' 10

t' :: Time'
t' = Time' 5

v' :: Velocity'
-- v' = velocity' t' d' вызывает ошибку
v' = velocity' d' t'

-- Классы типов описаны в разделах 1.4 и 1.5 (с. 18–20)
-- в книге Холомьёва «Учебник по Haskell»

infixl 6 #

-- Объявим класс Monoid, который является является подклассом Eq
-- Строго говоря, импликация направлена в другую сторону:
-- если a есть тип класса Monoid, то он также тип класса Eq
class Eq a => Monoid a where
  (#) :: a -> a -> a
  e :: a

-- Объявим Nat членом класса Eq
-- Для этого объявим функцию (==)
-- Её можно определить прямо здесь или заранее, а здесь указать
-- только имя ранее определенной функции
instance Eq Nat where
  Zero == Zero = True
  Succ x == Succ y = x == y
  _ == _ = False

instance Monoid Bool where
  e = False
-- операцию можно определить одним из следующих образов
--  (#) = (||)
  x # y = x || y

-- Теперь можно писать
bb = True # False
 
instance Eq a => Monoid [a] where
  e = []
  (#) = (++)
 
double :: Monoid a => a -> a
double x = x # x
 
instance Monoid Nat where
  e = Zero
  (#) = add
 
-- Домашнее задание
-- 1. Определите умножение на Nat.

-- 2. Объявите Nat членом класса Monoid двумя способами:
-- с операцией сложения и нулём, а также умножения и единицей.
-- Воспользуйтесь обёрткой newtype вокруг Nat.
-- См. Холомьёв, с. 113.

-- Следующий тип определяет арифметические выражения, состоящие из
-- целых чисел, сложения, вычитания и умножения.

data Exp =
  Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp

-- Например, выражение (5-3)((1+2)+4) представляется

e1 :: Exp
e1 = Mul (Sub (Const 5) (Const 3)) (Add (Add (Const 1) (Const 2)) (Const 4))

-- 3. Напишите функцию showExp :: Exp -> String, которая выдает
-- инфиксную запись выражения. Каждое подвыражение должно быть окружено
-- скобками. Например, на выражении выше функция должна выдавать
-- ((5-3)*((1+2)+4)).

-- 4. Зарегистрируйте тип Exp в качестве члена класса Show, используя
-- showExp как реализацию функции show. Проверьте, что выражения,
-- заданные в командной строке, печатаются должным образом.

-- 5. Напишите рекурсивную функцию eval :: Exp -> Int, вычисляющую
-- значение выражения.

-- 6. Напишите рекурсивную функцию applyDistr :: Exp -> Exp, которая
-- применяет законы дистрибутивности (слева) умножения относительно
-- сложения и вычитания:
-- x(y+z) = xy + xz
-- x(y-z) = xy - xz
-- Функция должна за один проход заменять все подвыражения вида
-- x(y +- z) на соответствующую правую часть. Проверьте на примерах,
-- что значение выражения не меняется после применения applyDistr},
-- то есть проверьте, что eval e == eval (applyDistr e)
-- возвращает True на различных выражениях e.

-- 7. Докажите, что функция applyDistr останавливается на любом входе.

-- 8. Прочитайте про двумерный синтаксис в Haskell Language Report §2.7
-- на русском или английском.
