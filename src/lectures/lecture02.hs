-- Если нужно переопределить функции из Prelude,
-- поместите следующую строчку в начале файла
import Prelude hiding ((^), (^^), even, odd)

-- Теперь можно переопределить функцию из Prelude
even n = (mod n 2) == 0

-- Образцы можно использовать не только при определении функции, но и в let
-- let (x : xs, 3, y) = ([1, 2, 3], 3, "abc")
-- Чтобы использовать сопоставление с образцом в любом месте, нужна
-- конструкция case (см. ниже)

-- Как написать head, tail?
-- Штрих (апостроф) можно использовать в идентификаторах

head' (x : xs) = x
tail' (x : xs) = xs

-- Рекурсия

-- Увеличивает каждый элемент списка на 1
-- Несколько стилей определения функции
inclist [] = []
inclist (x : xs) = x + 1 : inclist xs

inclist1 [] = []
inclist1 xs = head xs + 1 : inclist1 (tail xs)

inclist2 xs = if null xs then [] else (head xs) + 1 : inclist2 (tail xs)

inclist3 l =
  case l of
  [] -> []
  (x : xs) -> x + 1 : inclist3 xs

lngth [] = 0
lngth (_ : xs) = 1 + lngth xs

product' [] = 1
product' (x : xs) = x * product' xs

-- Обращение списка
-- Нехвостовая рекурсия, квадратичная сложность
reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

addToEnd :: a -> [a] -> [a]
addToEnd x [] = [x]
addToEnd x (y : ys) = y : addToEnd x ys

reverse'' [] = []
reverse'' (x : xs) = addToEnd x (reverse'' xs)

-- Хвостовая рекурсия, линейная сложность
-- reverse2 l1 l2 добавляет обращение l1 слева к l2
reverse2 :: [a] -> [a] -> [a]
reverse2 [] ys = ys
reverse2 (x : xs) ys = reverse2 xs (x : ys)

rev xs = reverse2 xs []

-- Следующие определения выглядят похоже,
-- но есть большая разница в поведении и в сложности.
-- append (конкатенация списков, или ++)
foo1 [] acc = acc
foo1 (x : xs) acc = x : foo1 xs acc

-- reverse
foo2 [] acc = acc
foo2 (x : xs) acc = foo2 xs (x : acc)

-- факториал с нехвостовой рекурсией
fact 0 = 1
fact n = n * fact (n - 1)

-- факториал с хвостовой рекурсией
fact1 0 m = m
fact1 n m = fact1 (n - 1) (n * m)

fact2 n = fact1 n 1

-- Ленивые вычисления

-- См. функцию const в Prelude

-- Следующие определения эквивалентны
myConst x = \ y -> x
myConst' x y = x
myConst'' = \ x y -> x
myConst''' = \ x -> \ y -> x

-- undefined - это функция без аргументов, вызывающая исключение

-- Следующие вызовы не вызывают исключение, потому что ненужные
-- подвыражения не вычисляются.
-- const "hello" undefined
-- length [undefined]
-- let f (_, _) = 1 in f (undefined, 2)

-- Бесконечный список из элемента x
myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

-- take n l возвращает первые n элементов списка l
-- take 5 (myRepeat 1)

-- replicate через repeat
myReplicate n x = take n (myRepeat x)

naturals = naturalsFrom 1
naturalsFrom n = n : naturalsFrom (n + 1)

factorial n = product (take n naturals)

-- еще один вариант
-- factorial n = product [1..n]

-- Следующее определение правильно с математической точки зрения
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]
-- или
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Что будет, если функции дать бесконечный список?
-- 1. Вернет конечный результат. Пример: take
-- 2. Вернет бесконечный список, с которым можно работать,
--    если не вычислять его целиком.
--    Пример: take 3 (drop 2 naturals)
-- 3. Не остановится. Пример: length naturals

-- Вычисление выражения в интепретаторе вызывает функцию show,
-- которая преобразовывает полученное значение в строку.
-- Значение анализируется целиком, поэтому вычисление в интепретаторе
-- (at the prompt) энергичное, а не ленивое.

-- Арифметические последовательности

-- [1 .. 10] -> [1,2,3,4,5,6,7,8,9,10]
-- [1, 3 .. 10] -> [1,3,5,7,9]
-- [10, 9 .. 1] -> [10,9,8,7,6,5,4,3,2,1]
-- [1 ..] -> [1,2,3,4,5,6,7,8,9,10,...]

-- Генераторы (или замыкания) списков (list comprehension)
-- [x^2 | x <- [1..10]]
-- [(x, y) | x <- [1..3], y <- "ab"]
-- [выражение | образец <- список, образец <- список, ... , условие, условие, ... ]

qsort [] = []
qsort (x : xs) =
  qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

