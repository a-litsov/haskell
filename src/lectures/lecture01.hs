-- Интерпретатор: ghci
-- Простая интегрированная среда под Windows: WinGHCI
-- Интерпретатор работает в режиме REPL (read, evaluate, print, loop)
-- :? информация
-- :cd /path/to/files изменить рабочий каталог (каталог без кавычек)
-- :show paths
-- :! dir исполнить команду shell

-- Основные типы данных
-- 5
-- True
-- 'a'
-- (1, True)
-- [1, 2, 3, 4, 5]
-- 5 + 6
-- x - 1
-- 5 :: Integer -- произвольная длина
-- 5 :: Int -- [-2^29 .. 2^29-1]
-- 3.14 :: Float -- одинарная точность
-- 2.718281828459045 :: Double -- двойная точность
-- True :: Bool
-- 'a' :: Char
-- 4 + 5
-- [1, 3, 4, 2]
-- [[1, 2], [], [3, 1]]
-- "hello" == ['h', 'e', 'l', 'l', 'o']
-- Polymorphism: функция length принимает списки элементов любого типа
-- length [1, 2, 3, 4]
-- :t (+) напечатать тип выражения
-- :t (>)
-- :set +t включить режим печатания типа
-- :unset +t включить режим печатания типа
-- :info (+), :i (+) напечатать информацию об идентификаторе
-- (+), (-), (*), (/), div, mod, (^), 
-- sum, product, max, min, maximum, minimum, even, odd, gcd, lcm
-- 5 `mod` 3
-- (>), (<), (==), (/=), (>=), (<=), (&&), (||), not, and, or
-- head [3, 2, 1]
-- head "hello"
-- tail [3, 2, 1]
-- null, []
-- (:) конструирование списка из головы и хвоста
-- last, init, length, (!!), (++), concat, take, drop, reverse, elem, replicate
-- см. тип и примеры, или см. описание в Prelude

-- Дополнительные функции на списках
-- import Data.List
-- :m + Data.List
-- :m - Data.List

-- Первый и второй элементы упорядоченной пары
-- fst, snd

-- :load file.hs, :l file.hs загрузить файл (имя файла без кавычек)
-- :r повторно загрузить последний загруженный файл

fact :: Integer -> Integer
fact 0 = 1
fact x = x * fact (x-1)

sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

solve a b c
  | d < 0 = []
  | otherwise = [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]
    where
      d = b * b - 4 * a * c

solve1 a b c =
  let d = b * b - 4 * a * c in
    if d < 0 then [] else [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]

-- В строке интерпретатора (в отличие от текста программы)
-- команды, занимающие несколько строк следует заключать в
-- :{ ... :}, а определение функций начинать с let.
-- Это используется редко.
-- :{
-- let fact 0 = 1
--     fact x = x * fact (x - 1)
-- :}

-- :list fact напечатать определение функции

plus :: (Int, Int) -> Int
-- plus z = (fst z) + (snd z)
plus (x, y) = x + y

-- Для кортежей длины 3 нет стандартных функций
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

plus1 x y = plus (x, y)

-- (+) 1

inc = (+) 1

-- (2^) == (^) 2
-- (2^) 3
-- (^2) 3

-- Образцы (patterns):
-- []
-- pat1 : pat2
-- (pat1, pat2 ... , patn)
-- name
-- _
-- constant
-- [pat1, pat2 ... , patn]

safediv _ 0 = 0
safediv x y = x / y
