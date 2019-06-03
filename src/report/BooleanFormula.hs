
-- Алексей Лицов, группа 381803-4м
-- adlitsov@gmail.com

module BooleanFormula where

import BooleanSyntax
import Formula
import System.Random
import Control.Monad.State

-- Этот модуль посвящен работе с булевыми формулами в отличие от
-- Formula.hs, который работает с формулами общего вида.

-- Задание

-- 1. Зарегистрировать в Formula.hs тип Formula как член класса Show,
-- используя функцию showFormula, которая возвращает текстовое
-- представление формулы с учетом приоритета и ассоциативности
-- операций.

-- 2. В этом файле написать функции, проверяющие, является ли функция,
-- определяемая данной формулой, тождественно истинной (ложной),
-- самодвойственной, монотонной и линейной.


-- Проверка на константу
constantTrue1 =  C Neg [C F []]
constantTrue2 =  C T []
constantFalse1 = C And [C F [], C T []]
constantFalse2 = C And [V 0, C Neg [V 0]]

-- Главная
isConstant :: Formula -> Bool
isConstant formula = isConstantTrue formula || isConstantFalse formula

-- Главная
isConstantTrue :: Formula -> Bool
isConstantTrue (C anOp []) = anOp == T
isConstantTrue formula =
    let
        varsCount = length (collectVars formula)
    in
        isAllElementsEqualTo (map (\x -> eval x formula) (allEnvs varsCount)) True

-- Главная
isConstantFalse :: Formula -> Bool
isConstantFalse (C anOp []) = anOp == F
isConstantFalse formula =
    let
        varsCount = length (collectVars formula)
    in
        isAllElementsEqualTo (map (\x -> eval x formula) (allEnvs varsCount)) False

-- Вспомогательная
isAllElementsEqualTo :: [Domain] -> Domain -> Bool
isAllElementsEqualTo [] _ = True
isAllElementsEqualTo (x: xs) val = (x == val) && isAllElementsEqualTo xs val

-- Проверка на самодвойственность
notSelfDual1 = C And [V 0, V 1]
notSelfDual2 = C If [V 0, V 1]
notSelfDual3 = C Or [C And [V 0, V 1], V 2]
-- ~x ~y v ~x ~z v ~y ~z  - пример самодвойственной функции
selfDual1 = C Or [C Or [C And [C Neg [V 0], C Neg [V 1]], C And [C Neg [V 0], C Neg [V 2]]],
                        C And [C Neg [V 1], C Neg [V 2]]]

-- Главная
isSelfDual :: Formula -> Bool
isSelfDual (C anOp []) = False
isSelfDual formula =
    let
        varsCount = length (collectVars formula)
        vector = map (\x -> eval x formula) (allEnvs varsCount)
    in
        selfDualComparison vector vector

-- Вспомогательная
-- Проверяет вектор на двойственность, нужно передать в оба параметра один и тот же вектор для проверки
selfDualComparison :: [Domain] -> [Domain] -> Bool
selfDualComparison [] [] = True
selfDualComparison (x:xs) snd = (x /= last snd) && selfDualComparison xs (init snd)

-- Проверка на монотонность
monotonic1 = V 0
monotonic2 = C And [V 0, V 1]
notMonotonic1 = C Xor [C And [V 0, V 1], C Neg [V 2]]

-- Главная
-- Проверяет монотонность вектора с помощью функции ниже по алгоритму из учебника
isMonotonic :: Formula -> Bool
isMonotonic formula =
    let
        varsCount = length (collectVars formula)
        vector = map (\x -> eval x formula) (allEnvs varsCount)
    in
        isVectorMonotonic vector

-- Вспомогательная
-- Проверяет монотонность по алгоритму из учебника
isVectorMonotonic :: [Domain] -> Bool
isVectorMonotonic [] = True
isVectorMonotonic [x] = True
-- isVectorMonotonic [x] = True
isVectorMonotonic vec =
    let
        len = length vec
        halves = splitAt (len `div` 2) vec
    in
        if (not (isFstVectorSmaller halves)) then
            -- Если векторы сравнимы и первый меньше второго - истина, иначе - ложь
            False
        else
            isVectorMonotonic (fst halves) && isVectorMonotonic (snd halves)

-- Вспомогательная
isFstVectorSmaller :: ([Domain], [Domain]) -> Bool
isFstVectorSmaller ([], []) = True
isFstVectorSmaller (x:xs, y:ys) =
    if (x == True && y == False) then
        False
    else
        isFstVectorSmaller (xs, ys)

-- Проверка на линейность
linear1 = C T []
linear2 = C Xor [V 0, V 1]
notLinear1 = C Xor [C And [V 0, V 1], C T []]

-- Главная
isLinear :: Formula -> Bool
isLinear formula =
    let
        varsCount = length (collectVars formula)
        vector = map (\x -> eval x formula) (allEnvs varsCount)
    in
        isFalseList (removeSquares (buildTriangleUpperRow vector))

-- Удаляет элементы, позиции которых являются степенями двойки и нулевой элемент (им соотв. несмешанные элементы полинома
-- Жегалкина)
removeSquares :: [Domain] -> [Domain]
removeSquares [] = []
removeSquares list =
    map fst $ filter (\x -> (snd (properFraction(logBase 2 (snd x))) /= 0) && snd x /= 0) indexed
    where
        indexed = zip list [0..]

isFalseList :: [Bool] -> Bool
isFalseList [] = True
isFalseList (x:xs) = (x == False) && isFalseList xs

-- Вспомогательная
-- Строит первую строку вспомогальной треугольной таблицы
-- Принимает построенный на предыдущем шаге столбец значений
buildTriangleUpperRow :: [Domain] -> [Domain]
buildTriangleUpperRow [] = [] -- row
buildTriangleUpperRow [x] = [x] -- row
buildTriangleUpperRow column =
    head column : (buildTriangleUpperRow (buildColumnElements column))

-- Вспомогательная.
-- Вход: предыдущий столбец, выход - новый
buildColumnElements :: [Domain] -> [Domain]
buildColumnElements [] = []
buildColumnElements [x] = []
buildColumnElements (x:xs) = evalOp Xor [x, head xs]  : buildColumnElements xs



-- 3. В этом файле написать функцию, генерирующую случайные формулы
-- заданной глубины с использованием генератора случайных чисел из
-- библиотеки Haskell.

-- randomInt (m, n) возвращает случайное число m <= k <= n.
-- Использует следующую функцию из System.Random.
-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
-- Напоминание: класс Random включает типы, значения которых можно
-- генерировать случайным образом. Содержит Int, Float, Char, Bool и др.
randomInt :: (Int, Int) -> State StdGen Int
randomInt range = state (randomR range)

-- Принимает монадное значение (действие) и начальное значение для
-- генератора случайных чисел. Возвращает настоящее значение.
-- Второй аргумент может быть произвольным целым.
runR :: State StdGen a -> Int -> a
runR action seed = fst (runState action (mkStdGen seed))

-- chooseAction [(p1, a1), ..., (pn, an)] k возвращает aj, такое что
-- sum_{i=1}^{j-1} pi <= k < sum_{i=1}^j pi.
-- Требования: n > 0, p1 > 0, ..., pn > 0, 0 <= k < sum_{i=1}^n pi.
-- Не использует случайные числа или монады.
chooseAction :: [(Int, a)] -> Int -> a
-- Разворачивает исходный список в список первых элементов и возвращает элемент с индексом, удовлетворяющим условиям
chooseAction xs k = getSecondElements xs !! (findProperIndex (getFirstElements xs) k)

-- Вспомогательная
-- Вовзращает список из первых элементов кортежей
getFirstElements::[(Int, a)] -> [Int]
getFirstElements xs = fst (unzip xs)

-- Вспомогательная
-- Вовзращает список из вторых элементов кортежей
getSecondElements::[(Int, a)] -> [a]
getSecondElements xs = snd (unzip xs)

-- Вспомогательная
-- Принимает список первых элементов и k, возвращает индекс подходящего элемента
findProperIndex :: [Int] -> Int -> Int
findProperIndex (y:ys) k = findIndexInternal [y] ys k 1

-- Вспомогательная
-- Используется функцией findProperIndex, внутри себя проверяет требуемое условие для каждого элемента и возвращает
-- индекс первого элемента, который ему удовлетворяет
findIndexInternal :: [Int] -> [Int] -> Int -> Int -> Int
findIndexInternal xs (y:ys) k i
    | (k <= (sum xs)) = i
    | otherwise = findIndexInternal (y:xs) ys k (i+1)

-- Принимает список монадных значений (действий) с целочисленными весами.
-- Выбирает и возвращает одно из действий с помощью случайного числа,
-- полученного от randomInt.
frequency :: [(Int, State StdGen a)] -> State StdGen a
frequency xs = do num <- randomInt (0, (sum (fst (unzip xs)))-1)
                  chooseAction xs num

-- Возвращается одно из V 0, ..., V (n-1)
-- Требование: n > 0
randomVar :: Int -> State StdGen Formula
randomVar n = do num <- randomInt (0, n-1)
                 return (V num)

-- Возвращает формулу T или F с вероятностью 0,5
randomConst :: State StdGen Formula
randomConst = frequency [ (1, return (C F [])), (1, return (C T [])) ]

-- Возвращает случайную бинарную связку. Если все семь связок, объявленных
-- в BooleanSyntax, равновероятны, то можно дать следующее определение.
randomBin :: State StdGen Op
randomBin = frequency [(1, return And), (1,  return Or), (1, return If),
                       (1, return Iff), (1,  return Xor), (1, return Nand),
                       (1, return Nor)]

-- Возвращает случайную формулу в виде монадного значения.
-- Первый аргумент: максимальная глубина формулы.
-- Второй аргумент: максимальное количество переменных в формуле
rf :: Int -> Int -> State StdGen Formula
rf 0 n = randomVar n
rf deep n = do f1 <- rf (deep-1) n
               f2 <- rf (deep-1) n
               op <- randomBin
               return (C op [f1, f2])

-- Возвращает случайную формулу.
-- Первый аргумент: максимальная глубина.
-- Второй аргумент: максимальное количесвто переменных.
-- Третий аргумент: начальное значение для генератора случайных чисел.
randomFormula :: Int -> Int -> Int -> Formula
randomFormula deep count seed = runR (rf deep count) seed

-- Литература
-- ГС: Гаврилов Г.П., Сапоженко А.А. Задачи и упражнения по
-- дискретной математике. М.: Физматлит, 2005.

-- Проверка на самодвойственность: ГС, с. 64.

-- Проверка на монотонность: ГС, с. 76.

-- Проверка на линейность: ГС, с. 53, метод построения полинома
-- Жегалкина, базирующийся на преобразовании вектора значений функции.
-- Альтернативно: https://ru.wikipedia.org/wiki/%D0%9F%D0%BE%D0%BB%D0%B8%D0%BD%D0%BE%D0%BC_%D0%96%D0%B5%D0%B3%D0%B0%D0%BB%D0%BA%D0%B8%D0%BD%D0%B0
-- Построение полинома Жегалкина методом Паскаля (то же, что в ГС) или
-- методом треугольника.


