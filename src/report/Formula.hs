
-- Алексей Лицов, группа 381803-4м
-- adlitsov@gmail.com

{-# LANGUAGE TemplateHaskell #-}

module Formula where

import BooleanSyntax
import Test.QuickCheck
import Test.QuickCheck.All

type Var = Int

-- C означает "compound", т.е. "составная"
data Formula = V Var | C Op [Formula]

-- Примеры формул ниже работают, если не ограничивать импорт из модуля
-- BooleanSyntax. С указанными ограничениями импортируется тип Connective,
-- но не его конструкторы T, F, Neg, And и т.д.
-- Однако, определения form1 и form2 работают в командной строке.

-- form1 = x v y -> ~z
form1 :: Formula
form1 = C If [C Or [V 0, V 1], C Neg [V 2]]

-- form2 = xy + ~z <-> x v z
form2 :: Formula
form2 = C Iff [C Xor [C And [V 0, V 1], C Neg [V 2]], C Or [V 0, V 2]]

-- Определяет, совпадает ли арность с размером списка формул (=аргументов) операции и делает редукцию
-- списка формул, при этом рекурсивно вызывая саму себя для каждой формулы в списке
correctArity :: Formula -> Bool
correctArity (V v) = True
correctArity (C anOp formulas) =
    let
        currentArity = length formulas
    in
        if (not (currentArity `elem` [0, 1, 2])) then
            arityError
        else
            foldr (\formula isPrevCorrect -> (correctArity formula) && isPrevCorrect) True formulas
                && (arity anOp == currentArity)


arityError = error "Arity other than 0, 1 or 2"

-- Значения переменной с номером i есть i-й элемент списка
type Environment = [Domain]

-- Просто возвращает i-й элемент списка
lookupVar :: Environment -> Var -> Domain
lookupVar env varNum = env !! varNum

-- Считаем внутренниие формулы, делаем редукцию результатов в список - получаем список аргументов для внешней операции
-- и выполняем её
eval :: Environment -> Formula -> Domain
eval env (V v) = lookupVar env v
eval env (C anOp formulas) =
    evalOp anOp (foldr (\formula prevResults -> (eval env formula : prevResults)) [] formulas)

-- Текстовое представление формул

-- Вариант, где каждая составная подформула окружена скобками

-- Решил сделать внутреннюю функцию, чтобы на самом верхнем уровне не было скобок
fullParen :: Formula -> ShowS
fullParen (V v) = varName v
fullParen (C anOp formulas) =
     if (arity anOp < 2 ) then fullParenInner (C anOp formulas)
     else fullParen(head formulas) . opText anOp . fullParen(last formulas)

-- В зависимости от арности операции выбираем, когда нужно поставить скобки и сколько раз вызвать
-- рекурсивно на листе formulas. Эта версия всегда окружает скобками подформулу, даже на самом верхнем
-- уровне, когда это не нужно.
fullParenInner :: Formula -> ShowS
fullParenInner (V v) = varName v
fullParenInner (C anOp formulas) =
    case (arity anOp) of
        0 -> opText anOp
        1 -> opText anOp . fullParen(head formulas)
        2 -> showString "(" . fullParen(head formulas)
            . opText anOp .
            fullParen(last formulas) . showString ")"


-- Вариант, учитывающий приоритет и ассоциативность операций

-- Скобки вокруг констант (связок арности 0) не ставятся.
-- Операции арности 1 являются префиксными или отображаются
-- специальным образом. Например, C Neg [f] отображается как ~f
-- в тексте и \overline{f} в LaTeX.

-- Инфиксные операции

-- Пусть данная формула (второй аргумент функции ниже) является левым
-- аргументом операции opExt, а главная операция формулы есть opInt.
-- Скобки вокруг формулы ставятся тогда и только тогда, когда
-- 1) приоритет opExt строго больше приоритета opInt, или
-- 2) приоритет opExt равен приоритету opInt и
-- 2а) opExt <> opInt, или
-- 2б) opExt = opInt имеет ассоциативность RA или NA.

-- Если данная формула является правым аргументом opExt, то в пункте 2б)
-- нужно заменить RA на LA.
-- Добавил здесь deriving Eq, чтобы была возможность сравнить их в 110 строке
data ArgPlace = LeftArg | RightArg deriving Eq

showFormula :: Op -> ArgPlace -> Formula -> ShowS
showFormula opExt argPlace (C opInt formulas) =
    let
        -- Виды ассоциативности из 2б
        assocTypes = if argPlace == LeftArg then [RA, NA] else [LA, NA]
        -- Нужно ли ставить скобки вокруг данной подформулы
        shouldPlace = (prec opExt > prec opInt) ||
            ( (prec opExt == prec opInt) && ((opExt /= opInt) || ((opExt == opInt) && (assoc opExt `elem` assocTypes))) )
    in case (length formulas) of
        -- Поменял во второй версии здесь opExt на opInt, теперь формулы вида 'C Neg [C T []]' печатаются корректно
        0 -> opText opInt
        1 -> if (shouldPlace) then
                showString "(" . opText opInt . showFormula opInt LeftArg (head formulas)
             else
                opText opInt . showFormula opInt LeftArg (head formulas)
        2 -> if (shouldPlace) then
                showString "(" . showFormula opInt LeftArg (head formulas)
                . opText opInt .
                showFormula opInt RightArg (last formulas) . showString ")"
             else
                showFormula opInt LeftArg (head formulas)
                . opText opInt .
                showFormula opInt RightArg (last formulas)
showFormula opExt argPlace (V v) = varName v

prop_form1 = showFormula noOp LeftArg form1 "" == "x v y -> ~z"
prop_form2 = showFormula noOp LeftArg form2 "" == "x y + ~z <-> x v z"
instance Show Formula where
--   show f = fullParen f ""
 show f = showFormula noOp LeftArg f ""

-- Возвращает отсортированный список переменных, входящих в формулу.
-- Каждая переменная входит в список не более одного раза.
-- Сливаем упорядоченные списки, начинаем с пустого и построенного для крайней правой формулы
-- Порядок гарантирован правильный, поскольку в первый раз вставляем в пустой список
collectVars :: Formula -> [Int]
collectVars (V v) = [v]
collectVars (C op formulas) = foldr (\fst res -> mergeUniqueSortedLists (collectVars fst) res) [] formulas

-- Вспомогательная для collectVars, сливает два упорядоченных списка
mergeUniqueSortedLists :: [Int] -> [Int] -> [Int]
mergeUniqueSortedLists fst [] = fst
mergeUniqueSortedLists [] snd = snd
mergeUniqueSortedLists fst snd = foldr (\curElem res -> insertInSortedUnique res curElem) fst snd

-- Вспомогательная для mergeUniqueSortedLists, вставляет в упорядоченный уникальный список очередное уникальное значение
insertInSortedUnique :: [Int] -> Int -> [Int]
insertInSortedUnique list x
    | list == [] = [x]
    | head list == x = list
    | head list < x = (head list) : (insertInSortedUnique (tail list) x)
    | head list > x = x : list



-- Принимает количество n переменных в формуле (вернее, номер максимальной
-- переменной плюс 1) и возвращает список всех окружений длины n в
-- лексикографическом порядке, где порядок на компонентах окружения
-- определяется членством Domain в Enum
allEnvs :: Int -> [Environment]
allEnvs 0 = [[]]
allEnvs n =
    let
        -- Строим нужный список для длины n-1
        prev = allEnvs (n-1)
        -- Получаем все допустимые значения (Domain в Bounded классе)
        domainValues = [minBound::Domain .. ]
    in
        -- Для каждого окружения длины n-1 мы добавляем каждый компонент из Domain, лексикографический порядок сохранён
        foldl (\res current -> res ++ (map (\value -> current ++ [value]) domainValues)) [] prev

return []
runTests :: IO Bool
runTests = $quickCheckAll