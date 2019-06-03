module Formula where

import BooleanSyntax
--  (Op, AssocType, Domain, arity, prec, noOp, opText, varName, assoc,
--   evalOp)

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

-- form2 = xy + z <-> x v z
form2 :: Formula
form2 = C Iff [C Xor [C And [V 0, V 1], C Neg [V 2]], C Or [V 0, V 2]]

-- correctArity :: Formula -> Bool

arityError = error "Arity other than 0, 1 or 2"

-- Значения переменной с номером i есть i-й элемент списка
type Environment = [Domain]

-- lookupVar :: Environment -> Var -> Domain

-- eval :: Environment -> Formula -> Domain

-- Текстовое представление формул

-- Вариант, где каждая состовная подформула окружена скобками

-- fullParen :: Formula -> ShowS

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

data ArgPlace = LeftArg | RightArg

-- showFormula :: Op -> ArgPlace -> Formula -> ShowS
-- showFormula opExt f =

-- instance Show Formula where
--   show f = fullParen f ""
--  show f = showFormula noOp LeftArg f ""

-- Возвращает отсортированный список переменных, входящих в формулу.
-- Каждая переменная входит в список не более одного раза.
-- collectVars :: Formula -> [Int]

-- Принимает количество n переменных в формуле (вернее, номер максимальной
-- переменной плюс 1) и возвращает список всех окружений длины n в
-- лексикографическом порядке, где порядок на компонентах окружения
-- определяется членством Domain в Enum
-- ellEnvs :: Int -> [Env]

