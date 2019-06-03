module BooleanSyntax where

data Op = T | F | Neg | And | Or | If | Iff | Xor | Nand | Nor
  deriving (Eq, Enum, Bounded)

arity :: Op -> Int
arity T = 0
arity F = 0
arity Neg = 1
arity And = 2
arity Or = 2
arity If = 2
arity Iff = 2
arity Xor = 2
arity Nand = 2
arity Nor = 2

prec :: Op -> Int
prec T = 0
prec F = 0
prec Neg = 8
prec And = 7
prec Or = 6
prec If = 4
prec Iff = 5
prec Xor = 6
prec Nand = 6
prec Nor = 6

-- Любая операция с приоритетом 0, которая используется при
-- переводе формулы в строку
noOp :: Op
noOp = T

-- Возвращает текстовое представление связок
opText :: Op -> ShowS
opText T = showString "1"
opText F = showString "0"
opText Neg = showString "~"
opText And = showString " "
opText Or = showString " v "
opText If = showString " -> "
opText Iff = showString " <-> "
opText Xor = showString " + "
opText Nand = showString " | "
opText Nor = showString " v "

-- В дальнейшем
-- Возвращает представление связок в LaTeX
-- connLaTeX :: Op -> ShowS

-- Возвращает имена переменных по номеру
varName :: Int -> ShowS
varName 0 = showChar 'x'
varName 1 = showChar 'y'
varName 2 = showChar 'z'
varName 3 = showChar 'u'
varName 4 = showChar 'v'
varName 5 = showChar 'w'

-- В отличие от Haskell, где операторы могут быть не иметь
-- ассоциативности или быть лево- или правоассоциативными, будем
-- различать следующие случаи.

-- 1) Ассоциативная операция (And, Or, Xor, Iff): в последовательности
-- одинаковых операций скобки не пишутся, поскольку могут
-- расставляться любым способом.

-- Операции ниже не являются ассоциативными.

-- 2) Правоассоциативная операция. Если в последовательности
-- одинаковых операций скобки остутствуют, подразумевается, что они
-- группируются вправо (If).

-- 3) Левоассоциативная операция. То же, что 2), только скобки
-- группируются влево (в данном наборе таких операций нет).

-- 4) Неассоциативная операция без соглашения о группировки скобок
-- (Nand, Nor).  Скобки должны указываться явно, и отсутствие скобок в
-- последовательности одинаковых операций не допускается.

data AssocType = FA | LA | RA | NA deriving Eq

-- FA (full associativity): случай 1)
-- LA (left associativity): случай 3)
-- RA (right associativity): случай 2)
-- NA (no associativity): случай 4)

assoc :: Op -> AssocType
-- T, F, Neg не являются бинарными операторами, поэтому их
-- ассоциативность не определена
assoc T = NA
assoc F = NA
assoc Neg = NA
assoc And = FA
assoc Or = FA
assoc If = RA
assoc Iff = FA
assoc Xor = FA
assoc Nand = NA
assoc Nor = NA

-- Область определения и значения операций.
-- Должен быть членом класса Enum
type Domain = Bool

evalOp :: Op -> [Domain] -> Domain
evalOp T [] = True
evalOp F [] = False
evalOp Neg [x] = not x
evalOp And [x, y] = x && y
evalOp Or [x, y] = x || y
evalOp If [x, y] = not x || y
evalOp Iff [x, y] = x == y
evalOp Xor [x, y] = x /= y
evalOp Nand [x, y] = not (x && y)
evalOp Nor [x, y] = not (x || y)
