-- Основано на "Лекциях по функциональному программированию", раздел 9.
-- Очень рекомендуется к прочтению.
-- http://pv.bstu.ru/?topic=flp

-- Для полноты некоторые типы и функции из Prelude определены в данном файле

import Prelude hiding
  (Maybe, Nothing, Just, Either, Left, Right, lookup, Monad, (>>=))

type Name = String
type Address = String
type Phone = Integer
type AddressDB = [(Name, Address)]
type PhoneDB = [(Address, Phone)]

-- Некоторые имена могут соотвествовать нескольким адресам

addressDB :: AddressDB
addressDB = [("Smith", "address1"), ("Smith", "address2"), ("Jones", "address3")]

-- Некоторые адреса могут соотвествовать нескольким номерам телефонов

phoneDB :: PhoneDB
phoneDB = [("address1", 1234567), ("address2", 7654321), ("address2", 5555555)]

-- Задача: получить номер телефона по имени

-- ******************************************
-- Вариант 1 (с возвращаемым типом Maybe ...)

-- Из Prelude
data Maybe a  =  Nothing | Just a
  deriving (Eq, Ord, Show)

-- Из Prelude

lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x          =  Just y
    | otherwise         =  lookup key xys

-- Композиция двух lookup

nameToPhone1 :: Name -> Maybe Phone
nameToPhone1 name =
  case (lookup name addressDB) of
  Just address -> lookup address phoneDB
  Nothing -> Nothing

-- Постараемся вынести детали композиции, имеющие дело с Maybe,
-- в отдельную функцию thenMaybe

thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe (Just x) f = f x
thenMaybe Nothing _ = Nothing

nameToPhone1' :: Name -> Maybe Phone
nameToPhone1' name =
  lookup name addressDB `thenMaybe` (\address -> lookup address phoneDB)

-- Примеры
-- nameToPhone1 "Smith" (первый из двух телефонов)
-- nameToPhone1 "Jones" (нет телефона)
-- nameToPhone1 "Brown" (нет адреса)

-- ******************************************
-- Вариант 2 (с возвращаемым типом Either Error ...)

-- Из Prelude
data Either a b  =  Left a | Right b
  deriving (Eq, Ord, Show)

type Error = String

-- Делаем так, чтобы lookup возвращал сообщение об ошибке, а не просто Nothing
nameToAddress :: Name -> Either Error Address
nameToAddress name =
  case lookup name addressDB of
  Just address -> Right address
  Nothing -> Left "Address not found"

-- Делаем так, чтобы lookup возвращал сообщение об ошибке, а не просто Nothing
addressToPhone :: Address -> Either Error Phone
addressToPhone address =
  case lookup address phoneDB of
  Just phone -> Right phone
  Nothing -> Left "Phone not found"

-- Композиция (addressToPhone . nameToAddress)
-- Возвращается сообщение о первой возникшей ошибке
nameToPhone2 :: Name -> Either Error Phone
nameToPhone2 name =
  case nameToAddress name of
  Right address -> addressToPhone address
  Left error -> Left error

-- Примеры
-- nameToPhone2 "Smith" (первый из двух телефонов)
-- nameToPhone2 "Jones" (нет телефона)
-- nameToPhone2 "Brown" (нет адреса)

-- Вынесем детали композиции, имеющие дело с Either, в отдельную функцию

thenEither :: Either e a -> (a -> Either e b) -> Either e b
thenEither (Left e) _ = Left e
thenEither (Right x) f = f x

nameToPhone2' :: Name -> Either Error Phone
nameToPhone2' name =
  nameToAddress name `thenEither` (\address -> addressToPhone address)

-- ******************************************
-- Вариант 3 (возвращается список всех телефонов, ассоциированых с именем)

lookupList :: Eq a => a -> [(a, b)] -> [b]
lookupList key xys = [snd xy | xy <- xys, fst xy == key]

-- Композиция двух lookupList

nameToPhone3 :: Name -> [Phone]
nameToPhone3 name =
  let addresses = (lookupList name addressDB)
      phoneLists = map (`lookupList` phoneDB) addresses
  in concat phoneLists

-- concatMap f = concat . map f

thenList :: [a] -> (a -> [b]) -> [b]
thenList xs f = concatMap f xs

nameToPhone3' :: Name -> [Phone]
nameToPhone3' name =
  lookupList name addressDB `thenList` (\address -> lookupList address phoneDB)

-- Примеры
-- nameToPhone3 "Smith" (все телефоны)
-- nameToPhone3 "Jones" (нет телефона)
-- nameToPhone3 "Brown" (нет адреса)

-- Еще раз типы разных версий then, а также функции, использующие их

-- thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- thenEither :: Either e a -> (a -> Either e b) -> Either e b
-- thenList :: [a] -> (a -> [b]) -> [b]

-- nameToPhone1' name =
--   lookup name addressDB `thenMaybe` (\address -> lookup address phoneDB)
--  
-- nameToPhone2' name =
--   nameToAddress name `thenEither` (\address -> addressToPhone address)
--  
-- nameToPhone3' name =
--   lookupList name addressDB `thenList`
--     (\address -> lookupList address phoneDB)

-- Что общего между типами thenMaybe, thenEither и thenList? В каждом
-- используется конструктор типов: Maybe, Either Error и [ ],
-- соответственно. Сами по себе это не типы, например, нельзя объявить
-- переменную x :: Maybe. Это конструкторы типов, потому что они
-- принимают и возвращают тип. Так, для любого типа a выражения Maybe
-- a, Either Error a и [a] являются типами. В этом смысле это функции
-- из типов в типы. (На языке теории категорий такие функции
-- называются функторами.)

-- Итак, если Maybe, Either Error или [ ] обозначить через m, то типы
-- thenMaybe, thenEither и thenList имеют вид then :: m a -> (a -> m
-- b) -> m b. Поэтому then v f похоже на применение f к v, но v
-- является не просто значением, а значением в контексте. В книге
-- Миран Липовача "Изучай Haskell во имя добра!" такие значения
-- называются "fancy values" ("причудливыми значениями"). Так, вместо
-- простого значения 1 аргумент v может быть Just 1 или
-- Nothing. Функция then должна сначала распаковать значение в
-- контексте, достать из него обычное значение и применить к нему f, а
-- также что-то сделать с контекстом (возможно, изменить его).

-- В Haskell можно объявлять не только классы типов, но и классы
-- конструкторов типов. Объявим класс конструкторов, поддерживающих
-- операцию then. Традиционно она обозначается >>= и используется как
-- инфиксная связка. Конструктор типов, который поддерживает операцию
-- >>= с этип типом называется монадой.

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  (>>=) = thenMaybe

instance Monad (Either e) where
  (>>=) = thenEither

instance Monad [] where
  (>>=) = thenList

-- Теперь можно использовать одну и ту же запись >>=, а Haskell
-- выберет нужную операцию на основании типа левой части.

nameToPhone1'' name =
  lookup name addressDB >>= (\address -> lookup address phoneDB)

nameToPhone2'' :: Name -> Either Error Phone
nameToPhone2'' name =
  nameToAddress name >>= (\address -> addressToPhone address)

nameToPhone3'' name =
  lookupList name addressDB >>= (\address -> lookupList address phoneDB)

-- Пример применения списочной монады

-- Возвращает список вершин ориентированного графа, достижимых из данной
-- вершины за три шага. В возвращаемом списке возможны повторения,
-- если некоторые вершины достижимы более, чем одним способом.
-- Первый аргумент -- граф, заданный списком ребер.
-- Второй аргумент -- начальная вершина.

threeSteps :: [(Int, Int)] -> Int -> [Int]
threeSteps graph v =
  step v  >>= \v1 ->
  step v1 >>= \v2 ->
  step v2
    where step u = lookupList u graph

-- Возвращает список вершин ориентированного графа, достижимых из данной
-- вершины за заданное количество шагов.
-- Первый аргумент -- граф, заданный списком ребер.
-- Второй аргумент -- начальная вершина.
-- Третий аргумент -- количество шагов.

nSteps :: [(Int, Int)] -> Int -> Int -> [Int]
nSteps _ v 0 = [v]
nSteps graph v n = lookupList v graph >>= \u -> nSteps graph u (n-1) 

-- Задание. Посторайтесь его сделать, но не огорчайтесь,
-- если оно окажется сложным для понимания.

-- Рассмотрим следующую монаду. (См. лекцию 6 про newtype.)

newtype Tick a = T (a, Int)

-- Применение функции f :: a -> Tick b к аргументу v :: (Tick a)
-- заключается в следующем.
-- (1) Пара v разбивается на (x :: a) и (n :: Int).
-- (2) f применяется к x. Получается пара (y, m).
-- (3) Возвращается пара (y, m + n + 1).
-- Объявите Tick членом класса Monad с таким определением (>>=).

-- instance Monad Tick where
-- begin
instance Monad Tick where
  T (x, m) >>= f = let T (y, n) = f x in T (y, m + n + 1)
-- end

-- Рассмотрим следующие определения

unit :: a -> Tick a
unit x = T (x, 0)

run :: Tick a -> a
run (T (x, _)) = x

ticks :: Tick a -> Int
ticks (T (_, t)) = t

fibM :: Int -> Tick Int
fibM 0 = unit 0
fibM 1 = unit 1
fibM n = fibM (n-1) >>= (\p1 -> fibM (n-2) >>= (\p2 -> unit (p1 + p2)))

fib :: Int -> Int
fib = run . fibM

fibTicks :: Int -> Int
fibTicks = ticks . fibM

-- Объясните, что возвращает функция fibTicks
