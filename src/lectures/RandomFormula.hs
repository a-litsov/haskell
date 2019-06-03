module RandomFormula where

import System.Random
import Control.Monad.State
import BooleanSyntax
import Formula

-- Генерирование случайных формул

-- Псевдослучайные числа

-- Функция properFraction возвращает пару, которая состоит из целой
-- и дробной частей числа.
-- Функция nextRandom генерирует последовательность псевдослучайных
-- чисел от 0 до 1.

-- Взято из: Антон Холомьёв. Учебник по Haskell. Раздел 7.1.
-- https://anton-k.github.io/ru-haskell-book/book/home.html
-- nextRandom = snd . properFraction . (105.947 *)

-- Взято из: Лекции по функциональному программированию.
-- http://pv.bstu.ru/?topic=flp

nextRandom :: Double -> Double
nextRandom s = snd (properFraction (11 * s + pi))

-- randB принимает начальное состояние (seed) и возвращает пару из
-- следующего состояния и псевдослучайного булева значения

randB :: Double -> (Double, Bool)
randB s = let s1 = nextRandom s in (s1, s1 < 0.5)

-- rf принимает глубину формулы и начальное состояние и возвращает
-- пару из нового состояния и формулы. В качестве примера используются
-- только переменные с индексом 0, а также конъюнкция и отрицание.
-- Состояние передается от одного вызова randB к следующему.

rf :: Int -> Double -> (Double, Formula)
rf 0 s = (s, V 0)
rf d s =
  let (s1, b) = randB s in
    if b
    then -- конъюнкция
      let (s2, f1) = rf (d-1) s1
          (s3, f2) = rf (d-1) s2 in
            (s3, C And [f1, f2])
    else -- отрицание
      let (s2, f) = rf (d-1) s1 in
        (s2, C Neg [f])

-- Для тестирования:
-- rf 3 1.0
-- Здесь 3 -- глубина формулы, 1.0 -- начальное значение генератора
-- случайных чисел.

-- Записи как частный случай алгебраических типов были введены
-- в lecture06.hs. Там же показана конструкция newtype.
-- См. также Холомьёв, «Учебник по Haskell», раздел 7.4.

-- Напоминание разницы между newtype и data:
-- 1) newtype допускает только один конструктор;
-- 2) функции r и R выше рассматриваются как NOOP (no operation)
--    вычислителем Haskell

newtype R a = R { r :: Double -> (Double, a) }

-- Это определение эквивалентно следующим.

-- data R a = R (Double -> (Double, a))
--  
-- r :: R a -> Double -> (Double, a)
-- r (R f) = f

-- Фактически типы R a и Double -> (Double, a) являются изоморфными,
-- а изоморфизм осуществляется взаимно обратными функциями r и R.

-- Версия randB, возвращающая R Bool вместо Double -> (Double, Bool)

randBool :: R Bool
randBool = R (\s -> let s1 = nextRandom s in (s1, s1 < 0.5))

-- Перепишем rf как функцию одного аргумента в стиле разностных списков
-- (аналогично функциям fullParen :: Formula -> ShowS из hw09.hs и
-- collectVars3 из hw10.hs).

-- Напоминание: идея разностных списков заключается в том, что вместо
-- функции f :: a -> [b] мы пишем f' :: [a] -> [b] -> [b]. Функция f
-- получает аргумент типа a и возвращает список-результат. Функция f'
-- получает не только аргумент типа a, но и суффикс списка и добавляет
-- список-результат к этому суффиксу.

-- Сопутствующая идея заключается в том, что мы рассматриваем f' не
-- как функцию двух аргументов, а как функцию одного аргумента типа a,
-- возвращающую функцию типа [b] -> [b].

-- Было: rf :: Int -> Double -> (Double, Formula)

rf1 :: Int -> R Formula
rf1 0 = R (\s -> (s, V 0))
rf1 d = R $ \s ->
  let (s1, b) = r randBool s in
    if b
    then let (s2, f1) = r (rf1 (d-1)) s1
             (s3, f2) = r (rf1 (d-1)) s2 in
           (s3, C And [f1, f2])
    else let (s2, f) = r (rf1 (d-1)) s1 in
           (s2, C Neg [f])

-- Функция rf1 возвращает тип R Formula, который изоморфен типу
-- Double -> (Double, Formula). Вместо того, чтобы вручную работать со
-- значениями этого типа, возвращаемыми рекурсивными вызовами, можно
-- определить на этом типе вспомогательную операцию, которая спрячет
-- передачу состояния генератора случайных чисел аналогично тому, как
-- в monads1.hs сопоставление с образцом было спрятано в функциях
-- thenMaybe и thenEither. Для этого объявим R монадой, определим
-- функцию thenR и будем использовать для нее стандартное обозначение
-- >>=.

-- Определение монады согласно инструкции в monads2.hs.
-- Следующий фрагмент можно пропустить при чтении.
instance Functor R where
  fmap = liftM

instance Applicative R where
  pure x = R $ \s -> (s, x)
  (<*>) = ap
-- Конец фрагмента

-- В дополнение к операции (>>=) :: m a -> (a -> m b) -> m b в монаде
-- нужно определить также операцию return :: a -> m a. Если монадное
-- значение рассматривать как значение в контексте (например, значение
-- плюс некоторая дополнительная информация), то функция return
-- превращает чистое значение в значение в простейшем контесте. В
-- рассмотренных монадах функция return определена следующим образом:

-- Maybe: return x = Just x
-- Either e: return x = Right x
-- []: return x = [x]

instance Monad R where
-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
  return x = R $ \s -> (s, x)
  x >>= k = R $ \s -> let (s1, x1) = r x s in r (k x1) s1

-- При чтенеии R и r рекомендуется игнорировать. Таким образом,
-- определения выше можно читать как
-- return x = \s -> (s, x)
-- x >>= k = \s -> let (s1, x1) = x s in k x1 s1

-- Поясним эти определения. Для любого типа t значения типа R t
-- являются значениями t в контексте. Контекст в данном случае
-- означает, что значение является полуфабрикатом: оно еще не готово,
-- но будет готово, если ему передать начальное состояние генератора
-- случайных чисел.  Таким образом, монадное значение типа R t есть
-- программа, которая получает начальное состояние генератора
-- случайных чисел и возвращает чистое значение типа t плюс новое
-- значение для генератора.

-- Функция return вкладывает чистое значение t в монадное. Поскольку
-- вызовов генератора случайных чисел при этом не происходит,
-- начальное состояние генератора возвращается неизменным.

-- Функция >>= описывает, как применить функцию k, ожидающую чистое
-- значение типа t, к монадному значению x типа R t. Нужно сделать
-- следующее.

-- 1. Получить начальное состояние s генератора случайных чисел.
-- 2. Дать s на вход полуфабрикату x и получить на выходе чистое
--    значение x1 и новое состояние s1 генератора.
-- 3. Дать чистое значение x1 на вход функции k. Результатом будет
--    полуфабрикат, ожидающий состояние генератора.
-- 4. Дать этому полуфабрикату на вход s1.

-- С помощью return и >>= функцию rf1 можно переписать, сохраняя
-- смысл, следующим образом.

rf2 :: Int -> R Formula
rf2 0 = return (V 0)
rf2 d = randBool >>=
        (\b ->
            if b
            then rf2 (d-1) >>= (\f1 ->
                 rf2 (d-1) >>= (\f2 ->
                 return (C And [f1, f2])))
            else rf2 (d-1) >>= (\f ->
                 return (C Neg [f])))

-- Здесь, например, rf2 (d-1) >>= (\f1 -> ...) означает, что монадное
-- значение, возвращаемое rf2 (d-1), нужно развернуть, достать из него
-- чистое значение и дать его на вход функции справа от >>=, то есть
-- подставить это чистое значение вместо переменной f1 и
-- тело функции.вычислить

-- Можно показать, что rf2 не только имеет тот же смысл, что и rf1, но
-- и редуцируется (переписывается) к ней, если подставить определения
-- функций return и >>= и воспользоваться редукциями.

-- Напоминание из lecture04.hs. Бета-редукция переписывает (\x -> M) N
-- к результату подстановки N в M вместо x (подстановка фактического
-- параметра вместо формального в тело функции). В данном случае
-- кроме бета-редукции необходимо использовать следующие правила
-- переписывания:
--   (if b then f else g) x заменяется на if b then f x else g x
--   f (if b then x else y) заменяется на if b then f x else f y

-- Haskell имеет синтаксический сахар для >>=. Именно,

-- do x <- e1
--    e2

-- означает

-- e1 >>= (\x -> e2)

-- С помощью этих обозначений rf2 можно переписать следующим образом,
-- что очень похоже на программу в императивном языке программирования.

rf3 :: Int -> R Formula
rf3 0 = return (V 0)
rf3 d =
  do b <- randBool
     if b
     then do f1 <- rf3 (d-1)
             f2 <- rf3 (d-1)
             return (C And [f1, f2])
     else do f <- rf3 (d-1)
             return (C Neg [f])

-- Для тестирования:
-- r (rf3 3) 1.0
-- Здесь 3 -- глубина формулы, 1.0 -- начальное состояние генератора
-- случайных чисел.

-- Как использовать функции Haskell вместо nextRandom

-- В начале файла нужно написать

-- import System.Random
-- import Control.Monad.State

-- Модуль System.Random предоставляет следующие вещи.

-- Тип StdGen. Является состоянием генератора случайных чисел
-- и заменяет собой Double.

-- Функция mkStdGen :: Int -> StdGen. Берет целое число и возвращает
-- соответствующее состояния генератора.

-- Класс типов RandomGen. Содержит типы, значения которых могут
-- являться состоянием генератора. Тип StdGen является членом этого
-- класса.

-- Класс Random. Содержит типы, значения которые могут
-- генерерироваться случайным образом. Содержит Int, Float, Double,
-- Char, и Bool.

-- Функция random :: (RandomGen g, Random a) => g -> (a, g).
-- Обычно используется при g = StdGen. Является заменой nextRandom.
-- При вызове Haskell должен понимать из контекста, каким типом дожен
-- быть a. Если это не ясно, тип нужно указать явно.

-- Некоторые другие функции:
-- randoms (бесконечный список случайных значений),
-- randomR (случайное значение из интервала),
-- randomRs (бесконечный список случайных значений из интервала).
-- Напоминание: можно взять первые n значений списка ls с помощью
-- take n ls.

-- Примеры (в интерпретатора тип нужно указывать):
-- random (mkStdGen 10) :: (Int, StdGen)
-- random (mkStdGen 10) :: (Bool, StdGen)

-- Модуль Control.Monad.State предоставляет конструктор типов
-- State s a c определением, эквивалентным следующему.

-- newtype State s a = State { runState :: s -> (a, s) }

-- Таким образом, State Double a изоморфен R a.
-- (Компоненты пары переставлены по сравнению с R.)

-- Конструктор State, как и R выше, объявлен монадой.
-- Таким образом, если v :: State StdGen a, то v является
-- полуфабрикатом типа a. Это значит, что v ожидает начальное
-- состояние герератора случайных чисел и возвращает пару из
-- случайного значения и нового состояния генератора.

-- Чтобы завернуть функцию типа StdGen -> (a, StdGen) в тип
-- State StdGen a, нужно использовать функцию state, а не
-- конструктор State. Обратную конверсию осуществляет функция
-- runState.

-- Поскольку функция random имеет тип g -> (a, g), дадим следующее
-- определение.

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- Функция randomSt аналогична randBool :: R Bool, только первая
-- возвращает значения любого типа из класса Random.

-- Переишем rf3 с помощью библиотечных функций

rf4 :: Int -> State StdGen Formula
rf4 0 = return (V 0)
rf4 d =
  do b <- randomSt
     if b
     then do f1 <- rf4 (d-1)
             f2 <- rf4 (d-1)
             return (C And [f1, f2])
     else do f <- rf4 (d-1)
             return (C Neg [f])

-- Функция для вызова rf4. Найдите типы всех подвыражений определения.
-- Первый аргумент: глубина формулы
-- Второй аргумент: начальное значение для генератора случайных чисел

randomFormulaExample :: Int -> Int -> Formula
randomFormulaExample depth seed =
  fst (runState (rf4 depth) (mkStdGen seed))

-- Дополнительную информацию по монадам и случайным числам в Haskell
-- см. в "Лекциях по функциональному программированию".
-- Описание модулей System.Random и Control.Monad.State см.
-- в "Изучай Haskell во имя добра!", гл. 9 и 14.
