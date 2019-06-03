import Control.Monad

-- Напоминание. монада -- это конструктор типа m, для которого определены
-- две функции:

-- return :: a -> ma
-- (>>=) :: m a -> (a -> m b) -> m b

-- http://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html
-- Haskell Hierarchical Libraries

-- https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14
-- Haskell Report 2010: do notation

-- https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10
-- Изменение в определении монад, начиная с GHC 7.10

-- Несколько лет назад порядок объявления конструктора типа монадой
-- изменился. Поскольку монада является так называемым аппликативным
-- функтором, класс Monad был сделан подклассом Applicative. Теперь
-- нужно объявить, что конструктор типа m является, во-первых,
-- функтором, во-вторых, аппликативным функтором, и только в-третьих,
-- монадой.

-- Вместо

-- instance Monad Foo where
--   return x = retDef
--   m >>= k = thenDef
--  
-- нужно писать
--
-- instance Functor Foo where
--     fmap  = liftM
--  
-- instance Applicative Foo where
--     pure  = retDef
--     (<*>) = ap
--  
-- instance Monad Foo where
--  m >>= k = thenDef

-- В определениях выше меняются только retDef и thenDef; всё остальное
-- печатается буквально.
    
-- Напоминание.

-- data  Maybe a  =  Nothing | Just a
--   deriving (Eq, Ord)
--  
-- instance  Monad Maybe  where
--     (Just x) >>= k      = k x -- k :: a -> m b
--     Nothing  >>= _      = Nothing
--  
--     return              = Just
--  
-- data  Either a b  =  Left a | Right b
--   deriving (Eq, Ord, Read, Show, Typeable)
--  
-- instance Monad (Either e) where
--     return = Right
--     Left  l >>= _ = Left l
--     Right r >>= k = k r -- Здесь k :: a -> Either e b
--  
-- instance  Monad []  where
--     m >>= k             = concatMap k m -- k :: a -> [b]; m :: [a]
--     return x            = [x]

data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving Show

t1 = Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Leaf 5)

inOrder (Leaf x) = [x]
inOrder (Node x l r) = (inOrder l) ++ [x] ++ (inOrder r)

newtype Writer a = Writer ([String], a)
  deriving Show

returnWriter :: a -> Writer a
returnWriter x = Writer ([], x)

-- Пусть v = (l1 :: [String], x :: a). Чтобы применить k :: a -> Writer b
-- к v, нужно сделать следующее.
-- 1. Применить k к x. Получится пара (l2, y).
-- 2. Вернуть (l1 ++ l2, y).

thenWriter :: Writer a -> (a -> Writer b) -> Writer b
thenWriter (Writer (l1, x)) k =
  let Writer (l2, y) = k x in
    Writer (l1 ++ l2, y)

-- Определение Functor и Applicative. Этот фрагмент можно пропустить.
instance Functor Writer where
  fmap = liftM

instance Applicative Writer where
  pure = returnWriter
  (<*>) = ap
-- Конец фрагмента

instance Monad Writer where
  (>>=) = thenWriter
 
write :: String -> Writer ()
write s = Writer ([s], ())
 
-- С помощью монады функция ниже написана в стиле, похожем на императивный

inOrderM :: Show a => Tree a -> Writer ()
inOrderM (Leaf x) = write (show x)
inOrderM (Node x l r) =
  do inOrderM l
     write (show x)
     inOrderM r

