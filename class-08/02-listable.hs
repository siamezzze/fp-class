{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
  toList :: a -> [a]
  fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integral a - любое целое число разбивается на список цифр.
-}

instance Listable String where
  toList = words
  fromList = unwords

instance Listable (Integer) where
  toList 0 = [] 
  toList x = if (x > 0) then (toList (x `div` 10)) ++ [x `mod` 10] else (toList (-1 * x))
  
  fromList = fromList' . reverse  where 
	  fromList' [] = 0
	  fromList' (x:xs) = (fromList' xs) * 10 + x
