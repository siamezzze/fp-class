{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length . nub

nub_seq :: Eq a => Seq.Seq a -> Int
nub_seq = count_nub . Seq.viewl
  where
    count_nub :: Eq a => Seq.ViewL a -> Int
    count_nub Seq.EmptyL = 0
    count_nub (h Seq.:< t) = if ((Seq.elemIndexL h t) == Nothing) then (1 + (count_nub $ Seq.viewl t)) else (count_nub $ Seq.viewl t)

nub_arr :: Array Int Int -> Int
nub_arr arr = count_nub f_ind
  where
    (f_ind, e_ind) = bounds arr
    
    exists :: Int -> Int -> Bool
    exists elem ind = 
		if ((ind < f_ind) || (ind > e_ind)) then False else
		if ((arr ! ind) == elem) then True else exists elem (ind+1)
    
    count_nub :: Int -> Int
    count_nub ind = 
		if ((ind < f_ind) || (ind > e_ind)) then 0 else 
		if (exists (arr ! ind) (ind + 1)) then count_nub (ind+1)
		else 1 + (count_nub (ind+1))


main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  mapM_ print results
  when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
