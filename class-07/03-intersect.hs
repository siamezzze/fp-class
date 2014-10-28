{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
  content <- readFile fname
  let xs = map read $ concatMap words $ lines content
  return xs

solve :: [[Int]] -> (Int, [Int])
solve ll = (Set.size rset,Set.toList rset)
  where
    sets = map Set.fromList ll
    rset = foldl1 (Set.intersection) sets

main = getArgs >>= mapM readNumFile >>= print.solve
