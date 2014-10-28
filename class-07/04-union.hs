{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
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
    rset = foldl1 (Set.union) sets

main = getArgs >>= mapM readNumFile >>= print.solve
