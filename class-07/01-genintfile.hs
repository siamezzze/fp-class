{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}

import System.Random
import System.Environment

rand_int :: StdGen -> (Int, StdGen)
rand_int gen = randomR (2, 20) gen :: (Int, StdGen)



rand_ints :: ([Char], (Int, (Int, (Int, StdGen)))) -> ([Char], (Int, (Int, (Int, StdGen))))
rand_ints (_, (minv, (maxv, (nmbints, gen)))) = 
	let line = unwords $ map show $ take nmbints $ (randomRs (minv, maxv) gen :: [Int] )
	    newGen = snd (random gen :: (Int, StdGen)) in
	(line, (minv, (maxv, (nmbints, newGen))))

rand_file :: FilePath -> Int -> Int -> Int -> Int -> IO ()
rand_file fname minvalue maxvalue nmbints nmblines = do
  gen <- newStdGen
  let strr = tail $ take (nmblines + 1) $ map (fst) $ iterate (rand_ints) ([], (minvalue, (maxvalue, (nmbints, gen))))
  writeFile fname (unlines strr)

main = do 
  args <- getArgs
  process args

process ([fname,minvalue,maxvalue,nmbints,nmblines]) = rand_file fname (read minvalue) (read maxvalue) (read nmbints) (read nmblines)
process _ = do
  putStrLn "Использование:"
  putStrLn "<имя файла> <минимальное значение> <максимальное значение> <чисел в строке> <строк в файле>"
