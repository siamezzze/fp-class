{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}
import System.Directory
import Data.Char
import System.Random

count_words :: String -> Int
count_words = length . words

count_words_in_file :: FilePath -> IO ()
count_words_in_file filename = do
  contents <- readFile filename
  putStr $ show $ count_words contents

append_start :: FilePath -> String -> IO()
append_start filename s = do
  contents <- readFile filename
  writeFile ".tmp" (s ++ contents)
  removeFile filename
  renameFile ".tmp" filename

{--appendFile :: FilePath -> String -> IO ()--}

out_upper :: FilePath -> IO ()
out_upper filename = do
  contents <- readFile filename
  putStr $ map toUpper contents
  
concat_strings_in_file :: String -> String -> String
concat_strings_in_file a b = unlines $ zipWith (++) (lines a) (lines b)

rand_int :: StdGen -> (Int, StdGen)
rand_int gen = randomR (2, 20) gen :: (Int, StdGen)



rand_chars :: ([Char], StdGen) -> ([Char], StdGen)
rand_chars (_,gen) = 
	let (nchars, newGen) = rand_int gen 
	    line = take nchars $ randomRs (' ', '~') newGen
	    newGen' = snd (random newGen :: (Int, StdGen)) in
	(line, newGen')

rand_file :: FilePath -> IO ()
rand_file fname = do
  gen <- newStdGen
  let (nlines, newGen) = rand_int gen
  let strr = tail $ take nlines $ map (fst) $ iterate (rand_chars) ([],newGen)
  writeFile fname (unlines strr)

main = undefined
