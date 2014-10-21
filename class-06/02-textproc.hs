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
import System.Environment

count_words :: String -> Int
count_words = length . words

count_words_in_file :: FilePath -> IO ()
count_words_in_file filename = do
  contents <- readFile filename
  putStrLn $ show $ count_words contents

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
  putStrLn $ map toUpper contents
  
concat_strings_in_file :: String -> String -> String
concat_strings_in_file a b = unlines $ zipWith (++) (lines a) (lines b)

concat_strings_in_file_IO :: FilePath -> FilePath -> IO ()
concat_strings_in_file_IO filename1 filename2 = do
  contents1 <- readFile filename1
  contents2 <- readFile filename2
  putStrLn $ concat_strings_in_file contents1 contents2

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

main = do 
  args <- getArgs
  process args
  
process ("1":fname:[]) = count_words_in_file fname
process ("2":fname:str:"front":[]) = append_start fname str
process ("2":fname:str:"back":[]) = appendFile fname str
process ("3":fname:[]) = out_upper fname
process ("4":fname1:fname2:[]) = concat_strings_in_file_IO fname1 fname2
process ("5":fname:[]) = rand_file fname
process _ = do
  putStrLn "Использование:"
  putStrLn " 1 <имя файла> - подсчет количества строк в файле"
  putStrLn " 2 <имя файла> <строка> front - добавить строку в начало файла"
  putStrLn " 2 <имя файла> <строка> back - добавить строку в конец файла"
  putStrLn " 3 <имя файла> - вывести содержимое файла со всеми буквами, приведенными к верхнему регистру"
  putStrLn " 4 <имя файла 1> <имя файла 2> - построчное слияние двух файлов"
  putStrLn " 5 <имя файла> - генерация случайного текстового файла"
