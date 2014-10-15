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

main = undefined
