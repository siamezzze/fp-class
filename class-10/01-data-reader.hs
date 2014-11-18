{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}

import System.Environment
import Data.List
import Control.Monad

data Student = Student {
  name :: String,
  age :: String,
  group :: String } deriving (Show, Read, Eq)

readStudent :: String -> Student
readStudent st = Student name age group where
  [name, age, group] = words st

readStudents :: String -> [Student]
readStudents = map (readStudent) . lines

studentsFromFile :: FilePath -> IO [Student]
studentsFromFile fname = (readFile fname) >>= return . readStudents

showStudent :: Student -> String
showStudent (Student name age group) = unwords [name, age, group]

showStudents :: [Student] -> String
showStudents = unlines . map showStudent

instance Ord Student where
  compare (Student n1 a1 g1) (Student n2 a2 g2) = compare n1 n2

studentsToFile :: FilePath -> [Student] -> IO()
studentsToFile fname students = return (showStudents students) >>= writeFile fname

combineStLists :: [Student] -> [Student] -> [Student]
combineStLists l1 l2 = sort (l1 ++ l2)

process [fname1, fname2, fnameout] = (liftM2 combineStLists (studentsFromFile fname1) (studentsFromFile fname2)) >>= (studentsToFile fnameout)

main = getArgs >>= process
