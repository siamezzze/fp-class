import System.Environment
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
    | (a `mod` 3) == 0  = 0
    | odd a             = a*a
    | otherwise         = a*a*a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 fa = fa
reduceNF n fa = fmap reduce (reduceNF (n-1) fa)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList [] = []
toList ((a,b):xs) = ((max a b):(toList xs))

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Just 0 
toMaybe ((a,b):xs) = if (a == b) then fmap (+a) (toMaybe xs) else Nothing

toEither :: Integral a => [(a, a)]  -> Either String a
toEither [] = Right 0 
toEither ((a,b):xs) = if (a == b) then fmap (+a) (toEither xs) else Left $ "Not all pairs contains equal numbers"

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO xs = do
  gen <- newStdGen
  let n = length xs
  let rands = take n $ randomRs (0, 1) gen :: [Int]
  return $ sum (zipWith (\(a,b) s -> if (s == 0) then a else b) xs rands)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs [a,b] = (a, x) where x = read b 

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
  contents <- readFile fname
  let pairs = map (\[a,b] -> (read a, read b) ) $ map words $ lines contents
  return pairs 

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
  
*Main> :main test2.txt  2
Nothing
Left "Not all pairs contains equal numbers"
390625

*Main> :main test2.txt  2
Nothing
Left "Not all pairs contains equal numbers"
707281

*Main> :main test3.txt  4
Just 32768
Right 32768
32768

*Main> :main test3.txt  2
Just 35184372088832
Right 35184372088832
35184372088832

*Main> :main test3.txt  3
Just 0
Right 0
0


-}
