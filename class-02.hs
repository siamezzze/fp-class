-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms s = (s `div` 3600, s `mod` 3600 `div` 60 , s `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec(h,m,s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)
--Похоже, уже есть стандартый length, для списков
length' :: Point -> Point -> Double
length' (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
  where
    p = x + y + z
    s = sqrt (p * (p - x) * (p - y) * (p - z))
    x = length' a b
    y = length' b c
    z = length' c a
-- Не получается у меня писать красиво и без скобок :(

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if even x then 1 + nEven xs else nEven xs 

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = (x*2) : doubleElems xs


-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if odd x then x : fltOdd xs else fltOdd xs 


-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x + y) : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.

takeNdec :: Integer -> [Integer]
takeNdec 0 = []
takeNdec n = n : takeNdec (n - 1)

takeNinc :: Integer -> [Integer]
takeNinc 0 = []
takeNinc n = takeNinc (n - 1) ++ [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.

insertBetween :: a -> [a] -> [a]
insertBetween x [] = []
insertBetween x [y] = [y]
insertBetween x (y:ys) = [y] ++ [x] ++ insertBetween x ys

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).



sameAsFirst :: Eq a => [a] -> ([a], [a])
sameAsFirst [] = ([],[])
sameAsFirst (x:xs) = sameAs [x] xs
  where
    sameAs xs [] = (xs,[])
    sameAs xs (y:ys) = if (x == y) then sameAs (y:xs) ys else (xs, y:ys) 

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a

elementAt :: [a] -> Int -> a
elementAt [] n = error "No such element"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs $ n - 1

-- б) Eq a => [a] -> a -> Bool

startsWith :: Eq a => [a] -> a -> Bool
startsWith [] _ = False
startsWith (x:xs) y = (x == y)

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = if (x == y) then True else contains xs y

-- в) [a] -> Int -> [a]

takeFirstN :: [a] -> Int -> [a]
takeFirstN [] _ = []
takeFirstN xs 0 = []
takeFirstN (x:xs) n = x : takeFirstN xs (n - 1)

-- г) a -> Int -> [a]

repeat' :: a -> Int -> [a]
repeat' _ 0 = []
repeat' x n = x : repeat' x (n - 1)

-- д) [a] -> [a] -> [a]

concatenate :: [a] -> [a] -> [a]
concatenate = (++)

-- е) Eq a => [a] -> [[a]]

groupEqual :: Eq a => [a] -> [[a]]
groupEqual [] = []
groupEqual xs = groupEqual' xs []
  where 
    groupEqual' [] ys = ys
    groupEqual' (x:xs) ys = groupEqual' xs $insertOrAdd x ys
    insertOrAdd e [] = [[e]]
    insertOrAdd e ((f:fs):ks) = if (e == f) then ((e:(f:fs)):ks) else ((f:fs):insertOrAdd e ks)

-- ж) [a] -> [(Int, a)]

enumerate :: [a] -> [(Int, a)]
enumerate xs = enumerate' xs 0
  where
    enumerate' [] _ = []
    enumerate' (x:xs) n = ((n,x):enumerate' xs (n + 1))
    
-- з) Eq a => [a] -> [a]
representatives :: Eq a => [a] -> [a]
representatives xs = representatives' xs []
  where
    representatives' [] ys = ys
    representatives' (x:xs) ys = if contains ys x then representatives' xs ys else representatives' xs (ys ++ [x])
