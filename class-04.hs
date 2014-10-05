{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}

{--a) Найти сумму чётных элементов списка с целочисленными элементами.--}
f1a :: [Int] -> Int
f1a = foldr (\x y -> if even x then x + y else y) 0
f1a_test1 = f1a [7,8,9,10] == 18
f1a_test2 = f1a [] == 0
f1a_test3 = f1a [-2,-7,-8,7] == -10

{--b) Найти сумму и произведение элементов списка вещественных чисел.--}
f1b :: [Double] -> (Double, Double)
f1b = foldr (\x (y0,y1) -> (y0 + x, y1 * x)) (0,1)
f1b_test1 = f1b [-1.0,-2.3,1] == (-2.3,2.3)
f1b_test2 = f1b [-9,2,-8,0] == (-15.0,0)
f1b_test3 = f1b [-6,0.5,10] == (4.5,-30)

{--  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход). --}
     
f1c :: [Double] -> Double
f1c = (\(x,y) -> x / y) . foldr (\x (y0,y1) -> (y0 + x, y1 + 1)) (0,0)
f1c_test1 = f1c [0..10] == 5.0
f1c_test2 = f1c [0.5,0.6..1] == 0.75
f1c_test3 = f1c [20000,0] == 10000.0

{--d) Найти минимальный элемент списка.--}
f1d :: Ord a => [a] -> a
f1d = foldr1(min)
f1d_test1 = f1d "ummagumma" == 'a'
f1d_test2 = f1d [10,9.5..1.0] == 1.0
f1d_test3 = f1d ["strawberry","fields","forever"] == "fields"

{--e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию). --}
{--
f1e = foldr(\x y -> if odd x then min x y else y) - нельзя, т.к. значение по умлчанию может выдаваться, даже если нечётные элементы есть. Сравнение с значением по умолчанию тоже не выход.  --}
f1e :: [Int] -> Int -> Int
f1e xs d = fst $ foldr(\x (y,found) -> if odd x then (if found then (min x y,found) else (x,True)) else (y,found)) (d,False) xs
f1e_test1 = f1e [0..8] (-200) == 1
f1e_test2 = f1e [0,2..10] (-200) == (-200)
f1e_test3 = f1e [11,9..(-7)] 0 == (-7)

{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

{--a) Сформировать список, содержащий каждый второй элемент исходного.--}
f2a :: [a] -> [a]
f2a = fst . foldl(\(ys,b) x -> if b then (ys ++ [x], not b) else (ys, not b)) ([],False)
f2a_test1 = f2a [1..10] == [2,4,6,8,10]
f2a_test2 = f2a "kashmir" == "ahi"
f2a_test3 = f2a ["foo",[],"bar"] == [[]]

{--b) Сформировать список, содержащий первые n элементов исходного.--}
f2b :: Int -> [a] -> [a]
f2b n = fst . foldl(\(xs,k) y -> if (k > 0) then (xs ++ [y], k - 1) else (xs,k)) ([],n)
f2b_test1 = f2b 5 [1..10] == [1,2,3,4,5]
f2b_test2 = f2b 60 "angie" == "angie"
f2b_test3 = f2b (-5) "nothingman" == ""

{--c) Сформировать список, содержащий последние n элементов исходного.--}
f2c :: Int -> [a] -> [a]
f2c n = fst . foldr(\x (ys,k) -> if (k > 0) then ((x:ys), k - 1) else (ys,k)) ([],n)
f2c_test1 = f2c 5 [1..10] == [6..10]
f2c_test2 = f2c 60 "angie" == "angie"
f2c_test3 = f2c (-5) "nothingman" == ""

{--  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.--}
f2d :: (Ord a) => [a] -> [a]
f2d [] = []
f2d (x:xs) = fst $ foldl(\(xs,x0) y -> if (y > x0) then (xs ++ [y], y) else (xs,y)) ([],x) xs
f2d_test1 = f2d [3,6,1,0,8,1] == [6,8]
f2d_test2 = f2d "lizard" == "zr"
f2d_test3 = f2d "zone" == ""

{--  e) Сформировать список, содержащий все локальные минимумы исходного списка. --}

f2e :: (Ord a) => [a] -> [a]
f2e [] = []
f2e (_:[]) = []
f2e (x:y:[]) = if (x < y) then [y] else if (y < x) then [x] else []
f2e (x0:x1:xs) = (\(x,(y,z)) -> if (z > y) then x ++ [z] else x) $ foldl (\(res, (pr, cur)) next ->  if (cur > pr) && (cur > next) then  ((res ++ [cur]), (cur, next)) else (res, (cur, next))) ((if (x0 > x1) then [x0] else []), (x0, x1)) xs
{--								^ обработка последнего элемента																																	^ обработка первого элемента--}																																				
f2e_test1 = f2e [1,3,1,6,7,8,1,8] == [3,8,8]
f2e_test2 = f2e "SOS" == "SS"
f2e_test3 = f2e [1,5] == [5]

{--f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.  --}
f2f :: [Char] -> [[Char]]
f2f xs = fst $ foldr(\ch (words, word) -> if (ch == ' ') then (if (word == "") then (words,word) else ((word:words),"")) else (words,ch:word)) ([],"") (' ':xs)
f2f_test1 = f2f "When The   Music's Over" == ["When","The","Music's", "Over"]
f2f_test2 = f2f " Lark's Tounges In Aspic" == ["Lark's","Tounges","In","Aspic"]
f2f_test3 = f2f "Set The Controls For The Heart Of The Sun" == ["Set","The","Controls","For","The","Heart","Of","The","Sun"]

{--g) Разбить список на непересекающиеся подсписки длиной n элементов. --}
f2g :: Int -> [a] -> [[a]]
f2g n = (\(x, (y0,y1)) -> x ++ [y0]) . foldl(\(sublists, (sublist, k)) elem -> if (k == 0) then (sublists ++ [sublist], ([elem], n-1)) else (sublists, (sublist ++ [elem], k-1))) ([],([],n))
f2g_test1 = f2g 5 [1..15] == [[1..5],[6..10],[11..15]]
f2g_test2 = f2g 1 [1..5] == [[1],[2],[3],[4],[5]]
f2g_test3 = f2g 3 "paint it black" == ["pai","nt ","it ","bla","ck"]

{--h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).--}
f2h :: Int -> Int -> [a] -> [[a]]
f2h n k = (\(x, (y0,y1)) -> x ++ [y0]) . foldl(\(sublists, (sublist, p)) elem -> if (p == 0) then (sublists ++ [sublist],((f2c k sublist) ++ [elem], n-k-1)) else (sublists, (sublist ++ [elem], p-1))) ([],([],n))
f2h_test1 = f2h 4 2 "Coming Back To Life" == ["Comi","ming","ng B"," Bac","ack ","k To","To L"," Lif","ife"]
f2h_test2 = f2h 3 1 [1..5] == [[1..3],[3..5]]
f2h_test3 = f2h 4 0 [1..9] == [[1..4],[5..8],[9]]

{--k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.--}
f2k :: (a -> Bool) -> [a] -> [a]
f2k pred = fst . foldl (\(x0, x1) y -> if x1 && pred y then (x0 ++ [y], True) else (x0, False)) ([],True)
f2k_test1 = f2k odd [1..20] == [1]
f2k_test2 = f2k (\x -> x < 8) [1..20] == [1,2,3,4,5,6,7]
f2k_test3 = f2k (\x -> x `mod` 3 == 1) [1,4,7,9,70,100] == [1,4,7]

{--l) Повторить каждый элемент списка заданное количество раз.--}
f2l :: Int -> [a] -> [a]
f2l n = foldr(\x y -> replicate n x ++ y) [] 
f2l_test1 = f2l 3 [1..5] == [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]
f2l_test2 = f2l 2 "spring" == "sspprriinngg"
f2l_test3 = f2l 5 "owl" == "ooooowwwwwlllll"

{--  m) Удалить из списка повторяющиеся подряд идущие элементы. --}

f2m :: Eq a => [a] -> [a]
f2m [] = []
f2m (x:xs) = (\(x,y) -> (x ++ [y])) $ foldl(\(lst,prev) y -> if y /= prev then ((lst++[prev]),y) else (lst,prev)) ([],x) xs
f2m_test1 = f2m "moonshake" == "monshake"
f2m_test2 = f2m "doom and gloom" == "dom and glom"
f2m_test3 = f2m [4,4,4,4,6,9,7,7,8] == [4,6,9,7,8]

{--  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков. --}
{--zip' xs ys = fst $ foldl (\(res, (k:ks)) y -> ((k,y):res, ks)) ([],xs) ys --}
f2n :: (a -> a -> b) -> [a] -> [a] -> [b]
f2n f xs ys = foldl (\ys (x0, x1) -> (f x0 x1):ys) [] $ fst $ foldl (\(res, (k:ks)) y -> ((k,y):res, ks)) ([],xs) ys
f2n_test1 = f2n (+) [1..3] [2..4] == [3,5,7]
f2n_test2 = f2n (*) [1..3] [2..4] == [2,6,12]
f2n_test3 = f2n (\x y -> y) "Bel" "Air" == "Air"

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

{--  a) Найти сумму чисел от a до b. --}
f3a a b = foldl (+) 0 [a..b]
f3a_test1 = f3a 1 7 == 28
f3a_test2 = f3a (-3) 3 == 0
f3a_test3 = f3a 10 12 == 33

{--  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются). --}
f3b :: Int -> Int -> Int
f3b a b = fst $ foldl (\(s,prevf) y -> (s + prevf + y, prevf + y)) (0, f3a 1 (a-1)) [a..b]
f3b_test1 = f3b 1 2 == 4
{--Дальше мне лень самой считать--}
f3b_test2 = f3b 5 10 == 200
f3b_test3 = f3b 100 200 == 1186750

{--  с) Сформировать список из первых n чисел Фибоначчи.--}

f3c :: Int -> [Int]
f3c n = fst $ foldl (\(res,(x, y)) k -> (res ++ [x],(y, x + y))) ([], (0,1)) [1..n]
f3c_test1 = f3c 5 == [0,1,1,2,3]
f3c_test2 = f3c 10 == [0,1,1,2,3,5,8,13,21,34]
f3c_test3 = f3c 20 == [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

{--  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых). --}


f3d x n = fst $ foldl (\(tsum, (sign, (power, fact))) k -> newelem tsum sign power fact k) (0, (1, (x, 1))) [1,3..(n*2 - 1)] 
	where
		newelem tsum sign power fact k = 
			let 
				newsign  = sign*(-1) 
				newpow   = power * x * x
				newfact  = fact * (k+1) * (k + 2)
			in (tsum + sign * power / fact, (newsign, (newpow, newfact)))

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}


{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}
