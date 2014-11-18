import Control.Monad 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole -> Either String Pole
updatePole (l,r) = if (l > r + balance) then Left "Too many birds at left side" else if (l < r - balance) then Left "Too many birds at right side" else Right (l,r)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Birds -> Pole -> Either String Pole
landBoth l r (left, right) = updatePole (left + l, right + r)

unlandAll :: Pole -> Either String Pole
unlandAll = const (Right (0,0))

banana :: Pole -> Either String Pole
banana = const (Left "Banana")

{-tests = all test [1..3]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Nothing
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Just (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Nothing-}

land :: [String] -> Pole -> Either String Pole
land ["L", k] = landLeft (read k)
land ["R", k] = landRight (read k)
land ["LR", l, r] = landBoth (read l) (read r)
land ["U"] = unlandAll
land ["B"] = banana

fromFile fname = (return (0,0) >>= )`liftM` (foldr (<=<) return) `liftM` (map (land. words)) `liftM` (reverse.lines) `liftM` (readFile fname)
