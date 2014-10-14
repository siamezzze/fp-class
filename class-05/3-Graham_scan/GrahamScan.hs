{-# LANGUAGE EmptyDataDecls #-}
module GrahamScan where
import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.


data Point = Point Double Double
           deriving(Eq, Show, Ord)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = DirLeft | DirRight | OneLine
           deriving(Eq, Show, Ord)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

getX (Point x _) = x
getY (Point _ y) = y

direction3 :: Point -> Point -> Point -> Direction
direction3 a b c = if (sgn > 0) then DirLeft else (if (sgn < 0) then DirRight else OneLine) where sgn = (getX b - getX a)*(getY c - getY a) - (getY b - getY a)*(getX c - getX a)
                   

directions :: [Point] -> [Direction]
directions (a:(b:(c:xs))) = ((direction3 a b c):(directions (b:(c:xs))))
directions _ = []

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

compare_point :: Point -> Point -> Ordering
compare_point a b = if (getY a) < (getY b) then LT 
              else (if (getY a) > (getY b) then GT
                    else (if (getX a) < (getX b) then LT
                          else (if (getX a) > (getX b) then GT
                                else EQ))) 

min_point :: [Point] -> Point
min_point = minimumBy compare_point

angle_to start point = angle' x0 y0
    where x0 = getX point - getX start
          y0 = getY point - getY start
          angle' 0 0 = 0
          angle' 0 y = if (y > 0) then (pi / 2) else (3 * pi /2)
          angle' x y = if (x < 0) then (atan2 y x) + pi else (if (y < 0) then (atan2 y x) + 2 * pi else (atan2 y x))
          
sortpoints point = sortBy (\a b -> if (angle_to point a > angle_to point b) then GT else (if (angle_to point a < angle_to point b) then LT else EQ))

addpoint :: [Point] -> Point -> [Point]
addpoint (p1:(p0:points)) point = if (direction3 p0 p1 point /= DirRight) then (point:(p1:(p0:points))) else addpoint (p0:points) point 
addpoint points point = point:points

graham_scan :: [Point] -> [Point]
graham_scan points = foldl addpoint [] (sortpoints (min_point points) points)

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
example1 = graham_scan [(Point (-3) 2), (Point 0 0), (Point (-1) 5), (Point 0 3), (Point 3 2)] == [Point (-3.0) 2.0,Point (-1.0) 5.0,Point 3.0 2.0,Point 0.0 0.0]
example2 = graham_scan [(Point 0 0), (Point 5 0), (Point 1 (-3)), (Point 2 4), (Point 4 5), (Point (-2) (-1)), (Point (-4) 1), (Point 0 6), (Point (-3) 5)] == [Point (-2.0) (-1.0),Point (-4.0) 1.0,Point (-3.0) 5.0,Point 0.0 6.0,Point 4.0 5.0,Point 5.0 0.0,Point 1.0 (-3.0)]
example3 = graham_scan [(Point 0 0), (Point 3 (-1)), (Point (-2) 0), (Point (-1) (-2)), (Point 2 2), (Point 4 2), (Point 3 5), (Point (-1) 6)] == [Point (-2.0) 0.0,Point (-1.0) 6.0,Point 3.0 5.0,Point 4.0 2.0,Point 3.0 (-1.0),Point (-1.0) (-2.0)]
