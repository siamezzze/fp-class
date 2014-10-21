{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Random
import System.Environment
import Data.List

data Point = Point Double Double
           deriving(Eq, Show, Read)

gen_point :: StdGen -> (Point, StdGen)
gen_point gen =
  let (x, newGen) = randomR(-100,100) gen :: (Double, StdGen)
      (y, newGen') = randomR(-100,100) newGen :: (Double, StdGen) in 
  (Point x y, newGen') 
  
get_points :: Int -> StdGen -> [Point]
get_points 0 _ = []
get_points n gen = 
  let (p, newGen) = gen_point gen in
  (p:(get_points (n-1) newGen))

gen_points_file :: FilePath -> IO ()
gen_points_file fname = do
  gen <- newStdGen
  let (nlines, newGen) = randomR(0,100) gen :: (Int, StdGen)
  let pts = get_points nlines newGen
  let pts_lines = map show pts
  writeFile fname (unlines pts_lines) 

read_point :: [String] -> Point
read_point ["Point", x, y] = Point (read x) (read y)
  
get_points_from_text :: String -> [Point]
get_points_from_text textstr = map (read_point . words) $ lines textstr

points_from_file fname = do
  contents <- readFile fname
  return $ get_points_from_text contents

point_in_quarter 1 (Point x y) = (x >= 0) && (y >= 0)
point_in_quarter 2 (Point x y) = (x < 0) && (y >= 0)
point_in_quarter 3 (Point x y) = (x < 0) && (y < 0)
point_in_quarter 4 (Point x y) = (x >= 0) && (y < 0)

points_in_quarter q iopts = do
  putStr "В четверти " 
  putStr (show q)
  putStr " точек: " 
  pts <- iopts
  putStrLn (show $ length $ filter (point_in_quarter q) pts)

dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))

compare_point :: Point -> Point -> Ordering
compare_point a b = 
  let dist_a = dist a (Point 0 0)
      dist_b = dist b (Point 0 0) in
  if (dist_a > dist_b) then GT
  else if (dist_a < dist_b) then LT
       else EQ

max_dist_point :: [Point] -> Point
max_dist_point = maximumBy compare_point

show_max_dist_point :: IO([Point]) -> IO()
show_max_dist_point iopts = do
  pts <- iopts
  putStr "Максимально удаленная от начала координат точка: "
  putStrLn $ show $ max_dist_point pts

points_in_quarters :: IO([Point]) -> IO()
points_in_quarters pts = do 
  points_in_quarter 1 pts
  points_in_quarter 2 pts
  points_in_quarter 3 pts
  points_in_quarter 4 pts

main = do
  args <- getArgs
  process args
  
process ["a",fname] = gen_points_file fname
process ["b",fname] = points_in_quarters $ points_from_file fname
process ["c",fname] = show_max_dist_point $ points_from_file fname
process _ = do
  putStrLn "Использование:"
  putStrLn " \"a\" <имя файла> - сгенерировать файл точек"
  putStrLn " \"b\" <имя файла> - по заданному файлу точек определить количество точек, лежащих в каждой из четвертей"
  putStrLn " \"c\" <имя файла> - по заданному файлу точек определить максимально удаленную от начала координат"
