import AbstractQueue
import System.Random
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ

randints :: StdGen -> Int -> [Int]
randints gen n = take n $ randomRs ((-500),500) gen 

insertRemove :: (AbstractQueue q) => [Int] -> Int -> q Int -> q Int
insertRemove ints 0 q = q
insertRemove ints k q = remove insert where
  insert = foldl enqueue q (take (k + 1) ints)
  remove qu = iterate (snd . dequeue) qu !! k

insertRemoveN :: (AbstractQueue q) => [Int] -> Int -> q Int -> q Int
insertRemoveN ints n q = foldl (\qu k -> insertRemove ints k qu) q [1..n]

equal :: (AbstractQueue tq1, AbstractQueue tq2) => tq1 Int -> tq2 Int -> Bool
equal q1 q2 = if (isEmpty q1) && (isEmpty q2) then True else
  if (isEmpty q1) || (isEmpty q2) then False else 
  (x1 == x2) && equal q1' q2' where
    (x1, q1') = dequeue q1
    (x2, q2') = dequeue q2

testQueue :: (AbstractQueue tq1, AbstractQueue tq2) => StdGen -> Int -> tq1 Int -> tq2 Int -> Bool
testQueue gen n q1 q2 = equal (insertRemoveN ints n q1) (insertRemoveN ints n q2) where
  ints = randints gen n

checkQueue :: (AbstractQueue q, Num a, Eq a) => q a -> Bool
checkQueue q = lastElem (enqueue q 5) == 5
 where
  lastElem q = let (x, q') = dequeue q in
               if isEmpty q' then x else lastElem q'

main = do
  print $
         checkQueue (enqueue empty 10 :: Q.Queue Int)
         &&  checkQueue (enqueue empty 10 :: FQ.Queue Int)
         &&  checkQueue (enqueue empty 10 :: SQ.Queue Int)
         
  gen <- getStdGen
  print $ 
     testQueue gen 50 (empty :: Q.Queue Int) (empty :: FQ.Queue Int)
     && testQueue gen 50 (empty :: Q.Queue Int) (empty :: SQ.Queue Int)
