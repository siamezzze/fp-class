import AbstractIntSet
import System.Random
import qualified ListIntSet as LS
import qualified BSTIntSet as BSTS

testEmpty :: (AbstractIntSet a) => a -> Bool
testEmpty s = isEmpty s && not (isEmpty (add s 5))

testContains :: (AbstractIntSet a) => a -> Bool
testContains s = contains (add (add s 5 ) 7 ) 5 

randints :: StdGen -> [Int]
randints gen = randomRs ((-500),500) gen 

testRandInt :: (AbstractIntSet a) => StdGen -> a -> Bool
testRandInt gen s = all (contains s') ints where
  n = 50
  ints = take n $ randints gen
  s' = foldl add s ints

main = do
  print $ testEmpty (empty :: LS.IntSet)
  print $ testEmpty (empty :: BSTS.IntSet)
  
  print $ testContains (empty :: LS.IntSet)
  print $ testContains (empty :: BSTS.IntSet)
  
  gen <- getStdGen
  
  print $ testRandInt gen (empty :: LS.IntSet)
  print $ testRandInt gen (empty :: BSTS.IntSet)
  
