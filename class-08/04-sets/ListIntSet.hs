module ListIntSet (IntSet, empty, add, contains, isEmpty) where

import AbstractIntSet

newtype IntSet = IntSet [Int]

instance AbstractIntSet IntSet where
  empty = IntSet []

  isEmpty (IntSet xs) = null xs

  add (IntSet xs) x = IntSet (xs ++ [x])
  
  contains (IntSet xs) x = elem x xs

