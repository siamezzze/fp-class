module AbstractIntSet where

class AbstractIntSet a where
  empty :: a
  isEmpty :: a -> Bool
  add :: a -> Int -> a
  contains :: a -> Int -> Bool
