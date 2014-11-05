module BSTIntSet (IntSet, empty, add, contains, isEmpty) where

import AbstractIntSet

data BST = Null | Node (BST) Int (BST)

insert :: BST -> Int -> BST
insert Null x = Node Null x Null
insert (Node t1 m t2) x 
	| m == x = Node t1 m t2
	| m > x = Node (insert t1 x) m t2
	| m < x = Node t1 m (insert t2 x)

containsT :: BST -> Int -> Bool
containsT Null _ = False
containsT (Node t1 m t2) x 
	| m == x = True
	| m > x = containsT t1 x
	| m < x = containsT t2 x

newtype IntSet = IntSet BST

instance AbstractIntSet IntSet where
  empty = IntSet Null

  isEmpty (IntSet Null) = True
  isEmpty _ = False

  add (IntSet t) x = IntSet (insert t x)
	
  contains (IntSet t) x = containsT t x
