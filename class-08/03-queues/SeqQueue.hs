module SeqQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as Seq

newtype Queue a = Queue (Seq.Seq a)

instance AbstractQueue Queue where
  empty = Queue (Seq.empty :: Seq.Seq a)
  isEmpty (Queue s) = Seq.null s
  enqueue (Queue s) x = Queue (x Seq.<| s)
  dequeue (Queue (s)) = (x, (Queue s') ) where (s' Seq.:> x) = Seq.viewr s
