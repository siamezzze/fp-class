import Control.Monad
import Data.Maybe
{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))
  p >>= q = Parser (\s -> let
                     m1 = apply p s
                     in case m1 of Just (x,s') -> apply (q x) s'
                                   Nothing     -> Nothing )
  fail _ = Parser (\s -> Nothing)

instance MonadPlus Parser where
  mzero = Parser (\s -> Nothing)
  p `mplus` q = Parser (\s -> let
                     m1 = apply p s
                     in if isJust m1 then m1 else apply q s)
