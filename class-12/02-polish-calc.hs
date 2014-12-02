{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import Control.Monad
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

type Stack = [Int]

push :: Maybe Int -> MaybeT (State Stack) ()
push x = do
  xs <- lift $ get
  guard (isJust x) 
  lift $ put ((fromJust x):xs)

pop :: MaybeT (State Stack) Int
pop = do
  (x:xs) <- lift $ get
  lift $ put xs
  return x

evalRPN xs = head $ execState (mapM step $ words xs) []
  where
    step "+" = processTops (liftM2 (+))
    step "*" = processTops (liftM2 (*))
    step  n  = runMaybeT (push (Just (read n)))
    processTops op = op `liftM` (runMaybeT pop) `ap` (runMaybeT pop) >>= (\x -> runMaybeT (push x))