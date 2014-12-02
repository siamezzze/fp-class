{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict
import System.Environment

import Data.Char

data Config = Config Int Bool Bool Bool

isValid :: Config -> String -> Bool
isValid (Config n hasLetters hasDigits hasPunctuation) s = length s >= n && 
                (if hasLetters then any isAlpha s else True) && 
                (if hasDigits then any isNumber s else True) && 
                (if hasPunctuation then any isPunctuation s else True)

getValidPassword :: MaybeT (WriterT [String] (ReaderT Config IO)) String
getValidPassword = do
  lift $ lift $ lift $ putStrLn "Введите новый пароль:"
  s <- lift $ lift $ lift $ getLine
  conf <- lift $ lift $ ask
  lift $ tell [s]
  guard (isValid conf s)
  return s
 
askPassword :: MaybeT (WriterT [String] (ReaderT Config IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ lift $ lift $ putStrLn "Сохранение в базе данных..."

getConfig :: [String] -> Config
getConfig [a1, a2, a3, a4] = Config (read a1) (read a2) (read a3) (read a4)  

main = do
  args <- getArgs
  runReaderT (runWriterT (runMaybeT askPassword)) (getConfig args)
