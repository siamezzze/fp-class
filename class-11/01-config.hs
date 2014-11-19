import Control.Monad
import Control.Monad.Reader
import System.Environment

data OpType = Sum | Mult | Div
  deriving (Show, Enum)
data Action = Action OpType Integer
data Config = Config [Action]

doOp :: Action -> Integer -> Integer
doOp (Action Sum n) = ( + n)
doOp (Action Mult n) = ( * n)
doOp (Action Div n) = ( div n)

work :: Integer -> Reader Config String
work a = do
  Config actions <- ask
  return $ show $ foldl (flip doOp) a actions

readOp :: String -> OpType
readOp "summand" = Sum
readOp "multiplier" = Mult
readOp "divisor" = Div

readConfigLine :: String -> Action
readConfigLine st = Action (readOp ot) (read n) where [ot, n] = words st 

loadConfig :: FilePath -> IO Config
loadConfig fname = readFile fname >>= return . Config . map (readConfigLine) . lines

main = do
  [configfname, numbersfname] <- getArgs
  cfg <- loadConfig configfname
  numbers <- (map (read) . words) `liftM` (readFile numbersfname)
  mapM_ (putStrLn . runReader . work) numbers
  
