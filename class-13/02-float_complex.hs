import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = (*) <$> minus <*> ((+) <$> naturalF <*> rest)
  where
    minus = (char '-' >> return (-1 :: Float)) <|> return (1 :: Float)
    toFloat x = (fromIntegral x) :: Float
    naturalF :: Parser Float
    naturalF = liftM (toFloat) natural
    toFract :: Float -> Float
    toFract x = x * (10 :: Float) ** ((-1 :: Float) * toFloat ( ceiling ( logBase (10 :: Float) x)))
    fract = liftM (toFract) naturalF 
    rest = (char '.' >> fract) <|> return (0 :: Float)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = do
  re <- char '(' >> float
  im <- char ',' >> float
  char ')'
  return (re,im)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (token complex) (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (token (complex <|> floatC)) (symbol ";")
  where
    floatC :: Parser (Float, Float)
    floatC = liftM (\x -> (x, 0)) float
{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


