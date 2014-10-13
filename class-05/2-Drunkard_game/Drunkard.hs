{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Hearts
          | Diamonds
          | Clubs
          | Spades
          deriving(Eq, Show)

data Value = Ace
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
           deriving(Eq, Show, Ord)

data Card = Card Value Suit
           deriving(Eq, Show)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2)  = s1 == s2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) = if (v1 < v2) then LT else (if v1 > v2 then GT else EQ)

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round ((x:xs),(y:ys)) = if (x `beats` y == GT) then (xs ++ [x, y], ys) else (if (x `beats` y == LT) then (xs, ys ++ [x, y]) else game_round ((xs ++ [x]), (ys ++ [y])))
{--game_round' x:xs y:ys prev = if (x `beats` y == GT) then (xs ++ [x, y] ++ prev, ys) else (if (x `beats` y == LT) then (xs, ys ++ [x, y] ++ prev) else game_round' xs ys x:(y:prev) --}

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
            deriving(Eq, Show)

game :: ([Card], [Card]) -> (Winner, Int)
game cards = game' cards 0

game' :: ([Card], [Card]) -> Int -> (Winner, Int)
game' ([], ys) n = (Second, n)
game' (xs, []) n = (First, n)
game' (xs, ys) n = game' (game_round (xs, ys)) (n + 1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

game1 = game ([(Card Ten Hearts), (Card Ace Hearts), (Card Jack Spades), (Card Three Diamonds), (Card Ace Clubs), (Card Four Spades), (Card Queen Spades), 
               (Card Five Clubs), (Card King Spades), (Card Five Diamonds)],
              [(Card Two Spades), (Card Queen Clubs), (Card Queen Clubs), (Card Nine Clubs), (Card Four Diamonds), (Card Six Hearts), 
               (Card Six Clubs), (Card Ten Diamonds), (Card Eight Hearts),  (Card Jack Diamonds)])
{--(First,220)--}


game2 = game ([(Card King Spades), (Card Ace Diamonds), (Card Ace Hearts), (Card Three Hearts), (Card Ace Clubs), (Card Ten Hearts), (Card Jack Spades), (Card Queen Spades), 
               (Card Queen Clubs), (Card Six Diamonds)],
              [(Card Four Spades), (Card Two Spades), (Card Two Clubs), (Card Queen Clubs), (Card Ten Diamonds), (Card Four Diamonds), (Card Six Hearts), 
               (Card Six Clubs), (Card Nine Clubs), (Card Ten Diamonds)])
{--(First,94)--}

game3 = game ([(Card Ace Diamonds), (Card Ace Hearts), (Card Two Clubs), (Card Three Hearts), (Card Ace Clubs), (Card Queen Clubs), (Card Ten Hearts), (Card Jack Spades), (Card Queen Spades), 
               (Card Queen Clubs), (Card Six Diamonds)],
              [(Card Four Spades), (Card Three Hearts), (Card Two Spades), (Card Ten Diamonds), (Card Four Diamonds), (Card Six Hearts), 
               (Card Six Clubs), (Card Nine Clubs), (Card King Spades), (Card Ten Diamonds)])
{--(Second,81)--}

game4 = game ([(Card Ace Hearts), (Card Nine Clubs), (Card Two Clubs), (Card Three Hearts), (Card Three Hearts), (Card Ace Clubs), (Card Queen Clubs), (Card Ten Hearts), (Card Jack Spades), (Card Queen Spades), 
               (Card Queen Clubs)],
              [(Card Four Spades), (Card Ace Diamonds), (Card Two Spades), (Card Six Diamonds), (Card Ten Diamonds), (Card Four Diamonds), (Card Six Hearts), 
               (Card Six Clubs), (Card King Spades), (Card Ten Diamonds)])
{--(Second,171)--}

game5 = game ([(Card Ace Hearts), (Card Nine Clubs), (Card Two Clubs), (Card Three Hearts), (Card Queen Clubs), (Card Ace Diamonds), (Card Two Spades), (Card Six Diamonds), (Card Ten Diamonds), (Card Queen Spades), 
               (Card Queen Clubs)],
              [(Card Four Spades), (Card Three Hearts), (Card Ace Clubs), (Card Four Diamonds), (Card Ten Hearts), (Card Jack Spades), (Card Six Hearts), 
               (Card Six Clubs), (Card King Spades), (Card Ten Diamonds)])
{--(Second,183)--}

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
