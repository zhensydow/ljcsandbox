In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

The file, problem054.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?

\begin{code}
import Data.List( group, groupBy, sort, sortBy, maximumBy, delete )
import Data.Function( on )
import Data.Ord( comparing )
import Data.List.Split( splitOn )
import Control.Arrow( (&&&), (***) )
\end{code}

\begin{code}
data Suit = Spades | Hearts | Diamonds | Clubs
            deriving( Eq, Ord )

instance Show Suit where
    show Spades = "S"
    show Hearts = "H"
    show Diamonds = "D"
    show Clubs = "C"

charToSuit :: Char -> Suit
charToSuit 'S' = Spades
charToSuit 'H' = Hearts
charToSuit 'D' = Diamonds
charToSuit 'C' = Clubs
charToSuit _ = error "Unknown suit"
\end{code}

\begin{code}
data Value = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 
           | Jack | Queen | King | Ace
             deriving( Enum, Eq, Ord )

instance Show Value where
    show V2 = "2"
    show V3 = "3"
    show V4 = "4"
    show V5 = "5"
    show V6 = "6"
    show V7 = "7"
    show V8 = "8"
    show V9 = "9"
    show V10 = "T"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

charToValue :: Char -> Value
charToValue '2' = V2
charToValue '3' = V3
charToValue '4' = V4
charToValue '5' = V5
charToValue '6' = V6
charToValue '7' = V7
charToValue '8' = V8
charToValue '9' = V9
charToValue 'T' = V10
charToValue 'J' = Jack
charToValue 'Q' = Queen
charToValue 'K' = King
charToValue 'A' = Ace
charToValue _ = error "Unknown value"
\end{code}

\begin{code}
data Card = Card { cardValue :: ! Value 
                 , cardSuit :: ! Suit }
                 deriving(Eq)

instance Show Card where
    show c = show (cardValue c) ++ show (cardSuit c)

stringToCard :: String -> Card
stringToCard (x:y:_) = Card (charToValue x) (charToSuit y)
stringToCard _ = error "Unknown card"
\end{code}

\begin{code}
type Hand = [Card]

stringToHand :: String -> Hand
stringToHand = map stringToCard . splitOn " "
\end{code}

\begin{code}
data HandRank = HighCard Value [Card]
              | OnePair Value [Card]
              | TwoPairs Value Value Value
              | Three Value [Card]
              | Straight Value 
              | Flush [Card]
              | FullHouse Value Value
              | Four Value Value
              | StraightFlush Value
              | RoyalFlush
              deriving( Show, Eq )

instance Ord HandRank where
    RoyalFlush > RoyalFlush = error "comparing hands"
    RoyalFlush > _ = True
    _ > RoyalFlush = False

    (StraightFlush v1) > (StraightFlush v2) = v1 > v2
    (StraightFlush _) > _ = True
    _ > (StraightFlush _) = False

    (Four v1 c1) > (Four v2 c2) = (v1 > v2) || ((v1 == v2) && (c1 > c2))
    (Four _ _) > _ = True
    _ > (Four _ _) = False

    (FullHouse v1 c1) > (FullHouse v2 c2) = (v1 > v2) || ((v1 == v2) && (c1 > c2))
    (FullHouse _ _) > _ = True
    _ > (FullHouse _ _) = False

    (Flush h1) > (Flush h2) = h1 `greatHand` h2
    (Flush _) > _ = True
    _ > (Flush _) = False

    (Straight v1) > (Straight v2) = v1 > v2
    (Straight _) > _ = True
    _ > (Straight _) = False

    (Three v1 h1) > (Three v2 h2) = (v1 > v2) || ((v1 == v2) && (h1 `greatHand` h2))
    (Three _ _) > _ = True
    _ > (Three _ _) = False

    (TwoPairs v1 u1 c1) > (TwoPairs v2 u2 c2) = (max v1 u1 > max v2 u2) 
                                                || ((max v1 u1 == max v2 u2) && (min v1 u1 > min v2 u2))
                                                || ((min v1 u1 == min v2 u2) && c1 > c2)
    (TwoPairs _ _ _) > _ = True
    _ > (TwoPairs _ _ _) = False

    (OnePair v1 h1) > (OnePair v2 h2) = (v1 > v2) || ((v1 == v2) && (h1 `greatHand` h2))
    (OnePair _ _) > _ = True
    _ > (OnePair _ _) = False

    (HighCard v1 h1) > (HighCard v2 h2) = (v1 > v2) || ((v1 == v2) && (h1 `greatHand` h2))

greatHand :: Hand -> Hand -> Bool
[] `greatHand` [] = error "comparing hands"
xs `greatHand` ys = (xVal > yVal) || ((xVal == yVal) && (delete xCard xs `greatHand` delete yCard ys))
    where
      xVal = highestValue xs
      yVal = highestValue ys
      xCard = highestCard xs
      yCard = highestCard ys
\end{code}

\begin{code}
sameSuit :: Hand -> Bool
sameSuit = (==1) . length . group . sort . map cardSuit
\end{code}

\begin{code}
lowestValue :: Hand -> Value
lowestValue = minimum . map cardValue
\end{code}

\begin{code}
highestValue :: Hand -> Value
highestValue = maximum . map cardValue
\end{code}

\begin{code}
highestCard :: Hand -> Card
highestCard = maximumBy (comparing cardValue)
\end{code}

\begin{code}
consecutiveValues :: Hand -> Bool
consecutiveValues xs = [minValue..maxValue] == (sort . map cardValue) xs
    where
      minValue = lowestValue xs
      maxValue = highestValue xs
\end{code}

\begin{code}
groupValues :: Hand -> [[Card]]
groupValues = groupBy ((==) `on` cardValue) . sortBy (comparing cardValue)
\end{code}

\begin{code}
calculateRank :: Hand -> HandRank
calculateRank xs
    | isConsecutive && hasSameSuit && (maxValue == Ace) = RoyalFlush
    | isConsecutive && hasSameSuit = StraightFlush maxValue
    | length bigGroup == 4 = Four bigGroupValue (groupNValue 1)
    | length groups == 2 = FullHouse bigGroupValue (groupNValue 1)
    | hasSameSuit = Flush xs
    | isConsecutive = Straight maxValue
    | length bigGroup == 3 = Three bigGroupValue (concat $ tail groups)
    | length groups == 3 = TwoPairs bigGroupValue (groupNValue 1) (groupNValue 2)
    | length groups == 4 = OnePair bigGroupValue (concat $ tail groups)
    | otherwise = HighCard maxValue (delete maxCard xs)
    where
      isConsecutive = consecutiveValues xs
      hasSameSuit = sameSuit xs
      maxCard = highestCard xs
      maxValue = highestValue xs
      groups = reverse . sortBy (comparing length) $ groupValues xs
      bigGroup = head groups
      bigGroupValue = cardValue $ head bigGroup
      groupNValue n = cardValue . head $ groups!!n
\end{code}

\begin{code}
solution :: [(Hand,Hand)] -> Int
solution = length . filter (uncurry (>)) . map (calculateRank *** calculateRank)
\end{code}

\begin{code}
main :: IO ()
main = do
  contents <- readFile "problem054.txt"
  let plays = map toHands $ lines contents
  print $ solution plays
  where toHands = (take 5 &&& drop 5) . stringToHand
\end{code}
