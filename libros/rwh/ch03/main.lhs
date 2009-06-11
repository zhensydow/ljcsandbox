\begin{code}
import Data.List (sortBy)
\end{code}

\begin{code}
data List a = Cons a (List a)
            | Nil
              deriving( Show )
\end{code}

\begin{code}
fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)
\end{code}

\begin{code}
toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
\end{code}

\begin{code}
data MTree a = MTree (Maybe (a ,MTree a, MTree a))
              deriving( Show )
\end{code}

\begin{code}
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
\end{code}

\begin{code}
mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral.length $ xs)
\end{code}

\begin{code}
mkPalindromo xs = xs ++ reverse xs
\end{code}

\begin{code}
isPalindromo xs = xs == reverse xs
\end{code}

\begin{code}
sortLen = sortBy (\a b->compare (length a) (length b))
\end{code}

\begin{code}
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse x (y:xs) = y ++ [x] ++ intersperse x xs
\end{code}

\begin{code}
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
\end{code}

\begin{code}
height Empty = 0
height (Node _ tl tr) = 1 + max (height tl) (height tr)
\end{code}

\begin{code}
data Direction = DLeft
               | DRigth
               | DStraight
               deriving( Show )
\end{code}

\begin{code}
data Point2D = P Double Double
               deriving( Show )
\end{code}

\begin{code}
cross :: Point2D -> Point2D -> Point2D -> Double
cross (P x1 y1) (P x2 y2) (P x3 y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
\end{code}

\begin{code}
direction :: Point2D -> Point2D -> Point2D -> Direction
direction p1 p2 p3
    | cross p1 p2 p3 > 0 = DLeft
    | cross p1 p2 p3 < 0 = DRigth
    | otherwise = DStraight
\end{code}

\begin{code}
listdirs (x:y:z:xs) = direction x y z : listdirs (y:z:xs)
listdirs _ = []
\end{code}
