\begin{code}
import Control.Arrow( (&&&) )
import Data.Ord( comparing )
import Data.List( sortBy )
\end{code}

\begin{code}
isRightTriangle :: Num a => (a,a,a) -> Bool
isRightTriangle (a,b,c) = ((a ^ 2) + (b ^ 2)) == (c ^ 2)
\end{code}

\begin{code}
triangles :: Int -> [(Int,Int,Int)]
triangles p = [(a,b,c) | 
               a <- [1..(((p-3) `div` 3)+1)]
              , b <- [a..(((p-a) `div` 2)+1)]
              , c <- [p - a - b]
              , isRightTriangle (a,b,c) ]
\end{code}

\begin{code}
solutions = map (id &&& length . triangles)
\end{code}

\begin{code}
solution = (fst.head.reverse) $ sortBy (comparing snd) $ solutions [1..1000]
\end{code}

\begin{code}
main = print solution
\end{code}
