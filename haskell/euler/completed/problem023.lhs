\begin{code}
import Euler( sigma1 )
import Data.IntSet( fromList, toList )
\end{code}

Sloane A048242

\begin{code}
limit :: Int
limit = 20161
\end{code}

\begin{code}
abundants :: [Int]
abundants = [ a | a <- [12 .. limit], sigma1 (fromIntegral a) > (fromIntegral $ 2*a) ]
\end{code}

\begin{code}
uniques :: [Int] -> [Int]
uniques = toList . fromList
\end{code}

\begin{code}
sumAbundants :: Int
sumAbundants = sum . uniques $ [ x + y | x <- abundants, y <- abundants, (x+y) <= limit]
\end{code}

\begin{code}
solution :: Int
solution = sum [1..limit] - sumAbundants
\end{code}

\begin{code}
main :: IO ()
main = print solution
\end{code}
