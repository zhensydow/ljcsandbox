\begin{code}
import Data.List( sort )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
fun n = [sort $ digits (n*a)| a <- [2..6]]
\end{code}

\begin{code}
allequal [] = True
allequal (x:[]) = True
allequal (x:y:xs)
    | x == y = allequal (y:xs)
    | otherwise = False
\end{code}

\begin{code}
solution = fst . head $ filter (allequal . snd) $ map (id &&& fun) [1..]
\end{code}

\begin{code}
main = print solution
\end{code}