\begin{code}
import Data.Char
\end{code}

\begin{code}
digits :: Int -> [Int]
digits = map digitToInt . show
\end{code}

\begin{code}
digitsbin n = reverse $ digitsbin' n
    where digitsbin' n
              | n < 2 = [n]
              | otherwise = n `mod` 2 : (digitsbin' $ n `div` 2)
\end{code}

\begin{code}
palin xs = xs == (reverse xs)
\end{code}

\begin{code}
numbers = filter (palin.digitsbin) $ filter (palin.digits) [1..1000000]
\end{code}

\begin{code}
solution = sum numbers
\end{code}