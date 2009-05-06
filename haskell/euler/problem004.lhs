\begin{code}
import Data.List ( sort )
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
palindromo xs = xs == reverse xs
\end{code}

\begin{code}
d2 = head $ reverse $ sort [ x*y | x <- [1..99], y <- [1..99], palindromo $ digits $ x*y ]
\end{code}

\begin{code}
d3 = head $ filter (palindromo.digits) $ reverse $ sort [x*y | x <- reverse [100..999], y <- reverse [x..999]]
\end{code}


\begin{code}
solution = d3
\end{code}