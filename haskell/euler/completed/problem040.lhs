\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
irrational = concatMap digits [1..]
\end{code}

\begin{code}
d n = irrational !! (n-1)
\end{code}

\begin{code}
solution = (d 1) * (d 10) * (d 100) * (d 1000) * (d 10000) * (d 100000) * (d 1000000)
\end{code}

\begin{code}
main = print solution
\end{code}