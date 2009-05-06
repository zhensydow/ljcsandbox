\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
factorial 0 = 1
factorial m@(n+1) = m * factorial n
\end{code}

\begin{code}
solution = sum $ digits $ factorial 100
\end{code}
