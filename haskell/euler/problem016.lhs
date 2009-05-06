\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
number = digits $ 2^1000
\end{code}

\begin{code}
solution = sum number
\end{code}