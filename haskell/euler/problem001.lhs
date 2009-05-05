\begin{code}
numbers n = [x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0]
\end{code}

\begin{code}
solution = sum $ numbers 999
\end{code}

