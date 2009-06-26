\begin{code}
sumdigits num 
    | d > 0 = r + sumdigits d
    | otherwise = r
    where (d,r) = num `divMod` 10
\end{code}

\begin{code}
numbers = [ a ^ b | a <- [1..100], b <- [1..100] ]
\end{code}

\begin{code}
sums = map sumdigits numbers
\end{code}

\begin{code}
solution = maximum sums
\end{code}