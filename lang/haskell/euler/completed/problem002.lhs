\begin{code}
fib = 1:2:zipWith (+) fib (tail fib)
\end{code}

\begin{code}
solution = sum [x| x <- takeWhile (<4000000) fib, even x]
\end{code}