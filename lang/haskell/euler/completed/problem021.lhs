\begin{code}
divisors n = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]
\end{code}

\begin{code}
d n = sum $ divisors n
\end{code}

\begin{code}
amicables = [ x | x <- [1..10000], (d (d x)) == x, (d x) /= x]
\end{code}

\begin{code}
solution = sum amicables
\end{code}

\begin{code}
main = print solution
\end{code}