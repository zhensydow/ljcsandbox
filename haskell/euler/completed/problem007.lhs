\begin{code}
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
\end{code}

\begin{code}
solution = primes !! 10000
\end{code}