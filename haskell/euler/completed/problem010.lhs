\begin{code}
primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime candidates
    isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
    divides n p = n `mod` p == 0
\end{code}

\begin{code}
solution = sum $ takeWhile (<2000000) primes
\end{code}

\begin{code}
main = print solution
\end{code}
