\begin{code}
divides n p = n `mod` p == 0
\end{code}

\begin{code}
primes :: [Integer]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
\end{code}

\begin{code}
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
composite = filter (not.isPrime) [9,11..]
\end{code}

\begin{code}
check n = any isPrime $ takeWhile (>0) $ map (\y -> n - 2*y*y) [1..]
\end{code}

\begin{code}
solution = head $ filter (not.check) composite
\end{code}

\begin{code}
main = print solution
\end{code}