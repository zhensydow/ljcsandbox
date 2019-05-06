\begin{code}
import Data.List( tails, sortBy )
import Data.Ord( comparing )
\end{code}

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
isPrime 0 = False
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
consecutivesum xs = scanl f (0,0) xs
    where f (n,c) y = (n+y, c+1)
\end{code}

\begin{code}
primessum n = filter (isPrime.fst) $ filter ((<=n).fst) $ concatMap consecutivesum $ tails $ takeWhile (<n) primes
\end{code}

\begin{code}
solution = last $ sortBy (comparing (snd)) $ primessum 100000
\end{code}

\begin{code}
main = print solution
\end{code}
