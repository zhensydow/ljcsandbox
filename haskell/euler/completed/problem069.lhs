\begin{code}
import Data.List( inits )
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
candidates = last $ takeWhile ((<1000000).product) $ scanl (\a b -> a ++ [b]) [] primes
\end{code}

\begin{code}
solution = maximum . map product . inits $ candidates
\end{code}
