\begin{code}
import Control.Arrow( (&&&) )
import Data.List( maximumBy )
import Data.Ord( comparing )
\end{code}

\begin{code}
divides n p = n `mod` p == 0
\end{code}

\begin{code}
primes :: [Int]
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
fun a b = (\n -> n*n + a * n + b)
\end{code}

\begin{code}
genprimos a b = takeWhile (\a->isPrime a && (a>0)) $ map (fun a b) [0..]
\end{code}

\begin{code}
longs = [((a,b),length $ genprimos a b) | b <- takeWhile (<=1000) primes, a <- [-1000..1000]]
\end{code}

\begin{code}
solution = maximumBy (comparing snd) $ longs
\end{code}

\begin{code}
main = print solution
\end{code}
