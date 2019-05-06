\begin{code}
import Data.List( sort )
import Euler( digits, toint )
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
numbers = [ [x,x+3330,x+3330+3330] | x <- [1000..3339]
          , isPrime x, isPrime (x+3330), isPrime (x+3330+3330) ]
\end{code}

\begin{code}
same x y = (sort $ digits x) == (sort $ digits y)
\end{code}

\begin{code}
sequences = filter check numbers
    where check [] = True
          check (x:[]) = True
          check (x:y:xs)
              | same x y = check (y:xs)
              | otherwise = False
\end{code}

\begin{code}
solution = toint $ concatMap digits $ head $ filter ((/=1487).head) sequences
\end{code}

\begin{code}
main = print solution
\end{code}
