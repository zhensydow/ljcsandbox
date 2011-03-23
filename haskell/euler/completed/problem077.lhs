\begin{code}
import Euler( primes )
import Control.Arrow( (&&&) )
import Data.List( find )

\end{code}

\begin{code}
ccvals 0 _ = []
ccvals _ [] = []
ccvals amount (x:xs)
    | amount == x = [[x]]
    | amount < 0 = []
    | otherwise = (ccvals amount xs) ++ (map (x:) (ccvals (amount - x) (x:xs)))
\end{code}

\begin{code}
cc 0 _ = 1
cc _ [] = 0
cc amount (x:xs)
    | amount < 0 = 0
    | otherwise = (cc amount xs) + (cc (amount - x) (x:xs))
\end{code}

\begin{code}
sumOfPrimes n = cc n $ takeWhile (<=n) primes
\end{code}

\begin{code}
waysOfPrimes n = ccvals n $ takeWhile (<=n) primes
\end{code}

\begin{code}
solution = head $ dropWhile ((<5000).snd) $ map (id &&& sumOfPrimes) [1..]
\end{code}

\begin{code}
main = print solution
\end{code}
