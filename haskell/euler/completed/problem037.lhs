\begin{code}
import Euler( digits, toint )
\end{code}

\begin{code}
divides n p = n `mod` p == 0
\end{code}

\begin{code}
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
\end{code}

\begin{code}
isPrime 0 = False
isPrime 1 = False
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
removeldigit = toint.tail.digits
\end{code}

\begin{code}
removerdigit = toint.init.digits
\end{code}

\begin{code}
lnumbers = takeWhile (>0) . iterate removeldigit
\end{code}

\begin{code}
rnumbers = takeWhile (>0) . iterate removerdigit
\end{code}

\begin{code}
checkTruncatable n = isleft && isrigth
    where isleft = and $ map isPrime $ lnumbers n
          isrigth = and $ map isPrime $ rnumbers n
\end{code}

\begin{code}
solution = sum $ take 11 $ [x | x <- dropWhile (<10) primes, checkTruncatable x]
\end{code}

\begin{code}
main = print solution
\end{code}