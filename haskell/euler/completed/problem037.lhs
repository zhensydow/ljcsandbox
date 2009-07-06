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
isPrime 1 = False
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
listToInt :: [Integer] -> Integer
listToInt = foldl (\a b -> a * 10 + b) 0
\end{code}

\begin{code}
removeldigit = listToInt.tail.digits
\end{code}

\begin{code}
removerdigit = listToInt.init.digits
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
