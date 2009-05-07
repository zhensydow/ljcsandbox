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
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
number xs = sum $ zipWith (*) (iterate (*10) 1) (reverse xs)
\end{code}

\begin{code}
rotate xs = (drop 1 xs) ++ [head xs]
\end{code}

\begin{code}
cycles xs = take (length xs) $ iterate rotate xs
\end{code}

\begin{code}
isCircular n = all isPrime $ tail $ map number $ cycles $ digits n
\end{code}

\begin{code}
solution = length $ filter isCircular $ takeWhile (<1000000) primes
\end{code}

\begin{code}
main = print solution
\end{code}