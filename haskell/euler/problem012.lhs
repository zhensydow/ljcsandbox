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
divisors2 n = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]

divisors n 
    | odd n = 1:[x | x <- [3,5..(n `div` 2)], n `mod` x == 0]
    | otherwise = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]
\end{code}

\begin{code}
iterateTriangle = iterateTriangle' 1 1
iterateTriangle' c n 
    | divs < 499 = iterateTriangle' (c+1) (n+c+1)
    | otherwise = n
    where divs = length $ divisors n
\end{code}

\begin{code}
solution = iterateTriangle
\end{code}

\begin{code}
main = print solution
\end{code}