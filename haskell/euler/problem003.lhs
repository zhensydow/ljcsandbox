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
number :: Integer
number = 600851475143

sqNumber = ceiling $ sqrt $ fromInteger number
\end{code}

\begin{code}
testList = reverse $ takeWhile (<sqNumber) primes
\end{code}

\begin{code}
checkLargest :: [Integer] -> Integer
checkLargest [] = error "no prime"
checkLargest (x:xs)
    | number `mod` x == 0 = x
    | otherwise = checkLargest xs
\end{code}

\begin{code}
solution = checkLargest testList
\end{code}

\begin{code}
main = print solution
\end{code}