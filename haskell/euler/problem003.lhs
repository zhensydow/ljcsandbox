\begin{code}
primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
\end{code}

\begin{code}
number = 600851475143
sqNumber = ceiling $ sqrt number
\end{code}

\begin{code}
isPrime n = isPrime' n $ ceiling $ sqrt number
    where isPrime' _ 1 = True
          isPrime' n p
              | n `mod` p == 0 = False
              | otherwise = isPrime' n (p-1)
\end{code}