\begin{code}
selections :: [a] -> [(a,[a])]
selections [] = []
selections (x:xs) = (x, xs) : [(y, x:ys) | (y,ys) <- selections xs]
\end{code}

\begin{code}
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y : zs | (y,ys) <- selections xs
                  , zs <- permutations ys ]
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
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}

\begin{code}
listToInt :: [Integer] -> Integer
listToInt = foldl (\a b -> a * 10 + b) 0
\end{code}

\begin{code}
pandigits n = map listToInt $ permutations [1..n]
\end{code}

\begin{code}
allpans = concat $ map pandigits [4..9]
\end{code}

\begin{code}
panprimes = filter isPrime allpans
\end{code}

\begin{code}
solution = maximum panprimes
\end{code}