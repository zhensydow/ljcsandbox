Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

\begin{code}
import Euler( isPrime )
\end{code}

\begin{code}
sizeSpiral :: Int -> Int
sizeSpiral n = foldr (+) 1 . map (*4) $ [(n-1),(n-3)..1]
\end{code}

\begin{code}
spiral :: Int -> [Int]
spiral 0 = error "Spiral can have even side length"
spiral 1 = [1]
spiral n = [1..limit]
    where limit = sizeSpiral n
\end{code}

\begin{code}
diagonalNumbers :: Int -> [Int]
diagonalNumbers 0 = error "Spiral can have even side length"
diagonalNumbers 1 = []
diagonalNumbers n = (diagonalNumbers (n-2)) ++ newLayer
    where
      newLayer = drop 1 $ take 5 $ iterate (+(n-1)) $ sizeSpiral (n-2)
\end{code}

\begin{code}
diagonalLayer :: Int -> [Int]
diagonalLayer n = drop 1 $ take 5 $ iterate (+(n-1)) $ sizeSpiral (n-2)
\end{code}

\begin{code}
iterateSpiral :: Int -> Int -> Int -> Double -> Int
iterateSpiral side np nl limit
    | ((fromIntegral np) / (fromIntegral nl)) < limit = side
    | otherwise = iterateSpiral (side+2) (np + nnewprimes) (nl + 4) limit
    where
      nnewprimes = length newprimes
      newprimes = filter (isPrime) $ diagonalLayer (side+2)
\end{code}

\begin{code}
solution :: Int
solution = iterateSpiral 3 3 5 0.10
\end{code}

\begin{code}
main :: IO ()
main = print solution
\end{code}
