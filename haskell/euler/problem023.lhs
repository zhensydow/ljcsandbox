\begin{code}
import qualified Data.Map as DM( toList, empty, insert, lookup, Map )
import Data.List( sort, nub )
\end{code}

\begin{code}
nextPrime i 
    | i `mod` 6 == 1 = i + 4
    | otherwise = i + 2
\end{code}

\begin{code}
primesPlus = 2:3:iterate nextPrime 5
\end{code}

\begin{code}
primeDecomp n = DM.toList $ primeDecomp' n primesPlus DM.empty
primeDecomp' :: Integer -> [Integer] -> DM.Map Integer Integer -> DM.Map Integer Integer
primeDecomp' 1 _ d = d
primeDecomp' n (p:ps) d
    | n `mod` p == 0 = primeDecomp' (n `div` p) (p:ps) (incDic d p)
    | otherwise = primeDecomp' n ps d
\end{code}

\begin{code}
incDic :: (Ord k, Num a) => DM.Map k a -> k -> DM.Map k a
incDic d k = DM.insert k (maybe 1 (+1) (DM.lookup k d)) d
\end{code}

\begin{code}
divisors = init . genFromDecomp . primeDecomp
\end{code}

\begin{code}
genFromDecomp [] = [1]
genFromDecomp ((a,n):xs) = [ i*j | 
                            i <- [a ^ k | k <- [0..n]]
                           , j <- genFromDecomp xs ]
\end{code}

\begin{code}
isAbundant n = (sum (divisors n)) > n
\end{code}

\begin{code}
abundants = filter isAbundant [12..(28124 - 12)]
\end{code}

\begin{code}
sumAbundants = nub $ sort $ filter (<28124) [ a+b | a <- abundants, b <- abundants ]
\end{code}

\begin{code}
sumandos n = take 1 [(x,n-x)| x <- takeWhile (<n) abundants, isAbundant (n-x)]
\end{code}

20161 is the last number than can not be expressed as a sum of two bundant numbers (http://mathworld.wolfram.com/AbundantNumber.html)

\begin{code}
notAbun = [ x | x <- [20162,20161..24], (sumandos x) == [] ]
\end{code}

\begin{code}
solution = sum notAbun
\end{code}

\begin{code}
main = print solution
\end{code}