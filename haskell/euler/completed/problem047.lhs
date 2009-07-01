\begin{code}
import qualified Data.Map as DM
import Control.Arrow ((&&&))
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
primeFactors = map fst . primeDecomp
\end{code}

\begin{code}
numbers n = [ a | a <- [1..], (length (primeFactors a) == n) ]
\end{code}

\begin{code}
checkf3 :: (Num a, Ord a) => [a] -> (a,a,a)
checkf3 (x:y:z:xs)
    | (z - x) == 2 = (x,y,z)
    | (z - y) > 1 = checkf3 (z:xs)
    | otherwise = checkf3 (y:z:xs)
\end{code}

\begin{code}
checkf4 :: (Num a, Ord a) => [a] -> (a,a,a,a)
checkf4 (x:y:z:w:xs)
    | (w - x) == 3 = (x,y,z,w)
    | (w - z) > 1 = checkf4 (w:xs)
    | (w - y) > 2 = checkf4 (z:w:xs)
    | otherwise = checkf4 (y:z:w:xs)
\end{code}
