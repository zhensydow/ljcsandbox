\begin{code}
import Euler( primeDecomp )
import Control.Arrow ((&&&))
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

\begin{code}
main = print . checkf4 $ numbers 4
\end{code}