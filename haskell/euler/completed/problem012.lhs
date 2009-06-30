\begin{code}
import Control.Arrow( (&&&) )
import qualified Data.Map as DM ( toList, empty, lookup, insert, Map )
\end{code}

\begin{code}
triangleNumber n = sum [1..n]
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
numDivisors n = product $ map ((+1).snd) $ primeDecomp n
\end{code}

\begin{code}
triangles = map (triangleNumber &&& (numDivisors . triangleNumber)) [1..]
\end{code}

\begin{code}
limit = 500
solution = fst . head $ dropWhile ((<limit).snd) triangles
\end{code}

\begin{code}
main = print solution
\end{code}
