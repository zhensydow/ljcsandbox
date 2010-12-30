%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Euler( 
             digits, digits', toint, primeDecomp, totient, fact, sigma1, 
             isPrime, primes ) 
    where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Char( digitToInt )
import qualified Data.Map as DM( toList, empty, insert, lookup, Map )
import Data.Ratio( (%) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
digits = map (fromIntegral . digitToInt) . show
\end{code}

\begin{code}
digits' = map (fromIntegral . digitToInt)
\end{code}

\begin{code}
toint = foldl (\a b-> a * 10 + fromIntegral b) 0
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
divides n p = n `mod` p == 0
\end{code}

\begin{code}
primes :: [Int]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
\end{code}

\begin{code}
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes
\end{code}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
mulPrimeDecomp = product . map (cociente . fst)
    where cociente p = 1 - (1 % p)
\end{code}

\begin{code}
totient n = round . fromRational $ fromInteger n * (mulPrimeDecomp $ primeDecomp n)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
fact 0 = 1
fact n = product [1..n]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sigma1 es la suma de los divisores de n
\begin{code}
sigma1 n = product [((p^(a+1)) - 1) `div` (p - 1) | (p,a) <- primeDecomp n]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
