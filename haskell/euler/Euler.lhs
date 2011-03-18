%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Euler
    ( digits, digits', toint, fact, primeDecomp, firstFactor, 
      totient, sigma0, sigma1, numDivisors,
      isPrime, primes, primesPlus, primesPlusFrom,
      permutations, isPermutation, combinations ) 
    where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Char( digitToInt )
import qualified Data.Map as DM( toList, empty, insert, lookup, Map )
import Data.Ratio( (%) )
import Data.List( (\\) )
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
primesPlusFrom n = iterate nextPrime n
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
mulPrimeDecomp = product . map (cociente . fst)
    where cociente p = 1 - (1 % p)
\end{code}

\begin{code}
totient n = round . fromRational $ fromInteger n * (mulPrimeDecomp $ primeDecomp n)
\end{code}

\begin{code}
firstFactor k = firstFactor' k primesPlus
firstFactor' 1 _ = 1
firstFactor' n (p:ps) 
    | n `mod` p == 0 = p
    | otherwise = firstFactor' n ps
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
fact 0 = 1
fact n = product [1..n]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
En general sigma_x(n) es definida como la suma de la x-esima potencia de los 
divisores positivos de n

sigma0 es el numero de los divisores de n === d(n)
\begin{code}
sigma0 n = product [(a + 1) | (_,a) <- primeDecomp n]
numDivisors = sigma0
\end{code}

sigma1 es la suma de los divisores de n
\begin{code}
sigma1 n = product [((p^(a+1)) - 1) `div` (p - 1) | (p,a) <- primeDecomp n]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
permutations :: Eq a => [a] -> [[a]]
permutations xs = [x:ps | x <- xs, ps <- permutations (xs\\[x])]
\end{code}

\begin{code}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = null $ xs \\ ys
\end{code}

\begin{code}
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs
\end{code}
