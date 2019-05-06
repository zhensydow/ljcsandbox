%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Euler
    ( digits, digits', toint, fact, primorial, primeDecomp, firstFactor, 
      divisors, totient, sigma0, sigma1, numDivisors, numDivisorsArray,
      isPrime, isPrime', primes, primesPlus, primesPlusFrom, nextPrime,
      permutations, isPermutation, combinations, cc, ccvals, insertInto,
      isSquare, isIntegral, msqrt, msqrt', divides, uniques, uniquesInt ) 
    where
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Char( digitToInt )
import qualified Data.Map as DM( toList, empty, insert, lookup, Map )
import Data.Ratio( (%) )
import Data.List( (\\), foldl', permutations )
import Data.Array.ST( runSTUArray )
import Data.Array.MArray( newArray, readArray, writeArray )
import Data.Array.Unboxed( UArray )
import qualified Data.IntSet as IS( toList, fromList )
import qualified Data.Set as S( toList, fromList )
import Control.Monad( forM_ )
import Control.Parallel( par, pseq )
import Control.Arrow( second )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
digits = map (fromIntegral . digitToInt) . show
\end{code}

\begin{code}
digits' = map (fromIntegral . digitToInt)
\end{code}

\begin{code}
toint = foldl' (\a b-> a * 10 + fromIntegral b) 0
\end{code}

\begin{code}
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]
\end{code}

\begin{code}
uniquesInt = IS.toList . IS.fromList
\end{code}

\begin{code}
uniques = S.toList . S.fromList
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
isSquare x = ix * ix == x
    where
      ix = truncate . sqrt . fromIntegral $ x
\end{code}

\begin{code}
isIntegral x = (fromIntegral . truncate) x == x
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
nextPrime n
    | isPrime n = n
    | otherwise = nextPrime (n+1)
\end{code}

\begin{code}
nextPrimePlus i 
    | i `mod` 6 == 1 = i + 4
    | otherwise = i + 2
\end{code}

\begin{code}
primesPlus = 2:3:iterate nextPrimePlus 5
\end{code}

\begin{code}
primesPlusFrom n = iterate nextPrimePlus n
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


Miller-Rabin Primality Test
http://www.haskell.org/haskellwiki/Testing_primality

\begin{code}
find2Decomp :: Integral a => a -> (a,a)
find2Decomp n = f 0 n
    where 
      f k m
          | r == 1 = (k,m)
          | otherwise = f (k+1) q
          where 
            (q,r) = m `quotRem` 2
\end{code}

\begin{code}
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b*c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b*b) `rem` a
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)
\end{code}

\begin{code}
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq a b = f a b 1
    where
      f x n y
          | n == 1 = x `mul` y
          | r == 0 = f x' q y
          | otherwise = f x' q (x `mul` y)
          where
            (q,r) = quotRem n 2
            x' = sq x
\end{code}

\begin{code}
millerRabinTest n a
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
      b0 = powMod n a m
      (k,m) = find2Decomp n'
      n' = n - 1
      b = take (fromIntegral k) $ iterate (squareMod n) b0
      iter [] = False
      iter (x:xs) 
          | x == 1 = False
          | x == n' = True
          | otherwise = iter xs
\end{code}

\begin{code}
isPrime' 2 = True
isPrime' 3 = True
isPrime' n = millerRabinTest n 2
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

\begin{code}
primorial n = product $ take n primes
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

\begin{code}
incarray arr i = do
  v <- readArray arr i
  writeArray arr i (v+1)

numDivisorsArray :: Int -> UArray Int Int
numDivisorsArray n = runSTUArray $ do
                       arr <- newArray (1,n) 1
                       forM_ [2..n] $ \i -> do
                                     forM_ [ i, i+i .. n] $ incarray arr
                       return arr
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
prop_permutations xs = permutations xs == permutations' xs
\end{code}

\begin{code}
permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [x:ps | x <- xs, ps <- permutations (xs\\[x])]
\end{code}

\begin{code}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = null $ xs \\ ys
\end{code}

\begin{code}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = let ck = combinations k xs 
                            ck1 = combinations (k-1) xs
                        in ck `par` ck1 `pseq` (map (x:) ck1 ++ ck)
\end{code}

FROM SICP

\begin{code}
cc :: Integral t => t -> [t] -> [[t]]
cc 0 _ = []
cc _ [] = []
cc amount (x:xs)
    | amount < 0 = []
    | otherwise = (cc amount xs) ++ (map (x:) (cc (amount - x) (x:xs)))
\end{code}

\begin{code}
ccvals :: Integral t => t -> [t] -> [[t]]
ccvals 0 _ = []
ccvals _ [] = []
ccvals amount (x:xs)
    | amount == x = [[x]]
    | amount < 0 = []
    | otherwise = (ccvals amount xs) ++ (map (x:) (ccvals (amount - x) (x:xs)))
\end{code}

\begin{code}
insertInto :: Int -> a -> [a] -> [[a]]
insertInto 0 _ xs = [xs]
insertInto n v [] = [replicate n v]
insertInto n v (x:xs) = (map (v:) $ insertInto (n-1) v (x:xs)) ++ (map (x:) $ insertInto n v xs)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
toinfinite :: [Int] -> [Int]
toinfinite dgts = pre ++ dgts ++ repeat 0
    where
      pre = if odd (length dgts) then [0] else []
\end{code}

\begin{code}
numpairs :: [Int] -> Int
numpairs dgts = ((length dgts) `div` 2) + 1
\end{code}

\begin{code}
calcx :: Integer -> Integer -> Integer
calcx c p = last . takeWhile check $ [0 .. ] 
    where
      check x = c >= ((20*p + x) * x)
\end{code}

\begin{code}
ddsqrt :: [Int] -> Integer -> Integer -> [Int]
ddsqrt xs r p = (fromInteger x) : ddsqrt rest nr (p*10 + x)
    where
      (next,rest) = splitAt 2 xs
      c =  r * 100 + (toint next)
      x = calcx c p
      y = (20*p + x) * x
      nr = c - y
\end{code}

\begin{code}
msqrt :: Integer -> [Int]
msqrt n = ddsqrt (toinfinite $ digits n) 0 0
\end{code}

\begin{code}
msqrt' :: Integer -> Int -> ([Int],[Int])
msqrt' n p = (second $ take p) . splitAt nn $ ddsqrt (toinfinite dgts) 0 0
    where
      dgts = digits n
      nn = numpairs dgts
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
