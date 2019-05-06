Let φ be Euler's totient function, i.e. for a natural number n, φ(n) is the number of k, 1  k n, for which gcd(k,n) = 1.

By iterating φ, each positive integer generates a decreasing chain of numbers ending in 1.
E.g. if we start with 5 the sequence 5,4,2,1 is generated.
Here is a listing of all chains with length 4:

5,4,2,1
7,6,2,1
8,4,2,1
9,6,2,1
10,4,2,1
12,4,2,1
14,6,2,1
18,6,2,1
Only two of these chains start with a prime, their sum is 12.

What is the sum of all primes less than 40000000 which generate a chain of length 25?

\begin{code}
import Euler( totient, primes, primesPlus, isPrime )
import Data.Array.ST( runSTUArray, newArray_, newListArray, writeArray, readArray, getBounds )
import Data.Array.Unboxed( UArray(..), (!) )
import Data.Array.MArray
import Control.Monad( forM_, when )
import Data.List( group )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
totientChain 1 = [1]
totientChain n = n : totientChain (totient n)
\end{code}

\begin{code}
phi :: Int -> Int
phi = totient . fromIntegral
primePhi :: Int -> Int
primePhi n = n - 1
\end{code}

\begin{code}
searchLength :: (Int->Int) -> Int -> [(Int,Int)] -> (Int,[(Int,Int)])
searchLength f n [] = error "invalid"
searchLength f n (x:xs)
    | (fst x) == n = (snd x, (x:xs))
    | (fst x) < n = let (l,ys) = searchLength phi (f n) (x:xs)
                    in (1+l, (n, 1+l) : ys)
    | otherwise = let (l,ys) = searchLength phi n xs
                  in (l, x : ys)
\end{code}

\begin{code}
mapPrimeLengths :: [Int] -> [(Int,Int)] -> [(Int,Int)]
mapPrimeLengths [] t = t
mapPrimeLengths (x:xs) t = let (ls,newt) = searchLength primePhi x t
                           in mapPrimeLengths xs newt
\end{code}

39999857   highest prime that generate a 25 chain

295937     lowest prime that generate a 20 chain
557057     lowest prime that generate a 21 chain
1193537    lowest prime that generate a 22 chain
2384897    lowest prime that generate a 23 chain
4227137    lowest prime that generate a 24 chain
9548417    lowest prime that generate a 25 chain

\begin{code}
highestPrime = 39999857
lowestPrime = 9548417 --4227137
myprimes :: [Int]
myprimes = map fromInteger . filter isPrime $ [lowestPrime .. highestPrime]
\end{code}

\begin{code}
table = mapPrimeLengths myprimes [(1,1)]
list s = filter ((==s) . snd) $ table
\end{code}

\begin{code}
miterate [] _ = return ()
miterate (x:xs) table = do
  let newt = mapPrimeLengths [x] [(1,1)]
  if ((==25).snd.head) newt 
    then do
      print $ head newt
    else do putStr "."
  miterate xs newt
solution = map (fromIntegral.fst) $ list 25
--main = print solution
--main = miterate myprimes [(1,1)]
\end{code}

\begin{code}
\end{code}

\begin{code}
divisorsArray :: Int -> UArray Int Int
divisorsArray n = runSTUArray $ do
               arr <- newListArray (1,n) [1..n]
               forM_ [4,6..n] $ \i -> writeArray arr i 2
               forM_ [3,5..n] $ \p -> do
                   divisor <- readArray arr p
                   when (p==divisor) $ do
                             forM_ [3*p,5*p..n] $ \i -> do
                                         v <- readArray arr i
                                         when (v==i) (writeArray arr i p)
               return arr
\end{code}

\begin{code}
factors :: UArray Int Int -> Int -> [Int]
factors _ 1 = []
factors divisors n = divisor : factors divisors (n `div` divisor)
    where 
      divisor = divisors ! n
\end{code}

\begin{code}
countGroups :: [Int] -> [(Int,Int)]
countGroups = map (head &&& length) . group
\end{code}

\begin{code}
phiUA :: UArray Int Int -> Int -> Int
phiUA _ 1 = 1
phiUA divisors n = product . map (\(p,k)-> (p-1) * (p^(k-1))) . countGroups $ factors divisors n
\end{code}

\begin{code}
phiChainsArray :: Int -> UArray Int Int -> UArray Int Int
phiChainsArray n divisors = runSTUArray $ do
                              arr <- newArray_ (1,n)
                              writeArray arr 1 1
                              forM_ [2..n] $ \i -> do
                                            v <- readArray arr (phiUA divisors i)
                                            writeArray arr i (v + 1)
                              return arr
\end{code}

\begin{code}
elements limit n = filter ((==limit) . (chainLengths!)) primeList
    where
      primeList = filter (\p -> p == divisors ! p) [2..n]
      divisors = divisorsArray n
      chainLengths = phiChainsArray n divisors
\end{code}

\begin{code}
solution2 = sum . map toInteger $ elements 25 (fromIntegral highestPrime)
\end{code}

\begin{code}
main = print solution2
\end{code}
