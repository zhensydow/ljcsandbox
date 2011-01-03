Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d 1,000,000?

\begin{code}
import Data.Array.ST
    ( runSTUArray, newArray_, newListArray, writeArray, readArray, getBounds )
import Data.Array.Unboxed( UArray(..), (!) )
import Control.Monad( forM_, when )
import Data.List( group )
import Control.Arrow( (&&&) )
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
solution = sum $ map (toInteger . phiUA divisors) [2..1000000]
    where divisors = divisorsArray 1000000
\end{code}

\begin{code}
main = print solution
\end{code}
