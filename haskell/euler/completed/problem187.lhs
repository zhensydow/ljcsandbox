A composite is a number containing at least two prime factors. For example, 15 = 3  5; 9 = 3 3; 12 = 2  2  3.

There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime factors?

\begin{code}
import Euler(primeDecomp)
import Data.Array.ST( runSTUArray )
import Data.Array.MArray( newArray, readArray, writeArray )
import Data.Array.Unboxed( UArray, assocs )
import Control.Monad( forM_, when )
\end{code}

\begin{code}
numberPrimeFactors = sum . fmap snd . primeDecomp . fromIntegral
\end{code}

\begin{code}
compositesBelow :: Int -> [Int]
compositesBelow n = filter ((==2) . numberPrimeFactors) [2..(n-1)]
\end{code}

\begin{code}
sumfactors arr i = do
  v <- readArray arr i
  if v == 0 
    then return 1
    else do
      sumf <- readArray arr (i `div` v)
      return $ 1 + sumf
         
putarray arr v i = do
  writeArray arr i v

strangeArray :: Int -> UArray Int Int
strangeArray n = runSTUArray $ do
  arr <- newArray (1,n) 0
  
  forM_ [2..n] $ \i -> do
    v <- readArray arr i
    when (v==0) $ do
      forM_ [i+i, i+2*i .. n] $ putarray arr i
  
  forM_ [1..n] $ \i -> do
    v <- sumfactors arr i
    writeArray arr i v
  
  return arr
\end{code}

\begin{code}
compositesBelow' :: Int -> [Int]
compositesBelow' n = fmap fst . filter ((==2).snd) . assocs $ strangeArray (n-1)
\end{code}

\begin{code}
solution :: Int
solution = length $ compositesBelow' (10^8) -- (10^8)
\end{code}

\begin{code}
main :: IO ()
main = print solution
\end{code}
