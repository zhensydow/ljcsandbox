import qualified Data.Map as DM( toList, empty, insert, lookup, Map )
import Control.Monad( replicateM_ )
import Data.Array.ST( runSTUArray )
import Data.Array.MArray( newArray, readArray, writeArray )
import Data.Array.Unboxed( UArray, (!) )
import Control.Monad( forM_ )

nextPrime i 
    | i `mod` 6 == 1 = i + 4
    | otherwise = i + 2

primesPlus = 2:3:iterate nextPrime 5

primeDecomp n = DM.toList $ primeDecomp' n primesPlus DM.empty
primeDecomp' :: Integer -> [Integer] -> DM.Map Integer Integer -> DM.Map Integer Integer
primeDecomp' 1 _ d = d
primeDecomp' n (p:ps) d
    | n `mod` p == 0 = primeDecomp' (n `div` p) (p:ps) (incDic d p)
    | otherwise = primeDecomp' n ps d

incDic :: (Ord k, Num a) => DM.Map k a -> k -> DM.Map k a
incDic d k = DM.insert k (maybe 1 (+1) (DM.lookup k d)) d

sigma1 n = product [((p^(a+1)) - 1) `div` (p - 1) | (p,a) <- primeDecomp n]
divisorSumation table n = (table!n) - n

incarray arr n i = do
  v <- readArray arr i
  writeArray arr i (v+n)

numDivisorsArray :: Int -> UArray Int Int
numDivisorsArray n = runSTUArray $ do
                       arr <- newArray (1,n) 1
                       forM_ [2..n] $ \i -> do
                                     forM_ [ i, i+i .. n] $ incarray arr i
                       return arr

testCase table = do
  n <- getLine
  print $ divisorSumation table (read n)
  
main = do
  let table = numDivisorsArray 500000
  num <- getLine
  replicateM_ (read num) (testCase table)
