{- Let p(n) represent the number of different ways in which n coins can be
separated into piles. For example, five coins can separated into piles in
exactly seven different ways, so p(5)=7.

OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O
Find the least value of n for which p(n) is divisible by one million.
-}

import Control.Monad( forM_ )
import Data.Array.ST( runSTArray, newArray, readArray, writeArray )
import Data.Array( Array(..), assocs, (!) )
import Data.Int( Int64 )

k m 
  | even m = (m `quot` 2) + 1
  | otherwise = -((m `quot` 2) + 1)

f k = (k*(3*k - 1)) `quot` 2

penta = f . k

indexes n = [n - i | i <- takeWhile (<=n) $ fmap penta [0..]]

signs = cycle [1,1,-1,-1]

fff arr (s,i) = do
  v <- readArray arr i
  return (s * v)

pArray :: Int -> Array Int Integer
pArray n = runSTArray $ do
  arr <- newArray (0,n) 0
  writeArray arr 0 1
  forM_ [1..n] $ \i -> do
    vals <- mapM (fff arr) $ zip signs (indexes i)
    writeArray arr i $ sum vals
    
  return arr

search :: [(Int, Integer)] -> [(Int, Integer)]
search = take 1 . dropWhile (\(_,a) -> a `mod` 1000000 /= 0)

solution :: [(Int, Integer)]
solution = search . assocs $ pArray 100000

main :: IO ()
main = do
  case solution of
    [(x,_)] -> print x
    _ -> putStrLn "not found!"
