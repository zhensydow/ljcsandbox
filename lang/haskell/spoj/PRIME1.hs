import Control.Monad( liftM, replicateM_ )

divides n p = n `mod` p == 0

primes :: [Int]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes

primesFromTo :: Int -> Int -> [Int]
primesFromTo m n = takeWhile (n>=) $ filter isPrime $ candidatesFromTo m

candidatesFromTo :: Int -> [Int]
candidatesFromTo m = m:[6*k+r | k <- [ka..], r <- [1,5]]
  where
    ka = ((m - 1) `div` 6) + 1

testCase :: IO ()
testCase = do
  [begin,end] <- liftM words $ getLine
  mapM_ print $ primesFromTo (read begin) (read end)
  putStrLn ""
  return ()
  
main = do
  num <- getLine
  replicateM_ (read num) testCase