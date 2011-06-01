import Control.Monad( liftM, replicateM_ )

divides n p = n `mod` p == 0

primes :: [Int]
primes = 2:3:primes'
  where
    1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
    primes' = p : filter isPrime' candidates
    isPrime' n = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'

primesFromTo :: Int -> Int -> [Int]
primesFromTo m n = dropWhile (<m) $ takeWhile (n>=) primes

testCase :: IO ()
testCase = do
  [begin,end] <- liftM words $ getLine
  mapM_ print $ primesFromTo (read begin) (read end)
  putStrLn ""
  return ()
  
main = do
  num <- getLine
  replicateM_ (read num) testCase