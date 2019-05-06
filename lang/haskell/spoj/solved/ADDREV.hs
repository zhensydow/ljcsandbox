import Control.Monad( liftM, replicateM_ )

summ :: Int -> Int -> Int
summ a b = a + b

testCase = do
  [a,b] <- liftM words $ getLine  
  putStrLn . dropWhile (=='0') . reverse . show $ summ (read $ reverse a) (read $ reverse b)

main = do
  num <- getLine
  replicateM_ (read num) testCase
