import Control.Monad( replicateM_ )

palindrome :: Int -> Bool
palindrome n = reverse sn == sn
  where sn = show n
        
testCase = do
  n <- getLine
  print . head . dropWhile (not.palindrome) $ [(read n) + 1 ..]
  
main = do
  num <- getLine
  replicateM_ (read num) testCase
