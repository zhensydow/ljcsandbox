import Data.Map( fromListWith, assocs )
import Control.Monad( liftM, replicateM_, replicateM, forM_ )

testCase = do
  num <- getLine
  mlines <- replicateM (read num) getLine
  let table = fromListWith (+) $ zip mlines (repeat 1)
  forM_ (assocs table) $ \(k,v) -> do
    putStr k
    putStr " "
    putStrLn . show $ v
  
main = do
  num <- getLine
  replicateM_ ((read num) - 1) $ testCase >> getLine >> putStrLn ""
  testCase
