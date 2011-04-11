%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import System.IO( openFile, hGetContents, IOMode(..) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
test01 :: Array (Int,Int) Integer
test01 = listArray ((0,0),(4,4)) [
          131, 673, 234, 103, 18, 
          201, 96, 342, 965, 150, 
          630, 803, 746, 422, 111, 
          537, 699, 497, 121, 956, 
          805, 732, 524, 37, 331]
\end{code}

\begin{code}
test02 :: IO (Array (Int,Int) Integer)
test02 = do
  h <- openFile "matrix081.txt" ReadMode
  dat <- hGetContents h
  let mat = read dat
  return $ listArray ((0,0),(79,79)) $ concat mat
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
calculateCosts :: Array (Int,Int) Integer -> Array (Int, Int) Integer
calculateCosts edges = runSTArray $ do

                         let bs@(_,(sx,sy)) = bounds edges
                         arr <- newArray_ bs :: ST s (STArray s (Int,Int) Integer)
                         writeArray arr (0,0) $ edges ! (0,0)
                         forM_ [j | j <- [1 .. sy]] $ \j -> do
                                      v <- readArray arr (0,j-1)
                                      writeArray arr (0,j) $ v + (edges ! (0,j))
                         forM_ [i | i <- [1 .. sx]] $ \i -> do
                                      v <- readArray arr (i-1,0)
                                      writeArray arr (i,0) $ v + (edges ! (i,0))
                                      forM_ [j | j <- [1 .. sy]] $ \j -> do
                                                              v1 <- readArray arr (i-1,j)
                                                              v2 <- readArray arr (i,j-1)
                                                              v3 <- readArray arr (i,j+1)
                                                              writeArray arr (i,j) $ (min v1 (min v2 v3)) + (edges ! (i,j))
                         return arr
\end{code}

\begin{code}
solution = liftM (getTotal . calculateCosts) test02
    where 
      getTotal arr = arr ! (snd . bounds $ arr)
\end{code}

\begin{code}
main = do
  sol <- solution
  print sol
\end{code}
