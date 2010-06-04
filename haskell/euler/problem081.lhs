%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Data.Array
import System.IO( openFile, hGetContents, IOMode(..) )
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
type EdgeCost = Array (Int,Int) Integer
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
calcMinimo :: Array (Int,Int) Integer -> (Int,Int) -> Integer
calcMinimo arr (i,j)
    | (i,j) == (x1,y1) = arr!(i,j)
    | i == x1 = arr!(i,j) + (calcMinimo arr (i,j+1))
    | j == y1 = arr!(i,j) + (calcMinimo arr (i+1,j))
    | otherwise = arr!(i,j) 
                  + (min (calcMinimo arr (i,j+1)) (calcMinimo arr (i+1,j)))
    where
      ((x0,y0),(x1,y1)) = bounds arr
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
infinite :: Integer
infinite = -1
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
test01 :: Array (Int,Int) Integer
test01 = listArray ((0,0),(4,4)) [131, 673, 234, 103, 18, 201, 96, 342, 965, 150, 630, 803, 746, 422, 111, 537, 699, 497, 121, 956, 805, 732, 524, 37, 331]
\end{code}

\begin{code}
test02 :: IO (Array (Int,Int) Integer)
test02 = do
  h <- openFile "matrix081.txt" ReadMode
  dat <- hGetContents h
  let mat = read dat
  return $ listArray ((0,0),(79,79)) $ concat mat
\end{code}

\begin{code}
edgeCost :: Array (Int,Int) Integer -> EdgeCost
edgeCost arr = array ((0,0),(n,n)) [((i,j),cost i j) | i <- [0..n], j <-[0..n]]
    where
      w = y1 - y0 + 1
      h = x1 - x0 + 1
      n = w * h
      ((x0,y0),(x1,y1)) = bounds arr
      cost i j
          | i == (n-1) && j == n = arr!(x1,y1)
          | i == j = 0
          | i+1 == j = if (i `mod` w) == (w - 1) 
                       then infinite 
                       else arr!(i`div`w, i`mod`w)
          | i+w == j = if (i `div` w) == (h - 1)
                       then infinite
                       else arr!(i`div`w, i`mod`w)
          | otherwise = infinite
\end{code}

\begin{code}
update :: EdgeCost -> Int -> EdgeCost
update arr k = array (bounds arr) 
               [((i,j), min' (arr!(i,j)) ((arr!(i,k)) `sum'` (arr!(k,j)))) | (i,j) <- indices arr]
\end{code}

\begin{code}
update' arr k n
    | k == n = arr
    | otherwise = update' (update arr k) (k+1) n
\end{code}

\begin{code}
min' (-1) b = b
min' a (-1) = a
min' a b = min a b
\end{code}

\begin{code}
sum' (-1) _ = -1
sum' _ (-1) = -1
sum' a b = a + b
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%