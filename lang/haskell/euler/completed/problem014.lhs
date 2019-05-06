\begin{code}
import qualified Data.HashTable as HT (new,insert,lookup,toList) 
import Data.Maybe (isJust,fromJust)
import Data.List (sortBy)
\end{code}

\begin{code}
nextSeq n
    | even n = n `div` 2
    | otherwise = (n*3) + 1
\end{code}

\begin{code}
seque 1 = [1]
seque n = n : (seque $ nextSeq n)
\end{code}

\begin{code}
getMax x1 [] = x1
getMax x1@(a1,b1) (x2@(a2,b2):xs)
    | b2 > b1 = getMax x2 xs
    | otherwise = getMax x1 xs
\end{code}

\begin{code}
sequen t d = do
  n <- HT.lookup t d
  if isJust n 
    then do
      return (fromJust n) 
    else do
      v <- sequen t (nextSeq d)
      HT.insert t d (v+1)
      return (v + 1)
\end{code}

\begin{code}
listGen t [] = return ()
listGen t (x:xs) = do
  sequen t x
  return ()
\end{code}

\begin{code}
test = do
  t <- HT.new ((==)::Integer->Integer->Bool) (fromInteger.id)

  HT.insert t 1 1

  mapM (sequen t) [1..1000000]
  
  l <- HT.toList t
  return $ head $ sortBy (\(a,b) (c,d)->compare d b) l
\end{code}

\begin{code}
solution = test
\end{code}

\begin{code}
main = do
  v <- solution
  print $ v
\end{code}
