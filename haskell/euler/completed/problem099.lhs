\begin{code}
import Data.List( maximumBy )
import Data.Ord( comparing )
import Data.List.Split( splitOn )
\end{code}

\begin{code}
convertPair :: String -> (Integer, Integer)
convertPair xs = (a,b)
    where (a:b:[]) = map read $ splitOn "," xs
\end{code}

\begin{code}
hashpair (a,b) = (fromInteger b) * (log $ fromInteger a)
\end{code}

\begin{code}
solution = fst . maximumBy (comparing snd) . zip [1..] . map hashpair
\end{code}

\begin{code}
main = do
  file <- readFile "problem099.txt"
  lista <- return $ map convertPair $ lines file
  print $ solution $ lista
  return ()
\end{code}
