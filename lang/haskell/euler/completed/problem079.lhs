\begin{code}
import System.IO
import qualified Data.Graph as G
import qualified Data.List as L
import Data.Char( digitToInt, intToDigit )
\end{code}

\begin{code}
listVertices :: [Int] -> [(Int,Int)]
listVertices [] = []
listVertices (x:[]) = []
listVertices (x:y:xs) = (x,y) : listVertices (y:xs)
\end{code}

\begin{code}
keyLog :: IO String
keyLog = readFile "keylog079.txt"
\end{code}

\begin{code}
digits txt = map digitToInt $ ['0'..'9'] `L.intersect` txt
\end{code}

\begin{code}
vertices txt = concatMap listVertices $ map (map digitToInt) $ lines txt
\end{code}

\begin{code}
road txt = G.topSort $ G.buildG (0,9) $ vertices txt
\end{code}

\begin{code}
solution = do
  txt <- keyLog
  return $ map intToDigit $ (road txt) `L.intersect` (digits txt)
\end{code}

\begin{code}
main = do
  sol <- solution
  putStrLn sol
\end{code}
