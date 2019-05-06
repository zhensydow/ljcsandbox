\begin{code}
import Euler(combinations)
import Control.Arrow( (&&&) )
import Data.List( group, sort, permutations, intersect )
\end{code}

\begin{code}
checkGonRing (x:xs) = checkGonRing' (x:xs) (x!!1)
checkGonRing' (x:[]) n = (x!!2) == n
checkGonRing' (x:y:xs) n = (x!!2) == (y!!1) && checkGonRing' (y:xs) n
\end{code}

\begin{code}
checkCount xs 
    | length groups < 10 = False
    | count10 /= 1 = False
    | otherwise = all ((<=2).snd) countE
    where
      groups = group . sort $ concat xs
      countE = map (head &&& length) groups
      count10 = snd $ last countE
\end{code}

\begin{code}
posibles5GonTris n = concatMap permutations . filter ((==n).sum) $ combinations 3 [1..10]
\end{code}

\begin{code}
posibles5Gon = filter checkCount . filter checkGonRing . combinations 5 . posibles5GonTris
\end{code}

\begin{code}
sumas5Gon = map (head &&& length) . group . sort . map sum $ combinations 3 [1..10]
\end{code}

\begin{code}
solution = concatMap posibles5Gon [13,14,15,16,17,18,19,20,21,22]
\end{code}

\begin{code}
main = print solution
\end{code}