\begin{code}
import Data.List( isPrefixOf, maximumBy )
import Control.Arrow
import Data.Ord
\end{code}

\begin{code}
restos k = restos' 1 k
restos' n k = modulo : restos' (modulo*10) k
    where 
      modulo = n `mod` k
\end{code}

Only valid for rationals with cycles, if the rational hasn't cycles, buscaLen return 1

\begin{code}
buscaLen n = 1 + (length $ takeWhile (/=r0) $ tail ys)
    where
      xs = restos n
      ys = drop (n-1) xs
      r0 = head ys
\end{code}

\begin{code}
buscaCycle n = takeWhile (/=r0) $ tail ys
    where
      xs = restos n
      ys = drop (n-1) xs
      r0 = head ys
\end{code}

\begin{code}
lengths = map (id &&& buscaLen) [2..1000]
solution = maximumBy (comparing snd) lengths
\end{code}

\begin{code}
main = print (fst solution)
\end{code}
