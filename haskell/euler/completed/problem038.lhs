\begin{code}
import Euler( digits, toint )
import Data.List( sort )
\end{code}

\begin{code}
isPan a = (sort a) == [1..9]
\end{code}

\begin{code}
createpan v = createpan' v 2 (digits v)
createpan' v n acc
    | (length acc) >= 9 = (n-1,acc)
    | otherwise = createpan' v (n+1) (acc ++ digits (v * n))
\end{code}

\begin{code}
numbers = map snd $ filter ((>=2).fst) $ map createpan [1..10000]
\end{code}

\begin{code}
solution = maximum . map toint $ filter isPan numbers
\end{code}

\begin{code}
main = print solution
\end{code}