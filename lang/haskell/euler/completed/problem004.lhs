\begin{code}
import Data.List( sortBy )
\end{code}

\begin{code}
palindromo xs = xs == reverse xs
\end{code}

\begin{code}
d3 = head $ filter (palindromo.show) $ sortBy (\a b -> compare b a) [x*y | x <- [999,998..100], y <- [999,998..x]]
\end{code}


\begin{code}
solution = d3
\end{code}