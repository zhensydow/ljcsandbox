\begin{code}
import Data.Char( digitToInt )
\end{code}

\begin{code}
digits :: Int -> [Int]
digits = map digitToInt . show
\end{code}

\begin{code}
hash = sum . map (^5) . digits
\end{code}

\begin{code}
check n = (hash n) == n
\end{code}

\begin{code}
solution = sum $ filter check [2..1000000]
\end{code}

\begin{code}
main = print solution
\end{code}