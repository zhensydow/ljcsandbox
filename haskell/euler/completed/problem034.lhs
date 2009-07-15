\begin{code}
import Euler( digits )
\end{code}

\begin{code}
fact :: Int -> Int
fact n = [1,1,2,6,24,120,720,5040,40320,362880] !! n
\end{code}

\begin{code}
hash = sum . map fact . digits
\end{code}

\begin{code}
check n = n == hash n
\end{code}

\begin{code}
solution = sum $ filter check [10..1000000]
\end{code}

\begin{code}
main = print solution
\end{code}