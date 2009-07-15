\begin{code}
import Euler( digits )
\end{code}

\begin{code}
fib = 1:1:zipWith (+) fib (tail fib)
\end{code}

\begin{code}
solution = head $ dropWhile ((<1000).snd) $ zip [1..] $ map (length.digits) fib
\end{code}

\begin{code}
main = print solution
\end{code}