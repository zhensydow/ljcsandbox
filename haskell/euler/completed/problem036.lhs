\begin{code}
import Euler( digits )
\end{code}

\begin{code}
digitsbin n = reverse $ digitsbin' n
    where digitsbin' n
              | n < 2 = [n]
              | otherwise = n `mod` 2 : (digitsbin' $ n `div` 2)
\end{code}

\begin{code}
palin xs = xs == (reverse xs)
\end{code}

\begin{code}
numbers = filter (palin.digitsbin) $ filter (palin.digits) [1..1000000]
\end{code}

\begin{code}
solution = sum numbers
\end{code}

\begin{code}
main = print solution
\end{code}
