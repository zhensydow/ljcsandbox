\begin{code}
import Euler( digits )
\end{code}

\begin{code}
factorial 0 = 1
factorial m@(n+1) = m * factorial n
\end{code}

\begin{code}
solution = sum $ digits $ factorial 100
\end{code}

\begin{code}
main = print solution
\end{code}