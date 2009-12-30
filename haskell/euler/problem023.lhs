\begin{code}
import Euler( totient )
\end{code}

\begin{code}
isAbundant n = totient n - n > n
\end{code}