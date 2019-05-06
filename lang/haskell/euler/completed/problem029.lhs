\begin{code}
import Data.List
\end{code}

\begin{code}
combinations = [a^b|a<-[2..100],b<-[2..100]]
\end{code}

\begin{code}
solution = length $ nub combinations
\end{code}