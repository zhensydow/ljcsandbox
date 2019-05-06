\begin{code}
import Control.Arrow( (&&&) )
import Euler( digits )
\end{code}

\begin{code}
pairs n = filter (\(a,b)->a==b) $ map (id &&& length . digits . (n^)) [1..100]
\end{code}

\begin{code}
solution = length $ concatMap pairs [1..9]
\end{code}