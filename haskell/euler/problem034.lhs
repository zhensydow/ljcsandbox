\begin{code}
import Data.List (group)
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}

\end{code}

\begin{code}
check n = equals $ map (length.digits) [x*n| x<-[2..6]]
    where equals xs = (length.group) xs == 1
\end{code}

\begin{code}
solution = 1
\end{code}

\begin{code}
main = print solution
\end{code}