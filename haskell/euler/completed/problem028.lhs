\begin{code}
espiral 1 = [1]
espiral n = another ++ (take 4 $ drop 1 $ iterate (+(2*(n-1))) initial)
    where another = espiral (n-1)
          initial = last another
\end{code}

\begin{code}
tamEspiral n = (n + 1) `div` 2
\end{code}

\begin{code}
solution = sum $ espiral $ tamEspiral 1001
\end{code}