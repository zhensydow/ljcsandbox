\begin{code}
fractions = filter (\(a,b) -> (length $ show a) > (length $ show b) )
            $ filter (\(a,_)-> head (show a) == '1') $ take 1000 iters
\end{code}

\begin{code}
iters = (3,2) : map incrementa iters
\end{code}

\begin{code}
incrementa (n,d) = (2*d+n,d+n)
\end{code}

\begin{code}
main = print $ length fractions
\end{code}
