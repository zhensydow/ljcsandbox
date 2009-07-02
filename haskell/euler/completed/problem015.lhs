\begin{code}
pascalTriangle n 0 = 1
pascalTriangle n 1 = n
pascalTriangle n k
    | n == k = 1
    | otherwise = ((pascalTriangle (n-1) (k-1))*n) `div` k 
\end{code}

\begin{code}
solution = pascalTriangle (2*20) 20
\end{code}
