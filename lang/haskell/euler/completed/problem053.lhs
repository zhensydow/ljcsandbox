\begin{code}
fact 0 = 1
fact n = product [1..n]
\end{code}

\begin{code}
combinatory _ 0 = 1
combinatory a 1 = a
combinatory n r 
    | n < r = error "invalid"
    | n == r = 1
    | otherwise = product [(r+1)..n] / (fact (n - r))
\end{code}

\begin{code}
numbers = [ combinatory n r | n <- [1..100], r <- [1..n] ]
\end{code}

\begin{code}
solution = length $ filter (>1000000) numbers
\end{code}