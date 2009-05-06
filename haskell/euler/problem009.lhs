\begin{code}
triplet = [(a,b,c) | a <- [1..1000], b <- [a..1000], c <- [b..1000], a+b+c == 1000, ]
\end{code}