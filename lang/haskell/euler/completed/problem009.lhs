\begin{code}
s = 1000
la = (s `div` 3) + 1
\end{code}

\begin{code}
triplet = [(a,b,c) | a <- [1..la], b <- [a..((s-a)`div`2)], c <- [s-a-b], a*a + b*b == c*c]
\end{code}

\begin{code}
solution = a*b*c
    where 
      (a,b,c) = head triplet
\end{code}

\begin{code}
main = print solution
\end{code}
