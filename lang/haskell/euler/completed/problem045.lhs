\begin{code}
triangle n = (n * (n+1)) `div` 2
\end{code}

\begin{code}
pentagonal n = (n * ((3*n) - 1)) `div` 2
\end{code}

\begin{code}
hexagonal n = n * ((2*n) - 1)
\end{code}

\begin{code}
invpentagonal n = floor $ ((sqrt sqn) + (1/2)) / 3
    where rn = fromInteger n
          sqn = (1/4) + 6*rn
\end{code}

\begin{code}
invhexagonal n = floor $ ((sqrt sqn) + 1) / 4
    where rn = fromInteger n
          sqn = 1 + 8*rn
\end{code}

\begin{code}
solution = triangle t
    where (t,_,_) = head $ [ (a,b,c) | a <- [286..]
                           , b <- [invpentagonal $ triangle a]
                           , c <- [invhexagonal $ triangle a]
                           , (triangle a) == (pentagonal b)
                           , (triangle a) == (hexagonal c)]
\end{code}