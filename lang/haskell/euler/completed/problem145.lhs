\begin{code}
import Euler( digits )
\end{code}

\begin{code}
sumlist [] [] c
    | c /= 0 = [c]
    | otherwise = []
sumlist (x:xs) (y:ys) c = s : sumlist xs ys r
    where (r,s) = (x+y+c) `divMod` 10
\end{code}

\begin{code}
reversible n = check && all odd suma
    where 
      suma = sumlist na nb 0
      na = digits n
      nb = dropWhile (==0) . reverse $ na
      check = (length na) == (length nb)
\end{code}

\begin{code}
solution = length . filter reversible $ [1..10^7]
\end{code}

\begin{code}
main = print solution
\end{code}