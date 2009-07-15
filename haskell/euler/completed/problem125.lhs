\begin{code}
import Data.List( nub, inits )
\end{code}

\begin{code}
palin n = reverse xs == xs
    where xs = show n
\end{code}

\begin{code}
hi = 10^8
limit = toInteger . truncate . sqrt . fromInteger $ hi `div` 2
\end{code}

\begin{code}
sumsFrom i = takeWhile (<hi) . drop 2 . scanl (\a b -> a + b*b) 0 $ [i..]
\end{code}

\begin{code}
numbers = nub $ concatMap (filter palin . sumsFrom) [1..limit]
\end{code}

\begin{code}
solution = sum numbers
\end{code}

\begin{code}
main = print solution
\end{code}
