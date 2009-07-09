\begin{code}
import Data.Set( member, fromList )
import Data.List( sort )
\end{code}

\begin{code}
pent n = (n * ((3*n) - 1)) `div` 2
\end{code}

\begin{code}
pentlist = [pent n | n <- [1..10000]]
\end{code}

\begin{code}
ispent n = n `member` fromList pentlist
\end{code}

\begin{code}
pentpairs = [ i-j | i <- pentlist
            , j <- takeWhile (<i) pentlist
            , ispent (i+j)
            , ispent (i-j)]
\end{code}

\begin{code}
solution = head . sort $ pentpairs
\end{code}

\begin{code}
main = print solution
\end{code}
