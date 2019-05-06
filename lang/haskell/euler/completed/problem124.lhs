The radical of n, rad(n), is the product of distinct prime factors of n. For example, 504 = 23  32  7, so rad(504) = 2  3  7 = 42.

If we calculate rad(n) for 1  n  10, then sort them on rad(n), and sorting on n if the radical values are equal, we get:

Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and E(6) = 9.

If rad(n) is sorted for 1  n  100000, find E(10000).

\begin{code}
import Euler( primeDecomp )
import Data.List( foldl', sortBy )
import Control.Arrow( (&&&) )
import Data.Ord( comparing )
\end{code}

\begin{code}
rad = foldl' (*) 1 . map fst . primeDecomp
\end{code}

\begin{code}
elements limit = sortBy (comparing snd) . map (id &&& rad) $ [1..limit]
\end{code}

\begin{code}
solution = (elements 100000) !! 9999
\end{code}

\begin{code}
main = print solution
\end{code}