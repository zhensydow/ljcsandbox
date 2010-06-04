\begin{code}
import Euler( totient, primes )
import Data.List( sort, minimumBy, (\\) )
import Control.Arrow( (&&&) )
import Data.Ord( comparing )
import Data.Ratio( (%) )
\end{code}

\begin{code}
isPermutation a b = null $ (show a) \\ (show b)
\end{code}

\begin{code}
numbers = filter (uncurry isPermutation) . map (id &&& totient) $ [2..10^7]
\end{code}

\begin{code}
solution = minimumBy (comparing (uncurry (%))) numbers
\end{code}

\begin{code}
main = print solution
\end{code}
