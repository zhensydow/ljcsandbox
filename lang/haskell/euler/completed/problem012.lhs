\begin{code}
import Control.Arrow( (&&&) )
import Euler( primeDecomp )
\end{code}

\begin{code}
triangleNumber n = sum [1..n]
\end{code}

\begin{code}
numDivisors n = product $ map ((+1).snd) $ primeDecomp n
\end{code}

\begin{code}
triangles = map (triangleNumber &&& (numDivisors . triangleNumber)) [1..]
\end{code}

\begin{code}
limit = 500
solution = fst . head $ dropWhile ((<limit).snd) triangles
\end{code}

\begin{code}
main = print solution
\end{code}
