\begin{code}
import Data.Char( digitToInt )
\end{code}

\begin{code}
digits :: Int -> [Int]
digits = map digitToInt . show
\end{code}

\begin{code}
next = sum . map (^2) . digits
\end{code}

\begin{code}
lastseq 1 = 1
lastseq 89 = 89
lastseq n = lastseq (next n)
\end{code}

\begin{code}
solution = length . filter (==89) . map lastseq $ [1..9999999]
\end{code}