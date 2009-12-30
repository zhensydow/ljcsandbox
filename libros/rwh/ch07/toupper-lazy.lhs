\begin{code}
import Data.Char( toUpper )
\end{code}

\begin{code}
main = interact $ (++) "Data is:\n\n" . map toUpper
\end{code}