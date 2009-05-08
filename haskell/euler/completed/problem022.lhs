\begin{code}
import System.IO (openFile, hClose, hGetContents, IOMode(..))
import Data.Char (ord)
\end{code}

\begin{code}
ordinal l = (ord l) - (ord 'A') + 1
\end{code}

\begin{code}
value = sum . (map ordinal)
\end{code}

\begin{code}
solution names = sum $ zipWith (*) [1..] $ map value names
\end{code}

\begin{code}
main = do
  h <- openFile "problem022.txt" ReadMode

  file <- hGetContents h

  
  print $  solution $ lines file

  hClose h

  return ()
\end{code}
