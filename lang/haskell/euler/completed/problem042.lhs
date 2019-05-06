\begin{code}
import Data.Char( ord )
import Data.List.Split( splitOn )
\end{code}

\begin{code}
triangle n = (n * (n+1)) `div` 2
\end{code}

\begin{code}
invtriangle n = floor $ ((sqrt sqn) - (1/2))
    where rn = fromInteger n
          sqn = (1/4) + 2*rn
\end{code}

\begin{code}
number :: Char -> Integer
number a = toInteger $ (ord a) - (ord 'A') + 1
\end{code}

\begin{code}
hashword :: String -> Integer
hashword = sum . map number
\end{code}

\begin{code}
checktriangle n = (triangle.invtriangle $ n) == n
\end{code}

\begin{code}
solution = length . filter checktriangle . map hashword
\end{code}

\begin{code}
main = do
  file <- readFile "problem042.txt"
  words <- return $ splitOn "," file
  print $ solution words
\end{code}