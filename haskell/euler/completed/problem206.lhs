\begin{code}
import Euler( digits, toint )
import Data.List( intersperse )
\end{code}

\begin{code}
extrae :: Integer -> String
extrae = extrae' 0 . show
extrae' _ [] = []
extrae' n (x:xs)
    | even n = x : extrae' (n+1) xs
    | otherwise = extrae' (n+1) xs
\end{code}

\begin{code}
maska = "1234567890"
maski = [1..9]++[0]
\end{code}

\begin{code}
check n = maska == ( extrae n )
\end{code}

\begin{code}
minval = floor . sqrt . fromInteger . toint $ intersperse 0 maski
\end{code}

\begin{code}
maxval = ceiling . sqrt . fromInteger . toint $ intersperse 9 maski
\end{code}

\begin{code}
solution = head $ filter (check.(^2)) [minval..maxval]
\end{code}

\begin{code}
main = print solution
\end{code}