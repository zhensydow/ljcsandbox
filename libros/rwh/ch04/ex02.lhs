\begin{code}
import Data.Char( digitToInt )
\end{code}

\begin{code}
asInt_fold :: String -> Int
asInt_fold "" = error "empty string"
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl (\a b-> a*10 + (digitToInt b)) 0 xs
\end{code}
