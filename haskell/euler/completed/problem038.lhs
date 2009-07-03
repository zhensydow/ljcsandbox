\begin{code}
import Data.List( sort )
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
listToInt :: [Integer] -> Integer
listToInt = foldl (\a b -> a * 10 + b) 0
\end{code}

\begin{code}
isPan a = (sort a) == [1..9]
\end{code}

\begin{code}
hasRepeated _ = True
\end{code}

\begin{code}
createpan v = createpan' v 2 (digits v)
createpan' v n acc
    | (length acc) >= 9 = (n-1,acc)
    | otherwise = createpan' v (n+1) (acc ++ digits (v * n))
\end{code}

\begin{code}
numbers = map snd $ filter ((>=2).fst) $ map createpan [1..10000]
\end{code}

\begin{code}
solution = maximum $ map listToInt $ filter isPan numbers
\end{code}
