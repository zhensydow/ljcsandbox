\begin{code}
import Data.List( sort )
\end{code}

\begin{code}
minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = [ z | z <- xs, z `notElem` ys ]
\end{code}

\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
products :: Integer -> [((Integer, Integer), Integer)]
products n = [((a,b),n) | a <- [1..lim]
             , b <- [n `div` a], a * b == n ]
    where lim = toInteger $ (floor . sqrt $ fromInteger n) + 1
\end{code}

\begin{code}
checkIs9Pan ((a,b),c) = [1..9] == sort num
    where num = concat [digits a, digits b, digits c]
\end{code}

\begin{code}
lastone = 9876543
--lastone = 10000
valids = concat $ map (take 1 . filter checkIs9Pan . products) [1..lastone]
\end{code}

\begin{code}
solution = sum $ map snd valids
\end{code}

\begin{code}
main = print solution
\end{code}
