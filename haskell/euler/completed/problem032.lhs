\begin{code}
import Data.List( sort )
\end{code}

\begin{code}
products :: Integer -> [((Integer, Integer), Integer)]
products n = [((a,b),n) | a <- [1..lim]
             , let (b,r) = n `divMod` a, r == 0 ]
    where lim = toInteger $ (floor . sqrt $ fromInteger n) + 1
\end{code}

\begin{code}
checkIs9Pan ((a,b),c) = "123456789" == sort num
    where num = concat [show a, show b, show c]
\end{code}

\begin{code}
lastone = 9876543
--lastone = 10000
valids = concatMap (take 1 . filter checkIs9Pan . products) [123..lastone]
\end{code}

\begin{code}
solution = sum $ map snd valids
\end{code}

\begin{code}
main = print solution
\end{code}
