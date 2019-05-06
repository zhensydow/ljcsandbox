\begin{code}
import Data.List( subsequences, minimumBy, inits )
import Control.Arrow( (&&&) )
import Data.Ord( comparing )
\end{code}

\begin{code}
consecutive [] = True
consecutive (x:[]) = True
consecutive (x:y:xs) 
    | y == (x+1) = consecutive (y:xs)
    | otherwise = False
\end{code}

\begin{code}
dist a b = abs $ (numrects a) * (numrects b) - 2000000
\end{code}

\begin{code}
rects n = combs [1..n]
combs [] = []
combs xss@(x:xs) = (tail . inits $ xss) ++ combs xs
\end{code}

\begin{code}
numrects = length . rects
\end{code}

\begin{code}
solution = minimumBy (comparing snd) $ map (id &&& (uncurry dist)) [(a,b) | a <- [1..100], b <- [1..100]]
\end{code}

\begin{code}
main = print solution
\end{code}