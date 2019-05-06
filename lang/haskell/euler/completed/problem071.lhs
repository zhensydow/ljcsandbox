\begin{code}
import Data.List( sortBy )
import Data.Ord( comparing )
\end{code}

\begin{code}
gdc :: Int -> Int -> Int
gdc a 0 = a
gdc a b = gdc b (a `mod` b)
\end{code}

\begin{code}
limitd = 1000000
\end{code}

\begin{code}
nmin d = floor $ (fromIntegral d) * (428/999)
nmax d = ceiling $ (fromIntegral d) * (3/7)
\end{code}

\begin{code}
minLim a b = ((fromIntegral a) / (fromIntegral b)) < (3/7)
\end{code}

\begin{code}
maxLim a b = ((fromIntegral a) / (fromIntegral b)) > (42857/100000)
\end{code}

\begin{code}
fracs = [ (n,d) | d <- [2..limitd], n <- [(nmin d)..(nmax d)]
        , minLim n d, maxLim n d, gdc n d == 1]
\end{code}

\begin{code}
sorted = sortBy (comparing (\(a,b)->(fromIntegral a) / (fromIntegral b))) fracs
\end{code}

\begin{code}
solution = last sorted
\end{code}

\begin{code}
main = print solution
\end{code}