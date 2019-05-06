\begin{code}
import Euler( digits )
import Data.Ratio( (%), denominator )
\end{code}

\begin{code}
reduce :: Int -> Int -> Bool
reduce a b
    | d1a == d2b && (d1b /= 0 ) = (a % b) == (d2a % d1b)
    | d2a == d1b && (d2b /= 0 ) = (a % b) == (d1a % d2b)
    | otherwise = False
    where [d1a,d2a] = digits a
          [d1b,d2b] = digits b
\end{code}

\begin{code}
numbers = [(a,b) | a <- [10..99], b <- [10..99], reduce a b, a % b < 1]
\end{code}

\begin{code}
gdc :: Int -> Int -> Int
gdc a 0 = a
gdc a b = gdc b (a `mod` b)
\end{code}

\begin{code}
solution = denominator . product $ map (uncurry (%)) numbers
\end{code}

\begin{code}
main = print solution
\end{code}