\begin{code}
import qualified Data.Set as S
import Euler( digits, toint )
\end{code}

\begin{code}
noLychrels :: S.Set Integer
noLychrels = foldl S.union (S.fromList [1..195]) 
             $ map S.singleton noLychrels'
    where noLychrels' = filter notLychrels [196..10000]
\end{code}

\begin{code}
iterations :: Integer -> [Integer]
iterations n = iterations' n 0

iterations' n 50 = []
iterations' n k 
    | isPalindrome = [n,nn]
    | otherwise = n : iterations' nn (k+1)
    where 
      nn = n + rev n
      isPalindrome = rev nn == nn
\end{code}

\begin{code}
rev = toint . reverse . digits
\end{code}

\begin{code}
notLychrels n = rev (last xs) == (last xs)
    where xs = iterations n
\end{code}

\begin{code}
isLychrel n = not $ n `S.member` noLychrels
\end{code}

\begin{code}
solution = length $ filter isLychrel [1..10000]
\end{code}

\begin{code}
main = print solution
\end{code}
