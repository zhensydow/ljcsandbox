\begin{code}
import Euler( digits )
import Control.Arrow( (&&&) )
import Data.Maybe( isJust )
import Data.List( sortBy )
import Data.Ord( comparing )
\end{code}

\begin{code}
findPower' _ 1 _ = Nothing
findPower' n base expo 
    | n == nn = Just expo
    | n < nn = Nothing
    | otherwise = findPower' n base (expo+1)
    where
      nn = base ^ expo

findPower n = findPower' n (sum $ digits n) 2
\end{code}

\begin{code}
powers = [ (n,(b,e)) | b <- [2..1000], e <- [2..10]
         , let n = b ^ e
         , (sum.digits) n == b]
\end{code}

\begin{code}
solution = last $ take 30 $ sortBy (comparing fst) $ powers
\end{code}

\begin{code}
main = print solution
\end{code}
