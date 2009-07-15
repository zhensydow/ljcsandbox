\begin{code}
import Data.List( sort )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
fibs = 1:1:zipWith (+) fibs (tail fibs)
\end{code}

\begin{code}
fib n = fibs !! (n-1)
\end{code}

\begin{code}
ispandigital a = (sort a) == "123456789"
\end{code}

\begin{code}
pansr,pansl :: Integer -> Bool
pansr n = ispandigital . show $ n `mod` 1000000000
pansl = ispandigital . take 9 . show
\end{code}

\begin{code}
pans n = ispandigital a && ispandigital b
    where a = show $ n `mod` 1000000000
          b = take 9 $ show n
\end{code}

\begin{code}
first = 1
solution =  fst . head . filter (pans.snd) $ zip [1..] fibs
\end{code}

\begin{code}
main = print solution
\end{code}
