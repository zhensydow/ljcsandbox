\begin{code}
import Data.Ratio( Ratio(..), (%) )
import Euler( fact )
\end{code}

\begin{code}
binom :: Integer -> Integer -> Ratio Integer
binom n 0 = 1
binom 0 k = 0
binom n k = (binom (n-1) (k-1)) * (n % k)
\end{code}

\begin{code}
launchs6x6 = [a*b*c*d*e*f | a <- [1..6], b <- [1..6], c <- [1..6]
             , d <- [1..6], e <- [1..6], f <- [1..6] ]
\end{code}

\begin{code}
launchs4x9 = [a*b*c*d*e*f*g*h*i | a <- [1..4], b <- [1..4], c <- [1..4]
             , d <- [1..4], e <- [1..4], f <- [1..4]
             , g <- [1..4], h <- [1..4], i <- [1..4] ]
\end{code}

\begin{code}
wins = [(a,b) | a <- launchs4x9, b <- launchs6x6, a > b ]
\end{code}
