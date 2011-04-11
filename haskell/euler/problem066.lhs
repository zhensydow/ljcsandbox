\begin{code}
import Euler( isSquare )
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
minimal :: Integer -> (Integer, Integer)
minimal d = head [(x,y) |
                   x <- [minx..]
                 , let maxy = ((x*x - 1) `div` d) + 1
                 , let miny = max (((round.sqrt.fromInteger) maxy) - 1) 1
                 , y <- [miny..maxy]
                 , x*x - d*y*y == 1]
    where
      minx = ((round.sqrt.fromInteger) (1 + d)) - 1
\end{code}

\begin{code}
minimaly :: Integer -> (Integer,Integer)
minimaly d = head [ (x,y) | 
                   y <- [1..]
                  , let rx = (sqrt.fromInteger) (1 + d*y*y)
                  , let x = round rx
                  , x*x - d*y*y == 1]
\end{code}

\begin{code}
dvals max = filter (not.isSquare) [2..max]
\end{code}