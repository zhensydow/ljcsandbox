\begin{code}
import Euler( isSquare )
import Control.Arrow( (&&&) )
import Data.List( (\\), sortBy )
import Data.Ord( comparing )
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
continuedFraction d = (a0,(a0,1)) : continuedFraction' d a0 a1 a0 p1 1 a1 pp1 qq1
    where
      a0 = floor $ sqrt (fromIntegral d)
      a1 = (a0 + pp1) `div` qq1
      p1 = a0 * a1 + 1
      pp1 = a0
      qq1 = d - (a0^2)
\end{code}

\begin{code}
continuedFraction' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [(Integer,(Integer,Integer))]
continuedFraction' d a0 a1 p0 p1 q0 q1 pp1 qq1 = (a1,(p1,q1)) : continuedFraction' d a0 an p1 pn q1 qn ppn qqn
    where
      ppn = a1 * qq1 - pp1
      qqn = (d - (ppn^2)) `div` qq1
      an = (a0 + ppn) `div` qqn
      pn = an * p1 + p0
      qn = an * q1 + q0
\end{code}

\begin{code}
miniyo d = (p,q)
    where
      ((x,y):xs') = continuedFraction d
      lll = length $ takeWhile ((/=(2*x)).fst) xs'
      xs = take (lll+1) xs'
      xs2 = take (2*r + 1) xs'
      r = length xs - 1
      (p,q) 
          | r == 0 = (snd.last) xs
          | odd r = (snd.last.init) xs
          | otherwise = (snd.last) xs2
\end{code}

\begin{code}
dvals max = [2 .. max] \\ map (^2) [1..max]
\end{code}

\begin{code}
prop_check d = miniyo d == minimaly d
\end{code}

\begin{code}
prop_check_map d = map miniyo (dvals d) == map minimaly (dvals d)
\end{code}

\begin{code}
solution = last . sortBy (comparing (fst.snd)) . map (id &&& miniyo) $ dvals 1000
\end{code}
