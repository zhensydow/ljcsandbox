It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

\begin{code}
import Euler( isSquare, isIntegral )
import Data.List( nub, sort, group )
import Data.Set( fromList, member )
\end{code}

\begin{code}
data Triangle = T { 
  a :: !Integer, b :: !Integer, c :: !Integer }
  deriving( Show )
\end{code}

\begin{code}
perimeter :: Triangle -> Integer
perimeter t = (a t) + (b t) + (c t)
\end{code}

\begin{code}
area :: Triangle -> Double
area t = sqrt( s*(s - at)*(s - bt)*(s - ct))
    where
      s = (fromIntegral $ perimeter t) / 2
      at = (fromIntegral . a) t
      bt = (fromIntegral . b) t
      ct = (fromIntegral . c) t
\end{code}

\begin{code}
internal t = 4*bt*bt - at*at
    where
      at = a t
      bt = b t
\end{code}

\begin{code}
limit = 1000000000
sqLimit = round . sqrt . fromIntegral $ limit
\end{code}

\begin{code}
posiblesAes = 3 : 6 : [m*m + off | m <- [3..sqLimit], off <- [-2,2]]
posibles = [t | a <- posiblesAes, b <- [a-1,a,a+1]
           , a + 2*b < limit
           , isSquare (4*b*b - a*a)
           , let t = T a b b ]
\end{code}

\begin{code}
solution = length posibles
\end{code}

\begin{code}
main = print solution
\end{code}
