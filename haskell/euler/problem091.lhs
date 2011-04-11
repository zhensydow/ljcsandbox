The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O(0,0), to form ΔOPQ.

There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0  x1, y1, x2, y2  2.

Given that 0  x1, y1, x2, y2  50, how many right triangles can be formed?

\begin{code}
import Data.List( sort )
\end{code}

\begin{code}
data Triangle = TR 
    { x0 :: !Int
    , y0 :: !Int
    , x1 :: !Int
    , y1 :: !Int
    , x2 :: !Int
    , y2 :: !Int }
    deriving( Show )
\end{code}

\begin{code}
posibles l = [TR 0 0 x1 y1 x2 y2 | x1 <- [0..l], x2 <-[0..l], y1 <- [0..l], y2 <- [0..l], (x1,y1) /= (0,0), (x2,y2) /= (0,0), (x1,y1) /= (x2,y2)]
\end{code}

\begin{code}
checkRigth tr = (abs ((c*c) - (b*b) - (a*a))) < 0.000001 
    where
      tx0 = x0 tr
      tx1 = x1 tr
      tx2 = x2 tr
      ty0 = y0 tr
      ty1 = y1 tr
      ty2 = y2 tr
      a' = sqrt.fromIntegral $ (tx1*tx1 + ty1*ty1)
      b' = sqrt.fromIntegral $ (tx2*tx2 + ty2*ty2)
      tx12 = tx1 - tx2
      ty12 = ty1 - ty2
      c' = sqrt. fromIntegral $ (tx12*tx12 + ty12*ty12)
      [a,b,c] = sort [a', b', c']
\end{code}
