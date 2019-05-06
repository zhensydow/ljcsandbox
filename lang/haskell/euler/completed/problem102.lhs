Three distinct points are plotted at random on a Cartesian plane, for which -1000  x, y  1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.

Using problem102.txt, a 27K text file containing the co-ordinates of one thousand "random" triangles, find the number of triangles for which the interior contains the origin.

\begin{code}
import Data.List.Split( splitOn )
\end{code}

\begin{code}
data Point = P { x :: ! Double, y :: ! Double }
             deriving( Eq, Show )

instance Num Point where
    p1 + p2 = P ((x p1) + (x p2)) ((y p1) + (y p2))
    p1 - p2 = P ((x p1) - (x p2)) ((y p1) - (y p2))
    p1 * p2 = P ((x p1) * (x p2)) ((y p1) * (y p2))
    abs p = P (abs . x $ p) (abs . y $ p)
    signum p = P (signum . x $ p) (signum . y $ p)
    fromInteger n = P (fromInteger n) (fromInteger n)
\end{code}

\begin{code}
data Triangle = Triangle { a :: Point, b :: Point, c :: Point }
                deriving( Show )
\end{code}

Barycentric Technique

\begin{code}
containsOrigin :: Triangle -> Bool
containsOrigin triangle = (u > 0) && (v > 0) && (u + v < 1)
    where
      u = (dot11 * dot02 - dot01 * dot12) * invDenom
      v = (dot00 * dot12 - dot01 * dot02) * invDenom
      invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
      dot00 = dot v0 v0
      dot01 = dot v0 v1 
      dot02 = dot v0 v2
      dot11 = dot v1 v1
      dot12 = dot v1 v2
      v0 = (c triangle) - (a triangle)
      v1 = (b triangle) - (a triangle)
      v2 = (P 0 0) - (a triangle)
\end{code}

\begin{code}
dot :: Point -> Point -> Double
dot p1 p2 = (x p1) * (x p2) + (y p1) * (y p2)
\end{code}

\begin{code}
mkTriangle ax ay bx by cx cy = Triangle a b c
    where 
      a = P ax ay
      b = P bx by
      c = P cx cy
\end{code}

\begin{code}
solution :: [Triangle] -> Int
solution = length . filter containsOrigin
\end{code}

\begin{code}
stringToDoubles :: String -> [Double]
stringToDoubles = map read . splitOn ","
\end{code}

\begin{code}
main = do
  contents <- readFile "problem102.txt"
  let tris = map (doublesToTriangle . stringToDoubles) $ lines contents
  print $ solution tris
  where 
    doublesToTriangle (ax:ay:bx:by:cx:cy:[]) = mkTriangle ax ay bx by cx cy
\end{code}
