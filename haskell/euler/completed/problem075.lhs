It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L  1,500,000 can exactly one integer sided right angle triangle be formed?

Note: This problem has been changed recently, please check that you are using the right parameters.

\begin{code}
import Control.Monad( forM_ )
import Control.Monad.ST( ST )
import Data.Array( Array, elems )
import Data.Array.ST
    ( STArray, runSTArray, newArray, writeArray, readArray  )
\end{code}

\begin{code}
type Triplet = (Int,Int,Int)

\end{code}

\begin{code}
transU (a,b,c) = (a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c)
transA (a,b,c) = (a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c)
transD (a,b,c) = (2*b + 2*c - a, b + 2*c - 2*a, 2*b + 3*c - 2*a)
\end{code}

\begin{code}
primitiveTiplets n t@(a,b,c)
    | a + b + c > n = []
    | otherwise = t : primitiveU ++ primitiveA ++ primitiveD
    where
      primitiveU = primitiveTiplets n $ transU t
      primitiveA = primitiveTiplets n $ transA t
      primitiveD = primitiveTiplets n $ transD t
\end{code}

\begin{code}
generateTriplets t@(a,b,c) n p
    | ap + bp + cp > n = []
    | otherwise = (ap,bp,cp) : generateTriplets t n (p+1)
    where
     ap = a * p
     bp = b * p
     cp = c * p
\end{code}

\begin{code}
tripletLength (a,b,c) = a + b + c
\end{code}

\begin{code}
countArray :: Int ->  Array Int Int
countArray n = runSTArray $ do
                 arr <- newArray (1,n) 0 :: ST s (STArray s Int Int)
                 forM_ primsLengths $ \l ->
                     do
                       forM_ [l,2*l .. n] $ \i ->
                           do
                             c <- readArray arr i
                             writeArray arr i (c+1)
                 return arr
    where
      primsLengths = map tripletLength $ primitiveTiplets n (3,4,5)
\end{code}

\begin{code}
solution = length . filter (==1) . elems $ countArray 1500000
\end{code}

\begin{code}
main = print solution
\end{code}
