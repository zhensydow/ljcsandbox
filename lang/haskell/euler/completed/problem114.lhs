A row measuring seven units in length has red blocks with a minimum length of three units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square. There are exactly seventeen ways of doing this.

1,1,1,1,1,1,1    3,1,1,1,1    1,3,1,1,1
1,1,3,1,1    1,1,1,3,1    1,1,1,1,3
3,1,3    4,1,1,1    1,4,1,1
1,1,4,1    1,1,1,4    5,1,1
1,5,1    1,1,5    6,1
1,6    7

How many ways can a row measuring fifty units in length be filled?

NOTE: Although the example above does not lend itself to the possibility, in general it is permitted to mix block sizes. For example, on a row measuring eight units in length you could use red (3), black (1), and red (4).

\begin{code}
import Euler()
\end{code}

4-row   ->  4
1,1,1,1    3,1    1,3    4

5-row   ->  7
1,1,1,1,1    3,1,1    1,3,1    1,1,3
4,1    1,4    5

6-row   -> 11
1,1,1,1,1,1,1    3,1,1,1      1,3,1,1    1,1,3,1
1,1,1,3    4,1,1    1,4,1    1,1,4
5,1    1,5    6

7-row   -> 17

8-row   -> 27
1,1,1,1,1,1,1,1    3,1,1,1,1,1    1,3,1,1,1,1    1,1,3,1,1,1
1,1,1,3,1,1    1,1,1,1,3,1    1,1,1,1,1,3    3,1,3,1
3,1,1,3    1,3,1,3    3,1,4    4,1,1,1,1
1,4,1,1,1    1,1,4,1,1    1,1,1,4,1    1,1,1,1,4
4,1,3    5,1,1,1    1,5,1,1    1,1,5,1
1,1,1,5    6,1,1    1,6,1    1,1,6
7,1    1,7    8


A005252

\begin{code}
a005252 = 1:1:1:1:zipWith3 (\a b c -> 2*c - b + a) a005252 (drop 2 a005252) (drop 3 a005252)
\end{code}

\begin{code}
solution = a005252 !! 51
\end{code}
