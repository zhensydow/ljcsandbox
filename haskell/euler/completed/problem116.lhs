A row of five black square tiles is to have a number of its tiles replaced with coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).

If red tiles are chosen there are exactly seven ways this can be done.

2 1 1 1,  1 2 1 1,  1 1 2 1,  1 1 1 2
2 2 1,  2 1 2,  1 2 2

 
If green tiles are chosen there are three ways.

3 1 1,  1 3 1,  1 1 3			

		 
And if blue tiles are chosen there are two ways.

4 1,  1 4

	
Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of replacing the black tiles in a row measuring five units in length.

How many different ways can the black tiles in a row measuring fifty units in length be replaced if colours cannot be mixed and at least one coloured tile must be used?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
import Euler( insertInto )
import Data.List( group, nub )
\end{code}
            tile size
row size     2   3   4

5            7   3   2  =  12
6           12   5   3  =  20
7           20   8   4  =  32
8           33  12   6  =  51
9           54  18   9  =  81
10          88  27  13  = 128
11         143  40  18  = 
12         232  59  25  =       
\begin{code}
ways s t = concatMap (\k-> insertInto k t (replicate (s - k*t) 1)) $ [1 .. s `div` t]
\end{code}

ways 50 2 == 

+RTS -A16384 -H8388608
ways 30 3 == 58424           1''   ->  1''
+RTS -A536870912 -H262144
ways 40 3 == 2670963         5''   ->  3''
ways 50 3 == 122106096    3'37''

+RTS -A67108864 -H524288
ways 50 4 == 5453760        10''   ->  7''

\begin{code}
solution' = length $ ways 50 3
\end{code}

\begin{code}
ways3 n = length $ ways n 3
\end{code}

http://oeis.org/A098578

\begin{code}
a098578 = 0:1:2:3:zipWith (\a b-> a + b + 1) a098578 (drop 3 a098578)
ways4 n = a098578 !! (n-3)
\end{code}

http://oeis.org/A000071

\begin{code}
a000071 = 0:0:zipWith (\a b-> a + b + 1) a000071 (drop 1 a000071)
ways2 n = a000071 !! n
\end{code}

\begin{code}
solution = ways2 50 + (fromIntegral . length $ ways3 50) + ways4 50
\end{code}

\begin{code}
main = print solution
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
