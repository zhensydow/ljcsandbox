In the following equation x, y, and n are positive integers.

1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:

x = 5, y = 20 

x = 6, y = 12

x = 8, y = 8

What is the least value of n for which the number of distinct solutions exceeds one-thousand?

\begin{code}
import Euler( numDivisors, numDivisorsArray )
\end{code}

\begin{code}
eqcond n (x,y) = (x*n + y*n) == x*y
\end{code}

\begin{code}
eqsol :: Integer -> [(Integer,Integer)]
eqsol n = filter (eqcond n) . takeWhile (\(x,y)-> x <= y) $ pairs
    where 
      pairs = [(x, - (x*n) `div` (n - x)) | x <- [(n+1)..]]
\end{code}

\begin{code}
posibles = filter ((>100).length.eqsol) [4..]
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
solution = numDivisorsArray 9900000
\end{code}

\begin{code}
main = print solution
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
