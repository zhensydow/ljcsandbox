%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Find the number of integers 1  n  107, for which n and n + 1 have the same 
number of positive divisors. For example, 14 has the positive divisors 
1, 2, 7, 14 while 15 has 1, 3, 5, 15.

\begin{code}
import Euler( sigma0, sigma1, isPrime, numDivisorsArray )
import Data.Array.Unboxed( elems )
\end{code}

\begin{code}
nextNum n
    | isPrime (n+1) = nextNum (n+1)
    | isPrime (n+2) = nextNum (n+2)
    | otherwise = n+1
\end{code}

\begin{code}
check n = sigma0 n == sigma0 (n + 1)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
limit = 10 ^ 7
nums = drop 1 $ elems $ numDivisorsArray limit
\end{code}

\begin{code}
solution = length . filter id $ zipWith (==) nums (tail nums)
\end{code}

\begin{code}
main = print solution
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
