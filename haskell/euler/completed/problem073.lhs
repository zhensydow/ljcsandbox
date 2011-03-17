Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d  12,000?

Note: The upper limit has been changed recently.

\begin{code}
import Data.Ratio( Ratio(..), (%), numerator, denominator )
import Data.List( nub )
\end{code}

\begin{code}
fractions d = [(a%b) | a <- [1..d], 
               let m = min (3*a) d,
               let l = 2*a,
               b <- [l..m] , a < b]
\end{code}

http://en.wikipedia.org/wiki/Farey_sequence

\begin{code}
mediant a b = (numerator a + numerator b) % (denominator a + denominator b)
\end{code}

\begin{code}
fareyList n a b
    | denominator m > n = []
    | otherwise = fareyList n a m ++ [m] ++ fareyList n m b
    where 
      m = mediant a b
\end{code}

\begin{code}
solution = length $ fareyList 12000 (1%3) (1%2)
\end{code}

\begin{code}
main = print solution
\end{code}