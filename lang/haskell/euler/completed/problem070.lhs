Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1  n  1^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.


\begin{code}
import Euler( primes )
import Data.List( sort, minimumBy, (\\) )
import Control.Arrow( (&&&), (***) )
import Data.Ord( comparing )
import Data.Ratio( (%) )
\end{code}

phi(n)=n(1-1/p1)(1-1/p2)...(1-1/pk)

n/phi(n)   minimo   si n es primo -> n/phi(n) = n/(n-1) 
pero n-1 no es permutacion de n nunca. Lo siguiente minimo es n multiplo
de dos primos.

si n=p1*p2   (p1!=p2)  phi(n) = (p1-1)*(p2-1)

\begin{code}
totient p1 p2 = (p1-1)*(p2-1)
\end{code}

\begin{code}
limit :: Int
limit = 10^7
limit_p = round $ sqrt (fromIntegral limit)
\end{code}

\begin{code}
pairs = [(a,b) | a <- mprimes, b <- mprimes, a /= b, (a*b) < limit]
mprimes = takeWhile (<= 4000) primes
\end{code}

\begin{code}
isPermutation a b = null $ (show a) \\ (show b)
\end{code}

\begin{code}
numbers = filter (uncurry isPermutation) . map ((uncurry (*)) &&& (uncurry totient)) $ pairs
\end{code}

\begin{code}
solution = minimumBy (comparing $ (uncurry (/)). (fromIntegral *** fromIntegral)) numbers
\end{code}

\begin{code}
main = print $ fst solution
\end{code}
