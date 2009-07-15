\begin{code}
import Data.List( sort, nub )
import Data.Ratio( Ratio, (%) )
import Euler( primeDecomp )
\end{code}

\begin{code}
divisors = init . genFromDecomp . primeDecomp
\end{code}

\begin{code}
genFromDecomp [] = [1]
genFromDecomp ((a,n):xs) = [ i*j | 
                            i <- [a ^ k | k <- [0..n]]
                           , j <- genFromDecomp xs ]
\end{code}

\begin{code}
isAbundant n = (sum (divisors n)) > n
\end{code}

\begin{code}
abundants = filter isAbundant [12..(28124 - 12)]
\end{code}

\begin{code}
sumAbundants = nub $ sort $ filter (<28124) [ a+b | a <- abundants, b <- abundants ]
\end{code}

\begin{code}
sumandos n = take 1 [(x,n-x)| x <- takeWhile (<n) abundants, isAbundant (n-x)]
\end{code}

20161 is the last number than can not be expressed as a sum of two bundant numbers (http://mathworld.wolfram.com/AbundantNumber.html)

\begin{code}
notAbun = [ x | x <- [20162,20161..24], (sumandos x) == [] ]
\end{code}

\begin{code}
solution = sum notAbun
\end{code}

\begin{code}
main = print solution
\end{code}