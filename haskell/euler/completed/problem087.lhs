The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:

28 = 22 + 23 + 24
33 = 32 + 23 + 24
49 = 52 + 23 + 24
47 = 22 + 33 + 24

How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?

\begin{code}
import Euler( primes )
import Data.List( group,sort )
\end{code}

\begin{code}
primes2 = takeWhile (<7075) primes
primes3 = takeWhile (<369) primes
primes4 = takeWhile (<85) primes
\end{code}

\begin{code}
sumes = filter (<50000000) [ m | x <- primes2, y <- primes3, z <- primes4
                              , let m = x^2 + y^3 + z^4 ]
\end{code}

\begin{code}
solution = length . group . sort $ sumes
\end{code}

\begin{code}
main = print solution
\end{code}
