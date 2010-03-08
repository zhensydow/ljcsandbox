\begin{code}
import Euler( sigma1 )
\end{code}

A048242		 Numbers that are not the sum of two abundant numbers (not necessarily distinct).

\begin{code}
isAbundant n = sigma1 n > 2*n
\end{code}

\begin{code}
abundants = filter isAbundant [1..20161]
\end{code}

\begin{code}
sumAbundants n = length sums > 0
    where sums = [ y | y <- [1..(n-1)]
                 , y `elem` abundants, (n-y) `elem` abundants]
\end{code}

\begin{code}
notSumAbundants = filter (not.sumAbundants) [2..20161]
\end{code}

\begin{code}
solution = sum notSumAbundants
\end{code}

\begin{code}
main = print solution
\end{code}