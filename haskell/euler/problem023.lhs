\begin{code}
divisors n = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]
\end{code}

\begin{code}
isAbundant n = (sum (divisors n)) > n
\end{code}

\begin{code}
abundants = filter isAbundant [12..]
\end{code}

\begin{code}
sumandos n = [(x,n-x)| x <- takeWhile (<n) abundants, isAbundant (n-x)]
\end{code}

\begin{code}
notAbun = map fst $ filter (null.snd) $ zip base $ map sumandos base
    where base = [24..28123]
\end{code}

\begin{code}
solution = sum notAbun
\end{code}

\begin{code}
main = print solution
\end{code}