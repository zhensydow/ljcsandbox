\begin{code}
divisors2 n = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]

divisors n 
    | odd n = 1:[x | x <- [3,5..(n `div` 2)], n `mod` x == 0]
    | otherwise = [ x | x <- [1..(n `div` 2)], n `mod` x == 0]
\end{code}

\begin{code}
iterateTriangle = iterateTriangle' 1 1
iterateTriangle' c n 
    | divs < 499 = iterateTriangle' (c+1) (n+c+1)
    | otherwise = n
    where divs = length $ divisors n
\end{code}

\begin{code}
solution = iterateTriangle
\end{code}

\begin{code}
main = print solution
\end{code}