\begin{code}
seque 1 = [1]
seque n 
    | even n = n : seque ( n `div` 2 )
    | otherwise = n : seque ( (n * 3) + 1 )
\end{code}

\begin{code}
getMax x1 [] = x1
getMax x1@(a1,b1) (x2@(a2,b2):xs)
    | b2 > b1 = getMax x2 xs
    | otherwise = getMax x1 xs
\end{code}
