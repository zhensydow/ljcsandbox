\begin{code}
digits n = reverse $ digits' n
    where digits' n
              | n < 10 = [n]
              | otherwise = n `mod` 10 : (digits' $ n `div` 10)
\end{code}

\begin{code}
fib = 1:1:zipWith (+) fib (tail fib)
\end{code}

\begin{code}
solution = head $ dropWhile (\(a,b)->b<1000) $ zip [1..] $ map (length.digits) fib
\end{code}