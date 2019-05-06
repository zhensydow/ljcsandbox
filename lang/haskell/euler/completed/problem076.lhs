\begin{code}
generalizedPentagonals :: [Int]
generalizedPentagonals = map fpent generators
generators :: [Int]
generators = 0 : (zipWith (*) (cycle [1,-1]) $ concatMap (replicate 2) [1..])
fpent :: Int -> Int
fpent n = round $ (n'*(3*n'-1)) / 2
    where n' = fromIntegral n
\end{code}

\begin{code}
calculaP :: Integer -> Integer
calculaP n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = sum $ concatMap (frag n) [1..n]
\end{code}

\begin{code}
testP n = concatMap (frag n) [1..n]
\end{code}

\begin{code}
frag :: Integer -> Integer -> [Integer]
frag n k = fmap (signo*) [(calculaP k1), (calculaP k2)]
    where k1 = n - round (k'*(3*k' - 1) / 2)
          k2 = n - round (k'*(3*k' + 1) / 2)
          signo = (-1)^(k-1)
          k' = fromIntegral k
\end{code}

\begin{code}
build n = (map sum (zipWith drop [0..] n) ++ [1]) : n
\end{code}

\begin{code}
p100  = (sum $ head $ iterate build [] !! 100) - 1
\end{code}

\begin{code}
main = print p100
\end{code}