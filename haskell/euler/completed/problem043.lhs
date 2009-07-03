\begin{code}
selections :: [a] -> [(a,[a])]
selections [] = []
selections (x:xs) = (x, xs) : [(y, x:ys) | (y,ys) <- selections xs]
\end{code}

\begin{code}
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ y : zs | (y,ys) <- selections xs
                  , zs <- permutations ys ]
\end{code}

\begin{code}
listToInt :: [Integer] -> Integer
listToInt = foldl (\a b -> a * 10 + b) 0
\end{code}

\begin{code}
pandigits n = permutations [0..n]
\end{code}

\begin{code}
groupn n xss@(x:xs)
    | length xss == n = [xss]
    | otherwise = take n xss : groupn n xs
\end{code}

\begin{code}
substrings p = tail $ map listToInt $ groupn 3 p
\end{code}

\begin{code}
checkprop p = all (\(a,b)-> a `mod` b == 0) $ zip (substrings p) [2,3,5,7,11,13,17]
\end{code}

\begin{code}
allpansprops = map listToInt $ filter checkprop $ pandigits 9
\end{code}

\begin{code}
solution = sum allpansprops
\end{code}

\begin{code}
main = print solution
\end{code}
