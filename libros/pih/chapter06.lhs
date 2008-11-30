\begin{code}
expe :: Integral a => a -> a -> a
expe _ 0 = 1
expe 0 _ = 0
expe n (k+1) = n * (expe n k)
\end{code}

\begin{code}
and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && (and1 xs)
\end{code}

\begin{code}
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ (concat xs)
\end{code}

\begin{code}
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 (n+1) a = a : (replicate1 n a)
\end{code}

\begin{code}
(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "indice"
(!!!) (x:_) 0 = x
(!!!) (x:xs) (n+1) = (!!!) xs n
\end{code}

\begin{code}
elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) 
      | a == x = True
      | otherwise = elem1 a xs
\end{code}

\begin{code}
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
    | x < y = x:(merge xs (y:ys))
    | x > y = y:(merge (x:xs) ys)
    | otherwise = x:y:(merge xs ys)
\end{code}

\begin{code}
halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
    where mid = (length xs) `div` 2
\end{code}

\begin{code}
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (xs) = merge (msort x1s) (msort x2s)
    where (x1s, x2s) = halve xs
\end{code}

\begin{code}
sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs
\end{code}

\begin{code}
take1 :: Int -> [a] -> [a]
take1 0 xs = []
take1 (n+1) [] = []
take1 (n+1) (x:xs) = x : take1 n xs
\end{code}

\begin{code}
last1 :: [a] -> a
last1 [x] = x
last1 (x:xs) = last1 xs
\end{code}
