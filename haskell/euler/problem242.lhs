\begin{code}
import Control.Arrow( (&&&) )
\end{code}

\begin{code}
import Data.List( tails )
\end{code}

\begin{code}
cnk :: Integer -> Integer -> Integer
cnk n 0 = 1
cnk n 1 = n
cnk n k 
    | n == k = 1
    | otherwise = ((cnk (n-1) (k-1)) * n ) `div` k
\end{code}

\begin{code}
oddcnk n 0 = True
oddcnk n 1 = odd n
oddcnk n k
    | n == k = True
    | n `mod` 10 == 1 = True
    | otherwise = (oddcnk (n-1) (k-1)) || (oddcnk (n-1) k)
\end{code}

\begin{code}
pascalrow n = [oddcnk n k | k <- [0..n]]
\end{code}

\begin{code}
firsttruerow n = fst . head $ filter (all id . snd) $ map (id &&& pascalrow ) [n..]
\end{code}

\begin{code}
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y:ks <- tails xs
                    , ys <- combinations (n-1) ks ]
\end{code}

\begin{code}
f n k = length $ filter (odd . sum) $ combinations k [1..n]
\end{code}

\begin{code}
oddf 1 1 = True
oddf 1 k = (k - 1) `mod` 4 == 0
oddf n k 
     | n == k = (k - 1) `mod` 4 == 0
     | otherwise = oddcnk n k
\end{code}

oddtriplets 20
[(1,1)  
,(5,1)  ,(5,5) 
,(9,1)  ,(9,9)   
,(13,1) ,(13,5) ,(13,9) ,(13,13)
,(17,1) ,(17,17) ]

\begin{code}
oddtriplets max = [ (n,k) | n <- [1,5..max]
              , k <- [1,5..n], oddf n k]
\end{code}

\begin{code}
sublist n = [(n,k) | k <- [5,9..(n-1)], oddf n k]
sumtriplets max = 1 + sum [2 + length xs | 
                           xs <- [ sublist n | n <- [5,9..max]]]
\end{code}

\begin{code}
solution = length $ oddtriplets 10000 --(10^12)
\end{code}

\begin{code}
main = print solution
\end{code}
