The Caesar cipher

\begin{code}
import Data.Char
import Data.List
import Data.Maybe
\end{code}

\begin{code}
let2int c = ord c - ord 'a'
int2let n = chr $ ord 'a' + n
let2int' c = ord c - ord 'A'
int2let' n = chr $ ord 'A' + n
\end{code}

\begin{code}
shift n c
    | isLower c = int2let $ mod (let2int c + n) 26
    | isUpper c = int2let' $ mod (let2int' c + n) 26
    | otherwise = c
\end{code}

\begin{code}
encode n xs = map (shift n) xs
\end{code}

\begin{code}
count n = length . (filter (==n))
\end{code}

\begin{code}
lowers = length . (filter isLower)
\end{code}

\begin{code}
percent a b = (fromIntegral a / fromIntegral b) * 100
\end{code}

\begin{code}
freqs :: String -> [Float]
freqs xs = [percent (count x (map toLower xs)) n| x <- ['a' .. 'z']]
    where n = length $ filter isAlpha xs
\end{code}

\begin{code}
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]
\end{code}

\begin{code}
rotate n xs = drop n xs ++ take n xs
\end{code}

\begin{code}
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
\end{code}

\begin{code}
crack xs = encode (-factor) xs
    where
      table' = freqs xs
      chitab = [chisqr (rotate n table') table | n <- [0..25]]
      best = minimum chitab
      (Just factor) = findIndex (==best) chitab 
\end{code}

Exercises

\begin{code}
hundredIntegers = sum [x*x | x <- [1..100]]
\end{code}

\begin{code}
replicame n x = [x|y<-[1..n]]
\end{code}

\begin{code}
pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x + y*y == z*z]
\end{code}

\begin{code}
factors n = [x|x<-[1..(n-1)], mod n x == 0]
\end{code}

\begin{code}
perfects n = [x | x <- [1..n], (sum (factors x)) == x]
\end{code}

\begin{code}
ex551 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
ex552 = concat [[(x,y)| y <- [4,5,6]]| x <- [1,2,3]]
\end{code}

\begin{code}
findme k t = [v|(k',v) <- t, k==k']
\end{code}

\begin{code}
-- posiciones x xs = [i|(z,i)<-zip xs [0..n], x == z]
--                   where n = length xs - 1
posiciones x xs = findme x $ zip xs [0..n]
    where n = length xs - 1
\end{code}

\begin{code}
scalarproduct xs ys = sum [x*y| (x,y) <- zip xs ys]
\end{code}