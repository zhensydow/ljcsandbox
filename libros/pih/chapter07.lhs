\begin{code}
import Data.Char
\end{code}

\begin{code}
ex711 f p xs = [ f x | x <- xs, p x]
ex712 f p = (map f) . (filter p)
\end{code}

\begin{code}
all1 :: (a -> Bool) -> [a] -> Bool
all1 f = and . map f
\end{code}

\begin{code}
any1 :: (a -> Bool) -> [a] -> Bool
any1 f = or . map f
\end{code}

\begin{code}
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs)
    | f x = x : takeWhile f xs
    | otherwise = []
\end{code}

\begin{code}
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 f (x:xs)
    | f x = dropWhile f xs
    | otherwise = x:xs
\end{code}

\begin{code}
map1 f = foldr (\a b -> f a : b) []
\end{code}

\begin{code}
filter1 p = foldr (\a b -> if p a then a : b else b) []
\end{code}

\begin{code}
dec2int :: [Int] -> Int
dec2int = foldl (\a b-> b + 10*a) 0
\end{code}

\begin{code}
compose = foldr (.) id
\end{code}

\begin{code}
curry1 :: ((a,b) -> c) -> (a -> b -> c)
curry1 f = \a b-> f (a,b)
\end{code}

\begin{code}
uncurry1 :: (a -> b -> c) -> ((a,b) -> c)
uncurry1 f = \(a,b)-> f a b
\end{code}

\begin{code}
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)
\end{code}

\begin{code}
int2bin = unfold (==0) (`mod` 2) (`div` 2)
\end{code}

\begin{code}
chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)
chop9 = unfold null (take 9) (drop 9)
\end{code}

\begin{code}
map2 f = unfold null (f.head) tail
\end{code}

\begin{code}
iterate1 f = unfold (\a->False) id f
\end{code}

\begin{code}
make8 bits = take 8 (bits ++ repeat 0)
\end{code}

\begin{code}
parity xs = if check then xs++[1] else xs++[0]
    where check = (odd . length . filter (==1)) xs
\end{code}

\begin{code}
encode = concat . map (parity . make8 . int2bin . ord)
\end{code}

\begin{code}
bin2int = foldr (\a b-> a + 2*b) 0
\end{code}

\begin{code}
decode = map (chr . bin2int . check) . chop9
    where check xs 
              | (even.sum) xs = take 8 xs
              | otherwise = error "invalid parity bit"
\end{code}

\begin{code}
faultyTransmit = decode . channel . encode
    where channel = tail
\end{code}

\begin{code}
transmit = decode . channel . encode
    where channel = id
\end{code}
