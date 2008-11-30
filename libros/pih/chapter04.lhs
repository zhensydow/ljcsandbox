\begin{code}
halve :: [a] -> ([a],[a])
halve xs 
    | odd n = (take n xs, drop n xs)
    | otherwise = error "invalid length"
    where n = div (length xs) 2
\end{code}

\begin{code}
safetail1 :: [a] -> [a]
safetail1 xs = if (length xs) == 0 then [] else tail xs
\end{code}

\begin{code}
safetail2 :: [a] -> [a]
safetail2 xs
    | (length xs) == 0 = []
    | otherwise = tail xs
\end{code}

\begin{code}
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 b = tail b
\end{code}

\begin{code}
v1 :: Bool -> Bool -> Bool
v1 True True = True
v1 True False = True
v1 False True = True
v1 False False = False
\end{code}

\begin{code}
v2 :: Bool -> Bool -> Bool
v2 False False = False
v2 _ _ = True
\end{code}

\begin{code}
v3 :: Bool -> Bool -> Bool
v3 False b = b
v3 True _ = True
\end{code}

\begin{code}
v4 :: Bool -> Bool -> Bool
v4 b c
    | b == c = b
    | otherwise = True
\end{code}

\begin{code}
ex44 :: Bool -> Bool -> Bool
ex44 a b = if (a == True) && (b == True) then True else False
\end{code}

\begin{code}
ex45 :: Bool -> Bool -> Bool
ex45 a b = if a == True then b else False
\end{code}

\begin{code}
multi :: Num a => a -> a -> a -> a
multi = \x -> \y -> \z -> x * y * z
\end{code}
