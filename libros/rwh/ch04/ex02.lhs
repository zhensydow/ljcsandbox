\begin{code}
import Data.Char( digitToInt )
import Data.List( foldl' )
\end{code}

\begin{code}
asInt_fold :: String -> Int
asInt_fold "" = error "empty string"
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs = foldl fn 0 xs
    where fn x y 
              | y `elem` ['0'..'9'] = x * 10 + (digitToInt y)
              | otherwise = error $ "invalid caracter " ++ [y] 
\end{code}

\begin{code}
type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "empty string"
asInt_either ('-':xs) = Right $ (-1) * asInt_fold xs
asInt_either xs = foldl fn (Right 0) xs
    where fn (Left m) y = Left m
          fn (Right x) y
              | y `elem` ['0'..'9'] = Right $ x * 10 + (digitToInt y)
              | otherwise = Left $  "invalid caracter " ++ [y] 
\end{code}

\begin{code}
miconcat :: [[a]] -> [a]
miconcat = foldr (\a b-> a++b) []
\end{code}

\begin{code}
mitakeWhile :: (a->Bool) -> [a] -> [a]
mitakeWhile f [] = []
mitakeWhile f (x:xs) 
    | f x = x : mitakeWhile f xs
    | otherwise = []
\end{code}

\begin{code}
mitakeWhile' :: (a->Bool) -> [a] -> [a]
mitakeWhile' f = foldr step []
    where step x xs
              | f x = x : xs
              | otherwise = []
\end{code}

\begin{code}
any' p = foldl' (\x y->x || p y) True
\end{code}

\begin{code}
cycle' xs = foldr (\_ ys-> xs ++ ys) [] [1..]
\end{code}

