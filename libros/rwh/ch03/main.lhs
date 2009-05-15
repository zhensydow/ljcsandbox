\begin{code}
data List a = Cons a (List a)
            | Nil
              deriving( Show )
\end{code}

\begin{code}
fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)
\end{code}

\begin{code}
toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
\end{code}

\begin{code}
data Tree a = Tree (Maybe (a ,Tree a, Tree a))
              deriving( Show )
\end{code}

\begin{code}
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
\end{code}

\begin{code}
mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral.length $ xs)
\end{code}

