\begin{code}
data Tree a = Leaf a | Node (Tree a) (Tree a)
\end{code}

\begin{code}
prettyShow :: (Show a, Num a) => Tree a -> String  -- WORKS
--prettyShow :: (Num a, Show a) => Tree a -> String  -- FAILS
prettyShow (Leaf a) = show a
prettyShow (Node a b) = (simple a) ++ (simple b)
\end{code}

\begin{code}
simple :: (Show a, Num a) => Tree a -> String
simple (Leaf x) = prettyShow (Leaf x)
simple x = "(" ++ prettyShow x ++ ")"
\end{code}
