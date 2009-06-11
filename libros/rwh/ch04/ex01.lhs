\begin{code}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
\end{code}

\begin{code}
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
\end{code}

\begin{code}
safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse
\end{code}

\begin{code}
safeInit :: [a] -> Maybe [a]
safeInit = safeTail . reverse
\end{code}

\begin{code}
splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = pre : splitWith p (dropWhile (not.p) pos)
    where (pre,pos) = span p xs
\end{code}
