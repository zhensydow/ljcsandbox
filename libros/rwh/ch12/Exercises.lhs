\begin{code}
getval4 :: (a, a, a, a) -> Int -> a
getval4 (_,_,_,x) 3 = x
getval4 (_,_,x,_) 2 = x
getval4 (_,x,_,_) 1 = x
getval4 (x,_,_,_) 0 = x
getval4 _ _ = error "invalid index"
\end{code}

\begin{code}
getval6 :: (a, a, a, a, a, a) -> Int -> a
getval6 (_,_,_,_,_,x) 5 = x
getval6 (_,_,_,_,x,_) 4 = x
getval6 (x,y,z,w,_,_) n 
    | n < 4 && n >= 0 = getval4 (x,y,z,w) n
    | otherwise = error "invalid index"
\end{code}
