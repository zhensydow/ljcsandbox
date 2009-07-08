\begin{code}
pent n = (n * ((3*n) - 1)) `div` 2
\end{code}

\begin{code}
invpentagonal n = floor $ ((sqrt sqn) + (1/2)) / 3
    where rn = fromInteger n
          sqn = (1/4) + 6*rn
\end{code}

\begin{code}
ispent :: Integer -> Bool
ispent n = ((ceiling divid) == (floor divid)) && ((floor divid) `mod` 3 == 0)
    where rn = fromInteger n
          sqn = (1/4) + 6*rn
          divid = (sqrt sqn) + (1/2)
   
\end{code}

\begin{code}
maxpent = 10000
\end{code}

\begin{code}
pentpairs = [(pent i, pent j) | i <- [1..maxpent], j <- [(i+1)..maxpent]]
\end{code}

\begin{code}
numbers = filter (\(a,b)->ispent (a+b)) $ pentpairs
\end{code}

\begin{code}
solution = filter ispent $ map (\(a,b)-> b - a) numbers
\end{code}

\begin{code}
main = print solution
\end{code}
