\begin{code}
tribonaccis = 1:1:1:zipWith3 (\a b c->a+b+c) tribonaccis (tail tribonaccis) (drop 2 tribonaccis)
\end{code}
