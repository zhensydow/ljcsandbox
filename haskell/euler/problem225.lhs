\begin{code}
tribonaccis = 1:1:1:zipWith3 (\a b c->a+b+c) tribonaccis (tail tribonaccis) (drop 2 tribonaccis)
\end{code}

\begin{code}
tn n = 3 * du / dl
    where
      du = ((du1 + du2 + ot)**n) * (b ** ot)
      du1 = ot * ((19 + a)**ot)
      du2 = ot * ((19 - a)**ot)
      dl = (b ** (2/3)) + 4 - (2 * (b ** ot))
      a = 3 * r33
      b = 586 + 102 * r33
      r33 = sqrt 33
      ot = 1 / 3
\end{code}
