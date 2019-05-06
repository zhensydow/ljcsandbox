Let r be the remainder when (a1)n + (a+1)n is divided by a2.

For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728  42 mod 49. And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.

For 3  a  1000, find  rmax.

\begin{code}
elements a = [(((a-1)^n)+((a+1)^n)) `mod` (a^2) | n <- [1..2*a] ]
\end{code}

\begin{code}
solution = sum $ map (maximum.elements) [3..1000]
\end{code}

\begin{code}
main = print solution
\end{code}
