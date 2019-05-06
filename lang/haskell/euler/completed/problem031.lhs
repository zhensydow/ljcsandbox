In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1£1 + 150p + 220p + 15p + 12p + 31p
How many different ways can £2 be made using any number of coins?

\begin{code}
coins = [200, 100, 50, 20, 10, 5, 2, 1]
\end{code}

FROM SICP

\begin{code}
cc 0 _ = 1
cc _ [] = 0
cc amount (x:xs)
    | amount < 0 = 0
    | otherwise = (cc amount xs) + (cc (amount - x) (x:xs))
\end{code}

\begin{code}
solution = cc 200 coins
\end{code}

\begin{code}
main = print solution
\end{code}
