\begin{code}
names = ["", "one", "two", "three", "four", "five"
        , "six", "seven", "eight", "nine" ]
names' = [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"
         , "sixteen", "seventeen", "eighteen", "nineteen"]
names'' = [ "", "ten", "twenty", "thirty", "forty", "fifty"
          , "sixty", "seventy", "eighty", "ninety"]
\end{code}

\begin{code}
tostring 100 = "onehundred"
tostring 1000 = "onethousand"
tostring n 
    | n < 10 = names !! n
    | n < 20 = names' !! (n - 10)
    | n < 100 = names'' !! (n `div` 10) ++ tostring (n `mod` 10)
    | n < 1000 = names !! (n `div` 100) ++ "hundred" 
                 ++ andstring (n `mod` 100) 
                 ++ tostring (n `mod` 100)
    where andstring 0 = ""
          andstring _ = "and"
\end{code}

\begin{code}
solution = sum $ map (length.tostring) [1..1000]
\end{code}