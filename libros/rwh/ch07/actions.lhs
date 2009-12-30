\begin{code}
str2action :: String -> IO ()
str2action input = putStrLn $ "Data: " ++ input
\end{code}

\begin{code}
printall = mapM_ (str2action.show) [1..10]
\end{code}
