\begin{code}
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  (putStr.show.length) xs
  putStrLn " characters"
\end{code}

\begin{code}
beep = putStr "\BEL"
\end{code}

\begin{code}
cls = putStr "\ESC[2J"
\end{code}

\begin{code}
type Pos = (Int, Int)
\end{code}

\begin{code}
goto (x,y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"
\end{code}

\begin{code}
writeat p xs = do
  goto p
  putStr xs
\end{code}

\begin{code}
seqn [] = return ()
seqn (a:as) = a >> seqn as
\end{code}

Calculator

\begin{code}
box = [ "+---------------+"
      , "|               |"
      , "+---+---+---+---+"
      , "| q | c | d | = |"
      , "+---+---+---+---+"
      , "| 1 | 2 | 3 | + |"
      , "+---+---+---+---+"
      , "| 4 | 5 | 6 | - |"
      , "+---+---+---+---+"
      , "| 7 | 8 | 9 | * |"
      , "+---+---+---+---+"
      , "| 0 | ( | ) | / |"
      , "+---+---+---+---+"]
\end{code}

\begin{code}
buttons = standard ++ extra
    where
      standard = "qcd=123+456-789*0()/"
      extra = "QCD \ESC\BS\DEL\n"
\end{code}

\begin{code}
showbox = seqn [ writeat (1,y) xs | (y,xs) <- zip [1..13] box ]
\end{code}

\begin{code}
\end{code}
