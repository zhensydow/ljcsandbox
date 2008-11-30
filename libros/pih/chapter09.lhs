\begin{code}
import Chapter08( parse, expr )
\end{code}

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
display xs = do
  writeat (3,2) "             "
  writeat (3,2) $ reverse (take 13 (reverse xs))
\end{code}

\begin{code}
calc xs = do
  display xs
  c <- getChar
  if elem c buttons then process c xs else do
                                        beep
                                        calc xs
\end{code}

\begin{code}
process c xs
    | elem c "qQ\ESC" = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n" = eval xs
    | elem c "cC" = clear
    | otherwise = press c xs
\end{code}

\begin{code}
quit = goto (1,14)
\end{code}

\begin{code}
delete "" = calc ""
delete xs = calc $ init xs
\end{code}

\begin{code}
eval xs = case parse expr xs of
            Just (n,"") -> calc $ show n
            Just (_,out) -> calc "unused data"
            Nothing -> calc "error"
\end{code}

\begin{code}
clear = calc ""
\end{code}

\begin{code}
press c xs = calc $ xs ++ [c]
\end{code}

\begin{code}
run = do
  cls
  showbox
  clear
\end{code}

Game of Life

\begin{code}
width = 50
height = 50

type Board = [Pos]
\end{code}

\begin{code}
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]
\end{code}

\begin{code}
showcells b = seqn [writeat p "O" | p <- b]
\end{code}

\begin{code}
clearcells b = seqn [writeat p " " | p <- b]
\end{code}

\begin{code}
isAlive b p = elem p b
\end{code}

\begin{code}
isEmpty b p = not $ isAlive b p
\end{code}

\begin{code}
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y)
                         ,(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
\end{code}

\begin{code}
wrap (x,y) = (((x-1) `mod` width) + 1
             ,((y-1) `mod` height) + 1)
\end{code}

\begin{code}
liveneighbs b = length . filter (isAlive b) . neighbs
\end{code}

\begin{code}
survivors b = [p|p<-b,elem (liveneighbs b p) [2,3]]
\end{code}

\begin{code}
births b = [p| p<-rmdups (concat (map neighbs b)),
            isEmpty b p, liveneighbs b p == 3]
\end{code}

\begin{code}
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
\end{code}

\begin{code}
nextgen b = survivors b ++ births b
\end{code}

\begin{code}
life b = do
  cls
  life' b

life' b = do
  showcells b
  wait 5000
  clearcells b
  life' $ nextgen b
\end{code}

\begin{code}
wait n = seqn [ return () | _ <- [1..n] ]
\end{code}

Exercises

\begin{code}
readLine :: IO String
readLine = do
  -- no funciona porque en GHC no hay get sin eco
  c <- getChar 
  case c of
    '\n' -> return []
    '\DEL' -> do
             putStr "\ESC[1D "
             readLine
    x -> do
      xs <- readLine
      return (x:xs)
\end{code}

\begin{code}
type NimBoard = [Int]
\end{code}

\begin{code}
initialBoard :: NimBoard
initialBoard = [5,4,3,2,1]
\end{code}

\begin{code}
showNimBoard :: NimBoard -> IO ()
showNimBoard xs = showNimBoard' xs 1
showNimBoard' [] n = return ()
showNimBoard' (x:xs) n = do
  putStr $ show n ++ ": "
  putStrLn $ putStars x
  showNimBoard' xs (n + 1)
    where
      putStars x = take x $ repeat '*'
\end{code}

\begin{code}
nim :: NimBoard -> Int -> IO ()
nim b p = do
  cls
  writeat (0,0) $ "Play " ++ show p ++ "\n"
  showNimBoard b
  (c,n) <- getInput b
  takeStar c n b p
\end{code}

\begin{code}
getInput b = do
  c <- getNumber
  n <- getNumber
  if n > b !! (c-1) 
    then do 
      putStrLn "too much"
      getInput b
    else 
      return (c,n)
\end{code}

\begin{code}
getNumber = do
  c <- getChar
  if elem c "12345" 
    then return $ (read::String->Int) [c] 
    else error "invalid number"
\end{code}

\begin{code}
takeStar pos n b p = if num >= n 
                       then
                         if emptyBoard new
                           then do
                             cls
                             putStrLn $ "Player " ++ show p ++ " win!!"
                           else nim new ((p + 1) `mod` 2)
                       else 
                         nim b p
    where
      num = b !! (pos-1)
      new = (take (pos-1) b) ++ [num-n] ++ (drop pos b)
\end{code}

\begin{code}
emptyBoard = null . filter (>0)
\end{code}