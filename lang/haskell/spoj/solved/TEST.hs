loop = do
  a <- getLine
  if (a == "42") 
    then return () 
    else 
      do
        putStrLn a
        loop

main = loop
