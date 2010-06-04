\begin{code}
module Logger( Logger, Log, runLogger, record ) where
\end{code}

\begin{code}
import Control.Monad( liftM, liftM2 )
\end{code}

\begin{code}
type Log = [String]
\end{code}

\begin{code}
newtype Logger a = Logger { execLogger :: (a, Log) }
\end{code}

\begin{code}
runLogger :: Logger a -> (a, Log)
runLogger = execLogger
\end{code}

\begin{code}
record :: String -> Logger ()
record s = Logger ((), [s])
\end{code}

\begin{code}
instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = runLogger m 
                  (b, x) = runLogger $ k a
              in Logger (b, w ++ x)
\end{code}

\begin{code}
globToRegex :: String -> Logger String
globToRegex cs = do
  ds <- globToRegex' cs
  return $ '^':ds
\end{code}

\begin{code}
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) = do
  record "any" 
  ds <- globToRegex' cs
  return $ '.':ds
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return $ ".*" ++ ds 
globToRegex' ('[':'!':c:cs) = do
  record "character class, negative"
  ds <- charClass cs
  return $ "[^" ++ c : ds
globToRegex' ('[':c:cs) = do
  record "character class"
  ds <- charClass cs
  return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
\end{code}

\begin{code}
escape :: Char -> Logger String
escape c 
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise = return [c]
    where regexChars = "\\+()^$.{}]|"
\end{code}

\begin{code}
charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
\end{code}
