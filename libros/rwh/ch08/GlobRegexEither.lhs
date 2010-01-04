\begin{code}
module GlobRegexEither( globToRegex, matchesGlob ) where
\end{code}

\begin{code}
import Text.Regex.Posix( (=~) )
\end{code}

\begin{code}
type GlobError = String
\end{code}

\begin{code}
globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
                   Right ret -> Right $ '^' : ret ++ "$"
                   err -> err
\end{code}

\begin{code}
globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = case globToRegex' cs of
                          Right ret -> Right $ ".*" ++ ret
                          err -> err
globToRegex' ('?':cs) = case globToRegex' cs of
                          Right ret -> Right $ '.' : ret
                          err -> err
globToRegex' ('[':'!':c:cs) = case charClass cs of
                                Right ret -> Right $ "[^" ++ c : ret
                                err -> err
globToRegex' ('[':c:cs) = case charClass cs of
                            Right ret -> Right $ '[' : c : ret
                            err -> err
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs) = case globToRegex' cs of
                        Right ret -> Right $ escape c ++ ret
                        err -> err 
\end{code}

\begin{code}
escape :: Char -> String
escape c 
    | c `elem` regexChars = '\\' : [c]
    | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"
\end{code}

\begin{code}
charClass :: String -> Either GlobError String
charClass (']':cs) = case globToRegex' cs of
                       Right ret -> Right $ ']' : ret
                       err -> err
charClass (c:cs) = case charClass cs of
                     Right ret -> Right $ c : ret
                     err -> err
charClass [] = Left "unterminated character class"
\end{code}

\begin{code}
matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = case globToRegex pat of
                           Right pat' -> name =~ pat'
                           Left err -> False
\end{code}

