\begin{code}
module GlobRegex( globToRegex, matchesGlob ) where
\end{code}

\begin{code}
import Text.Regex.Posix( (=~) )
import Data.Char( isAlpha, toLower, toUpper )
\end{code}

\begin{code}
globToRegex :: Bool -> String -> String
globToRegex b cs = '^' : globToRegex' b cs ++ "$"
\end{code}

\begin{code}
globToRegex' :: Bool -> String -> String
globToRegex' _ "" = ""
globToRegex' b ('*':cs) = ".*" ++ globToRegex' b cs
globToRegex' b ('?':cs) = '.' : globToRegex' b cs
globToRegex' b ('[':'!':c:cs) = "[^" ++ checkCase b c ++ charClass b cs
globToRegex' b ('[':c:cs) = '[' : checkCase b c ++ charClass b cs
globToRegex' _ ('[':_) = error "unterminated character class"
globToRegex' b (c:cs) = escape b c ++ globToRegex' b cs
\end{code}

\begin{code}
escape :: Bool -> Char -> String
escape b c 
    | c `elem` regexChars = '\\' : [c]
    | b = [c]
    | otherwise = '[' : checkCase b c ++ "]"
    where regexChars = "\\+()^$.{}]|"
\end{code}

\begin{code}
charClass :: Bool -> String -> String
charClass b (']':cs) = ']' : globToRegex' b cs
charClass b (c:cs) = checkCase b c ++ charClass b cs
charClass _ [] = error "unterminated character class"
\end{code}

\begin{code}
matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex False pat
\end{code}

\begin{code}
checkCase :: Bool -> Char -> String
checkCase True c = [c]
checkCase False c 
    | isAlpha c = toUpper c : [toLower c]
    | otherwise = [c]
\end{code}
