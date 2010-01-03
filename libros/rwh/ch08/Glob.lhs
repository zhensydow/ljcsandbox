\begin{code}
module Glob( namesMatching ) where
\end{code}

\begin{code}
import System.Directory( doesDirectoryExist, doesFileExist
                       , getCurrentDirectory, getDirectoryContents )
import System.FilePath( dropTrailingPathSeparator, splitFileName, (</>) )
import Control.Exception( handle )
import Control.Monad( forM )
import GlobRegex( matchesGlob )
\end{code}

\begin{code}
isPattern :: String -> Bool
isPattern = any ( `elem` "[*?" )
\end{code}

\begin{code}
namesMatching pat
    | not $ isPattern pat = do
                              exists <- doesNameExist pat
                              return $ if exists then [pat] else []
\end{code}

\begin{code}
doesNameExist name = do
  fexist <- doesFileExist name
  if fexist
    then return True
    else doesDirectoryExist name
\end{code}
