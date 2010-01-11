\begin{code}
import System.FilePath( replaceExtension )
import System.Directory( doesFileExist, renameDirectory, renameFile )
import Glob( namesMatching )
\end{code}

\begin{code}
renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
renameWith f path = do
  rename path real
  return real
    where real = f path
\end{code}

\begin{code}
rename :: FilePath -> FilePath -> IO ()
rename old new = do
  isFile <- doesFileExist old
  let f = if isFile then renameFile else renameDirectory
  f old new
\end{code}

\begin{code}
cc2cpp = namesMatching "*.cc" >>= mapM (renameWith $ flip replaceExtension ".cpp")
\end{code}

\begin{code}
main = cc2cpp
\end{code}