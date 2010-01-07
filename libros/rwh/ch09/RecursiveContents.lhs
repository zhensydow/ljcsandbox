\begin{code}
module RecursiveContents( getRecursiveContents ) where
\end{code}

\begin{code}
import Control.Monad( forM, liftM )
import System.Directory( doesDirectoryExist, getDirectoryContents )
import System.FilePath( (</>) )
\end{code}

\begin{code}
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- filterSys $ getDirectoryContents topdir
  paths <- forM names $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return $ concat paths
      where filterSys = liftM $ filter (`notElem` [".", ".."])
\end{code}