\begin{code}
import RecursiveContents( getRecursiveContents )
\end{code}

\begin{code}
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return $ filter p names
\end{code}
