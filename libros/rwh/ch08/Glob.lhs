\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
module Glob( namesMatching ) where
\end{code}

\begin{code}
import System.Directory( doesDirectoryExist, doesFileExist
                       , getCurrentDirectory, getDirectoryContents )
import System.FilePath( dropTrailingPathSeparator, splitFileName, (</>) )
import Control.Exception( handle, SomeException )
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
        return [pat | exists]
    | otherwise = case splitFileName pat of
                    ("", baseName) -> do
                      curDir <- getCurrentDirectory
                      listMatches curDir baseName
                    (dirName, baseName) -> do
                      dirs <- if isPattern dirName 
                              then namesMatching $ dropTrailingPathSeparator dirName
                              else return [dirName]
                      let listDir = if isPattern baseName
                                    then listMatches
                                    else listPlain
                      pathNames <- forM dirs $ \dir -> do
                        baseNames <- listDir dir baseName
                        return $ map (dir </>) baseNames
                      return $ concat pathNames
\end{code}

\begin{code}
doesNameExist name = do
  fexist <- doesFileExist name
  if fexist
    then return True
    else doesDirectoryExist name
\end{code}

\begin{code}
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle (\(e::SomeException) -> return []) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not . isHidden) names
    return $ filter (`matchesGlob` pat) names'
\end{code}

\begin{code}
isHidden ('.':_) = True
isHidden _ = False
\end{code}

\begin{code}
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist $ dirName </> baseName
  return [baseName | exists]
\end{code}
