\begin{code}
import ControlledVisit( Info(..), getUsefulContents, getInfo, isDirectory )
import System.FilePath( (</>), takeExtension, takeFileName )
import Data.Char( toLower )
\end{code}

\begin{code}
data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                  deriving( Show )
\end{code}

\begin{code}
type Iterator seed = seed -> Info -> Iterate seed
\end{code}

\begin{code}
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return $ unwrap endSeed
    where
      fold seed spath = getUsefulContents spath >>= walk seed
      walk seed (x:xs) = do
        let path' = path </> x
        info <- getInfo path'
        case iter seed info of
          done @ (Done _) -> return done
          Skip seed' -> walk seed' xs
          Continue seed'
            | isDirectory info -> do
                next <- fold seed' path'
                case next of
                  done @ (Done _) -> return done
                  seed'' -> walk (unwrap seed'') xs
            | otherwise -> walk seed' xs
      walk seed [] = return $ Continue seed
\end{code}

\begin{code}
atMostNPictures :: Int -> Iterator [FilePath]
atMostNPictures n paths info
    | length paths == n = Done paths
    | isDirectory info && takeFileName path == ".svn" = Skip paths
    | extension `elem` [".jpg", ".png"] = Continue $ path : paths
    | otherwise = Continue paths
    where 
      extension = map toLower (takeExtension path)
      path = infoPath info
\end{code}

\begin{code}
countDirectories count info
    | isDirectory info = Continue $ count + 1
    | otherwise = Continue count
\end{code}
