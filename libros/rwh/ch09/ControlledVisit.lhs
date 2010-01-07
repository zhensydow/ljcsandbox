\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
module ControlledVisit( Info(..), traverse, traverseFilter, getUsefulContents, getInfo, isDirectory, myTest ) where
\end{code}

\begin{code}
import System.Directory( Permissions(..), getPermissions, getModificationTime, getDirectoryContents )
import System.FilePath( (</>), takeExtension )
import System.Time( ClockTime(..) )
import System.IO( IOMode(..), hClose, hFileSize, openFile )
import System.Posix.Types( UserID(..) )
import System.Posix.Files( getFileStatus, fileOwner )
import Control.Exception( bracket, handle, SomeException )
import Control.Monad( filterM, liftM, forM )
\end{code}

\begin{code}
data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    , infoOwner :: Maybe UserID
    } deriving( Eq, Ord, Show )
\end{code}

\begin{code}
getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO $ getPermissions path
  size <- maybeIO $ bracket (openFile path ReadMode) hClose hFileSize
  modified <- maybeIO $ getModificationTime path
  user <- maybeIO $ getUserID path
  return $ Info path perms size modified user
\end{code}

\begin{code}
getUserID path = do
  fstatus <- getFileStatus path
  return $ fileOwner fstatus
\end{code}

\begin{code}
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_::SomeException) -> return Nothing) (Just `liftM` act)
\end{code}

\begin{code}
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo $ path : map (path </>) names
  liftM concat $ forM (order contents) $ \info -> 
    if isDirectory info && infoPath info /= path
      then traverse order $ infoPath info
      else return [info]
\end{code}

\begin{code}
traverseFilter :: ([Info] -> [Info]) -> (Info -> Bool) -> FilePath -> IO [Info]
traverseFilter order f path = traverse order path >>= filterM (return.f)
\end{code}

\begin{code}
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return $ filter (`notElem` [".", ".."]) names
\end{code}

\begin{code}
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
\end{code}

\begin{code}
liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> (Info -> c)
liftP2 q f g i = f i `q` g i
\end{code}

\begin{code}
constP :: a -> (Info -> a)
constP k _ = k
\end{code}

\begin{code}
sizeP k = case infoSize k of
            Just s -> s
            Nothing -> -1
\end{code}

\begin{code}
(==?) :: (Eq a) => (Info -> a) -> a -> (Info -> Bool)
(==?) f = liftP2 (==) f . constP
(>?) f = liftP2 (>) f . constP
(<?) f = liftP2 (<) f . constP
(&&?) = liftP2 (&&)
(||?) = liftP2 (||)
\end{code}

\begin{code}
myTest = ((takeExtension . infoPath) ==? ".pdf") &&? (sizeP >? (128*1024))
\end{code}
