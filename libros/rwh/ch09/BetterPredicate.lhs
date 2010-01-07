\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
\end{code}

\begin{code}
import Control.Monad( filterM )
import System.Directory( Permissions(..), getModificationTime, getPermissions )
import System.Time( ClockTime(..) )
import System.FilePath( takeExtension )
import Control.Exception( bracket, handle, SomeException )
import System.IO( IOMode(..), hClose, hFileSize, openFile )
import RecursiveContents( getRecursiveContents )
\end{code}

\begin{code}
type Predicate = FilePath            -- path to object entry
               -> Permissions        -- permissions
               -> Maybe Integer      -- file size (or Nothing)
               -> ClockTime          -- last modified
               -> Bool
\end{code}

\begin{code}
type InfoP a = FilePath
             -> Permissions
             -> Maybe Integer
             -> ClockTime
             -> a
\end{code}

\begin{code}
getFileSize :: FilePath -> IO (Maybe Integer )
getFileSize path = handle nothingHandler $ 
                   bracket (openFile path ReadMode) hClose $ \h -> do
                     size <- hFileSize h
                     return $ Just size
    where nothingHandler (_::SomeException) = return Nothing
\end{code}

\begin{code}
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return $ p name perms size modified
\end{code}

\begin{code}
pathP :: InfoP FilePath
pathP p _ _ _ = p
\end{code}

\begin{code}
sizeP :: InfoP Integer
sizeP _ _ (Just s) _ = s
sizeP _ _ Nothing _ = -1
\end{code}

\begin{code}
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k
\end{code}

\begin{code}
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z
\end{code}

\begin{code}
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

andP = liftP2 (&&)
orP = liftP2 (||)
\end{code}

\begin{code}
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w
\end{code}

\begin{code}
(==?) = equalP
(&&?) = andP
(||?) = orP
(>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
(<?) = lesserP

infix 4 ==?
infix 3 &&?, ||?
infix 4 >?, <?
\end{code}

\begin{code}
myTest path _ (Just size) _ = (takeExtension path == ".cpp") && (size > (128*1024))
myTest _ _ _ _ = False
\end{code}

\begin{code}
myTest2 = liftPath takeExtension ==? ".cpp" &&? sizeP >? (128*1024)
\end{code}
