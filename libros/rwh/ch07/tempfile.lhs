\begin{code}
import System.IO( hTell, hPutStrLn, hPutStr, hSeek, SeekMode(..)
                , hGetContents, hClose, openTempFile, Handle )
import System.Directory( getTemporaryDirectory, removeFile )
import System.IO.Error( catch )
import Control.Exception( finally )
\end{code}

\begin{code}
main :: IO ()
main = withTempFile "mytemp.txt" myAction
\end{code}

\begin{code}
myAction :: FilePath -> Handle -> IO()
myAction tempName tempH =
    do
      putStrLn "Welcome to tempfile.hs"
      putStrLn $ "I have a temporary file at " ++ tempName

      pos <- hTell tempH
      putStrLn $ "My initial position is " ++ show pos

      let tempdata = show [1..10]
      putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
      hPutStr tempH tempdata

      pos <- hTell tempH
      putStrLn $ "After writing, my new position is " ++ show pos

      putStrLn $ "The file content is: "
      hSeek tempH AbsoluteSeek 0

      c <- hGetContents tempH

      putStrLn c

      putStrLn $ "Which could be expressed as this Haskell literal:"
      print c
\end{code}

\begin{code}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
      tempdir <- catch getTemporaryDirectory (\_ -> return ".")
      (tempFile, tempH) <- openTempFile tempdir pattern 
                                      
      finally (func tempFile tempH) 
                  (do 
                    hClose tempH
                    removeFile tempFile)
\end{code}
