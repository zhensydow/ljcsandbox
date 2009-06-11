\begin{code}
import System.Environment (getArgs)
import Data.List (transpose)
\end{code}

\begin{code}
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)
\end{code}

\begin{code}
main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
\end{code}

\begin{code}
myFunction xs = unlines $ transpose $ lines xs
\end{code}
