\begin{code}
module PutJSON where
\end{code}

\begin{code}
import Data.List( intercalate )
import SimpleJSON
\end{code}

\begin{code}
renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)
\end{code}

\begin{code}
putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue
\end{code}

