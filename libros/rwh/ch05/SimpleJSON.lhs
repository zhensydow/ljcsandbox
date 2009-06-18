\begin{code}
module SimpleJSON
    ( JValue(..)
    , getString, getNumber
    , getBool, getObject, getArray, isNull ) where
\end{code}

\begin{code}
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving( Eq, Ord, Show )
\end{code}

\begin{code}
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing
\end{code}

\begin{code}
getNumber :: JValue -> Maybe Int
getNumber (JNumber n) = Just (truncate n)
getNumber _ = Nothing
\end{code}

\begin{code}
getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing
\end{code}

\begin{code}
getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing
\end{code}

\begin{code}
getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing
\end{code}

\begin{code}
isNull :: JValue -> Bool
isNull = (JNull ==)
\end{code}

