\begin{code}
module PrettyJSON( renderJValue ) where
\end{code}

\begin{code}
import SimpleJSON( JValue(..) )
import Prettify
\end{code}

\begin{code}
renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JArray a) = series '[' ']' renderJValue a
renderJValue (JObject o) = series '{' '}' field o
    where field (k,v) = string k
                        <> text ": "
                        <> renderJValue v
\end{code}
