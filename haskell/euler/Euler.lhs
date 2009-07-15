\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}
\end{code}

\begin{code}
module Euler( digits, toint ) where
\end{code}

\begin{code}
import Data.Char( digitToInt )
\end{code}

\begin{code}
digits = map (fromIntegral . digitToInt) . show
\end{code}

\begin{code}
toint = foldl (\a b-> a * 10 + (fromIntegral b)) 0
\end{code}
