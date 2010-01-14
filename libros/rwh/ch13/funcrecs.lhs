\begin{code}
data CustomColor = CustomColor {
      red :: Int
    , green :: Int
    , blue :: Int 
    } deriving( Eq, Show, Read )
\end{code}

\begin{code}
data FuncRec = FuncRec { name :: String
                       , colorCalc :: Int -> ( CustomColor, Int ) }
\end{code}

\begin{code}
plus5color c x = (c, x + 5)
\end{code}

\begin{code}
purple = CustomColor 255 0 255
\end{code}

\begin{code}
plus5 = FuncRec { name = "plus5", colorCalc = plus5color purple }
always0 = FuncRec { name = "always0", colorCalc = const (purple, 0) }
\end{code}
