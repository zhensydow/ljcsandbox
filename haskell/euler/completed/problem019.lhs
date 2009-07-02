\begin{code}
data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
           deriving( Show, Enum, Eq )
\end{code}

\begin{code}
data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre
           deriving( Show, Enum, Eq )
\end{code}

\begin{code}
data Fecha = Fecha { dia :: Int
                   , mes :: Mes
                   , anyo :: Int 
                   , name :: Dia }
             deriving( Show, Eq )
\end{code}

\begin{code}
inicio1 = Fecha { dia = 1, mes = Enero
                , anyo = 1900, name = Lunes }
inicio2 = Fecha {dia = 1, mes = Enero
                , anyo = 1901, name = Martes}
final = Fecha {dia = 31, mes = Diciembre
              , anyo = 2000, name = Sabado}
\end{code}

\begin{code}
leapyear n 
    | n `mod` 400 == 0 = True
    | n `mod` 100 == 0 = False
    | n `mod` 4 == 0 = True
    | otherwise = False
\end{code}

\begin{code}
numberOfdays Febrero y
    | leapyear y = 29
    | otherwise = 28
numberOfdays m _
    | m `elem` [Abril,Junio,Septiembre,Noviembre] = 30
    | otherwise = 31
\end{code}

\begin{code}
incDia :: Dia -> Dia
incDia Domingo = Lunes
incDia d =  succ d
\end{code}

\begin{code}
incMes :: Mes -> Mes
incMes Diciembre = Enero
incMes m = succ m
\end{code}

\begin{code}
incrementa :: Fecha -> Fecha
incrementa (Fecha { dia = 31, mes = Enero, anyo = a, name = n }) = Fecha {dia = 1, mes = Febrero, anyo = a + 1, name = incDia n }
incrementa (Fecha { dia = d, mes = m, anyo = a, name = n})
    | d == numberOfdays m a = Fecha { dia = 1, mes = incMes m, anyo = a, name = incDia n }
    | otherwise = Fecha { dia = d + 1, mes = m, anyo = a, name = incDia n }
\end{code}

\begin{code}
checkFecha d1 m1 a1 (Fecha { dia = d, mes = m, anyo = a, name = _}) = d1 == d && m1 == m && a1 == a
\end{code}

\begin{code}
cuentaDomingos inicio final = cuentaDomingos' 0 inicio final
cuentaDomingos' n f final
    | f == final = n
    | (name f) == Domingo && (dia f) == 1 = cuentaDomingos' (n+1) (incrementa f) final
    | otherwise = cuentaDomingos' n (incrementa f) final
\end{code}

\begin{code}
solution = (cuentaDomingos inicio1 final) - 1
\end{code}
