\begin{code}
data Op = Plus | Minus | Mul | Div | Pow
          deriving (Eq, Show)
\end{code}

\begin{code}
data SymbolicManip a = Number a
                     | Arith Op (SymbolicManip a) (SymbolicManip a)
                     deriving (Eq, Show)
\end{code}

\begin{code}
instance Num a => Num (SymbolicManip a) where
    (+) = Arith Plus
    (-) = Arith Minus
    (*) = Arith Mul
    negate = Arith Mul (Number (-1))
    abs a = error "abs is unimplemented"
    signum _ = error "signum is unimplemented"
    fromInteger = Number . fromInteger
\end{code}
