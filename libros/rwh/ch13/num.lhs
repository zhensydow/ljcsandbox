\begin{code}
import Data.List( intersperse )
\end{code}

\begin{code}
data Op = Plus | Minus | Mul | Div | Pow
          deriving (Eq, Show)
\end{code}

\begin{code}
data SymbolicManip a = Number a
                     | Symbol String
                     | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
                     | UnaryArith String (SymbolicManip a)
                     deriving (Eq)
\end{code}

\begin{code}
instance Num a => Num (SymbolicManip a) where
    (+) = BinaryArith Plus
    (-) = BinaryArith Minus
    (*) = BinaryArith Mul
    negate = BinaryArith Mul (Number (-1))
    abs = UnaryArith "abs"
    signum _ = error "signum is unimplemented"
    fromInteger = Number . fromInteger
\end{code}

\begin{code}
instance (Fractional a) => Fractional (SymbolicManip a) where
    (/) = BinaryArith Div
    recip = BinaryArith Div (Number 1)
    fromRational = Number . fromRational
\end{code}

\begin{code}
instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp = UnaryArith "exp"
    log = UnaryArith "log"
    sqrt = UnaryArith "sqrt"
    (**) = BinaryArith Pow
    sin = UnaryArith "sin"
    cos = UnaryArith "cos"
    tan = UnaryArith "tan"
    asin = UnaryArith "asin"
    acos = UnaryArith "acos"
    atan = UnaryArith "atan"
    sinh = UnaryArith "sinh"
    cosh = UnaryArith "cosh"
    tanh = UnaryArith "tanh"
    asinh = UnaryArith "asinh"
    acosh = UnaryArith "acosh"
    atanh = UnaryArith "atanh"
\end{code}

\begin{code}
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number a) = show a
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) = pa ++ pop ++ pb
    where
      pa = simpleParen a
      pb = simpleParen b
      pop = op2str op
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ prettyShow a ++ ")"
\end{code}

\begin{code}
op2str Plus = "+"
op2str Minus = "-"
op2str Div = "/"
op2str Mul = "*"
op2str Pow = "**"
\end{code}

\begin{code}
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x
\end{code}

\begin{code}
instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow
\end{code}

\begin{code}
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow = join " " . toList
    where 
      toList (Number x) = [show x]
      toList (Symbol x) = [x]
      toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
      toList (UnaryArith opstr a) = toList a ++ [opstr]
      join delim = concat . intersperse delim 
\end{code}

\begin{code}
simplify :: (Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op a b) = 
    case (op, sa, sb) of
      (Mul, Number 1, b) -> b
      (Mul, a, Number 1) -> a
      (Mul, Number 0, _) -> Number 0
      (Mul, _, Number 0) -> Number 0
      (Div, a, Number 1) -> a
      (Plus, Number 0, b) -> b
      (Plus, a, Number 0) -> a
      (Minus, a, Number 0) -> a
      _ -> BinaryArith op sa sb
    where sa = simplify a
          sb = simplify b
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x
\end{code}

\begin{code}
data Num a => Units a = Units a (SymbolicManip a)
                        deriving (Eq)
\end{code}

\begin{code}
instance (Num a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "mis-matched units"
    a - (Units xb ub) = a + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)
\end{code}

\begin{code}
instance (Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)
\end{code}

\begin{code}
instance (Floating a) => Floating (Units a) where
    pi = Units pi (Number 1)
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub) 
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua) 
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua) 
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua) 
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"
\end{code}

\begin{code}
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)
\end{code}

\begin{code}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x
\end{code}

\begin{code}
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)
\end{code}

\begin{code}
instance (Show a, Num a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)
\end{code}

\begin{code}
test :: Num a => a
test = 2 * 5 + 3
\end{code}
