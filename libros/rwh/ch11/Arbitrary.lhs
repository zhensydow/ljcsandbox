\begin{code}
import Test.QuickCheck( Arbitrary(..), elements )
\end{code}

\begin{code}
data Ternary = Yes
             | No
             | Unkown
             deriving( Eq, Show )
\end{code}

\begin{code}
instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unkown]
\end{code}
