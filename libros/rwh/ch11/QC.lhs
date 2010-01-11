\begin{code}
import Test.QuickCheck( Gen(..), Arbitrary(..), quickCheck, verboseCheck, elements, oneof )
import Test.QuickCheck.Batch( run, TestOptions(..), runTests )
import Control.Monad( liftM, liftM2 )
import Data.List( intersperse )
import Prettify
\end{code}

\begin{code}
instance Arbitrary Char where
    arbitrary = elements $ ['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()"
\end{code}

\begin{code}
instance Arbitrary Doc where
    arbitrary = oneof 
                [ return Empty
                , liftM Char arbitrary
                , liftM Text arbitrary
                , return Line
                , liftM2 Concat arbitrary arbitrary
                , liftM2 Union arbitrary arbitrary ]
\end{code}

\begin{code}
prop_empty_id x = empty <> x == x && x <> empty == x
\end{code}

\begin{code}
prop_char c   = char c   == Char c
prop_text s   = text s   == if null s then Empty else Text s
prop_line     = line     == Line
prop_double d = double d == text (show d)
\end{code}

\begin{code}
prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds
\end{code}

\begin{code}
prop_punctuate s xs = punctuate s xs == combine ( intersperse s xs )
    where
      combine [] = []
      combine [x] = [x]
      combine (x:Empty:xs) = x : combine xs
      combine (Empty:y:xs) = y : combine xs
      combine (x:y:xs) = x `Concat` y : combine xs
\end{code}

\begin{code}
options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }
\end{code}

\begin{code}
main = do
    runTests "simple" options
        [ run prop_empty_id
        , run prop_char
        , run prop_text
        , run prop_line
        , run prop_double
        ]

    runTests "complex" options
        [ run prop_hcat
        , run prop_punctuate
        ]
\end{code}
