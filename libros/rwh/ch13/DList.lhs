\begin{code}
module DList ( DList, fromList, toList
             , empty, append, cons, dfoldr ) where
\end{code}

\begin{code}
import Data.Monoid
\end{code}

\begin{code}
newtype DList a = DL {
      unDL :: [a] -> [a]
    }
\end{code}

\begin{code}
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
\end{code}

\begin{code}
append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)
\end{code}

\begin{code}
fromList :: [a] -> DList a
fromList = DL . (++)
\end{code}

\begin{code}
toList :: DList a -> [a]
toList (DL xs) = xs []
\end{code}

\begin{code}
empty :: DList a
empty = DL id
\end{code}

\begin{code}
cons :: a -> DList a -> DList a
cons x (DL xs) = DL $ (x:) . xs
infixr `cons`
\end{code}

\begin{code}
dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z = foldr f z . toList
\end{code}

\begin{code}
safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                (y:_) -> Just y
                _ -> Nothing
\end{code}

\begin{code}
dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
    where go x xs = cons (f x) xs
\end{code}

\begin{code}
instance Functor DList where
    fmap = dmap
\end{code}

\begin{code}
instance Monoid (DList a) where
    mempty = empty
    mappend = append
\end{code}

\begin{code}
prop_monoid_assoc a b c = l == r
    where l = a `mappend` (b `mappend` c)
          r = (a `mappend` b) `mappend` c
\end{code}

\begin{code}
prop_monoid_id a = (a `mappend` mempty == a) && (mempty `mappend` a == a)
\end{code}