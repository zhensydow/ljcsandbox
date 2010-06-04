\begin{code}
import Control.Monad
\end{code}

\begin{code}
data MovieReview = MovieReview { revTitle :: String
                               , revUser :: String
                               , revReview :: String }
\end{code}

\begin{code}
lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
\end{code}

\begin{code}
liftedReview alist =
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)
\end{code}

\begin{code}
apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist
\end{code}
