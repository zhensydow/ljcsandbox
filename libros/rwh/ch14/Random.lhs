\begin{code}
import System.Random
import Control.Monad
\end{code}

\begin{code}
newtype State s a = State {
      runState :: s -> (a, s)
    }
\end{code}

\begin{code}
type RandomState a = State StdGen a
\end{code}

\begin{code}
get :: State s s
get = State $ \s -> (s, s)
\end{code}

\begin{code}
put :: s -> State s ()
put s = State $ \_ -> ((), s)
\end{code}

\begin{code}
instance Monad (State s) where
    return a = State $ \s -> (a,s)
    m >>= k = State $ \s -> let (a,s') = runState m s
                            in runState (k a) s'
\end{code}

\begin{code}
getRandom :: RandomState Int
getRandom = do
  g <- get
  let (val, g') = random g
  put g'
  return val
\end{code}

\begin{code}
getTwoRandoms :: RandomState (Int, Int)
getTwoRandoms = liftM2 (,) getRandom getRandom
\end{code}
