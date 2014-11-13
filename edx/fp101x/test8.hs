sequence_' :: Monad m => [m a] -> m ()
--sequence_' [] = return ()
--sequence_' (m:ms) = (foldl (>>) m ms) >> return ()
--sequence_' ms = foldl (>>) (return ()) ms
--sequence_' (m:ms) = m >> sequence_' ms
--sequence_' (m:ms) = m >>= \_ -> sequence_' ms
sequence_' ms = foldr (>>) (return ()) ms


sequence1' :: Monad m => [m a] -> m [a]
sequence1' [] = return []
sequence1' (m:ms) = m >>= \ a -> do as <- sequence1' ms
                                    return (a : as)

--sequence2' :: Monad m => [m a] -> m [a]
--sequence2' ms = foldr func (return ()) ms
--  where
--    func m acc = do x <- m
--                    xs <- acc
--                    return (x: xs)

--sequence3' :: Monad m => [m a] -> m [a]
--sequence3' ms = foldr func (return []) ms
--  where
--    func :: Monad m => m a -> m [a] -> m [a]
--    func m acc = m : acc


--sequence4' :: Monad m => [m a] -> m [a]
--sequence4' [] = return []
--sequence4' (m:ms) = return (a:as)
--  where
--    a <- m
--    as <- sequence4' ms


sequence5' :: Monad m => [m a] -> m [a]
sequence5' ms = foldr func (return []) ms
  where
    func m acc = do x <- m
                    xs <- acc
                    return (x : xs)


--sequence6' :: Monad m => [m a] -> m [a]
--sequence6' [] = return []
--sequence6' (m:ms) = m >> \ a -> do as <- sequence6' ms
--                                   return (a : as)

--sequence7' :: Monad m => [m a] -> m [a]
--sequence7' [] = return []
--sequence7' (m:ms) = m >>= \ a ->
--    as <- sequence7' ms
--    return (a:as)

sequence8' :: Monad m => [m a] -> m [a]
sequence8' [] = return []
sequence8' (m:ms) = do a <- m
                       as <- sequence8' ms
                       return (a:as)



mapM1' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1' f as = sequence8' (map f as)


mapM2' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM2' f [] = return []
mapM2' f (a:as) = f a >>= \b -> mapM2' f as >>= \ bs -> return (b:bs)


--mapM3' :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM3' f as = sequence_' (map f as)

mapM4' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM4' = undefined

mapM5' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM5' = undefined

mapM6' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM6' f [] = return []
mapM6' f (a:as) = do b <- f a
                     bs <- mapM6' f as
                     return (b:bs)

mapM7' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM7' f [] = return []
mapM7' f (a:as) = undefined

mapM8' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM8' f [] = return []
mapM8' f (a:as) = undefined


foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do
  a' <- f a x
  foldLeftM f a' xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (x:xs) = do
  b' <- foldRightM f b xs
  f x b'

liftM :: Monad m => (a -> b) -> m a -> m b
--liftM f m = do x <- m
--               return (f x)
liftM f m = m >>= \a -> f a
--liftM f m = m >>= \a -> return (f a)
--liftM f m = return (f m)
--liftM f m = m >>= \a -> m >>= \b -> return (f a)
--liftM f m = m >>= \a -> m >>= \b -> return (f b)
--liftM f m = mapM f [m]
