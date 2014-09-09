--
-- from Mark P Jones paper
--

data State s a = ST (s -> (a, s))

instance Functor (State s) where
  fmap f (ST st) = ST (\s -> let (x, s') = st s in (f x, s'))

instance Monad (State s) where
  return x = ST (\s -> (x,s))
  (>>=) m f = ST (\s -> let ST m' = m
                            (x, s1) = m' s
                            ST f' = f x
                            (y, s2) = f' s1
                        in (y, s2))

sinc = ST (\s -> (5 * s, s + 1))

sc :: s -> State s a -> a
sc s (ST f) = let (x, _) = f s in x
