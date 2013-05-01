module FunctorExercises where

instance Functor (Either e) where
  fmap _ (Left l) = Left l
  fmap g (Right r) = Right (g r)

f0 = fmap id Right 9

instance Functor ((->) e) where
  fmap g x = g . x

f1 = fmap (+1) (+1)

data Pair a = Pair a a

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

-- (,) :: a -> b -> (a, b)

instance Functor ((,) e) where
  fmap g (a, b) = (a, g b)

f2 = (6, 7)
f3 = (,) 6 7
f4 = ((,) 5)

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node ts) = Node (map (fmap g) ts)

f5 = Node [Leaf (\x -> "foo")]

