--
-- TypeClazz - http://www.haskell.org/haskellwiki/Typeclassopedia
--

z0 :: Num a => [] a
z0 = 5 : []

z1 = Left 5
z2 = Right 9

-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Functor (Either e) where
  fmap g (Left x)  = Left (x)
  fmap g (Right y) = Right (g y)

f0 = fmap (either id (+3)) Left 5
f1 = fmap (either id (+3)) Right 9
-- f2 = fmap (+1) Right 4

instance Functor ((->) e) where
  fmap g h = \x -> g (h x)

