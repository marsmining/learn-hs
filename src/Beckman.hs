--
-- Beckman.hs
--

data Tr a = Lf a | Br (Tr a) (Tr a)
  deriving Show

tr1 = Br (Lf 'a') (Br (Br (Lf 'b') (Lf 'c')) (Lf 'd'))

type S = Int
type Lt a = Tr (S, a)

label :: Tr a -> Lt a
label tr = snd (lab tr 0)
  where
    lab :: Tr a -> S -> (S, Lt a)
    lab (Lf contents) n = ((n+1), (Lf (n, contents)))
    lab (Br l r) n0     = let (n1, l') = lab l n0
                              (n2, r') = lab r n1
                          in  (n2, Br l' r')

tr2 = label tr1

-- monadic style

newtype Labeled a = Labeled (S -> (S, a))

instance Monad Labeled where

  return x = Labeled (\st -> (st, x))

  Labeled fst0 >>= fany1 = 
    Labeled $ \st0 -> 
      let (st1, any1) = fst0 st0 
          Labeled fst1 = fany1 any1
      in fst1 st1 

mlabel :: Tr a -> Lt a
mlabel tr = let Labeled mt = mkm tr
            in snd (mt 0)

mkm :: Tr a -> Labeled (Lt a)

mkm (Lf x) = updateState >>= \n -> return $ Lf (n,x)

-- Alternative: do n <- updateState
--                 return $ Lf (n,x)

mkm (Br l r)
  = mkm l >>= \l' ->
    mkm r >>= \r' ->
    return $ (Br l' r')

-- Alternative: do l' <- mkm l
--                 r' <- mkm r
--                 return $ (Br l' r')          

updateState :: Labeled S
updateState =  Labeled (\n -> ((n+1),n))

main = print $ mlabel tr1
