--
-- Final.hs - http://ertes.de/articles/monads.html
--

import Data.Word
import Control.Monad.State

-- maybe monad
data Maybo a = Justo a | Nothingo
  deriving (Eq, Show)

instance Monad Maybo where
  return x = Justo x
  (>>=) Nothingo f = Nothingo
  (>>=) (Justo x) f = f x

-- eg int sqrt
isqrt :: Integer -> Maybo Integer
isqrt x = isqrt' x (0,0)
  where
    isqrt' x (s,r)
      | s > x     = Nothingo
      | s == x    = Justo r
      | otherwise = isqrt' x (s + 2*r + 1, r+1)

i4thrt :: Integer -> Maybo Integer
i4thrt x = isqrt x >>= isqrt

-- list monad
mps :: Num a => a -> [a]
mps x = [x, x*2, x*3]

tmps :: Num a => [a]
tmps = [2, 7, 23] >>= mps

-- identity monad
data Id a = Id a
instance Monad Id where
  return x = Id x -- or: return = Id
  Id x >>= f = f x

-- monad laws
-- return x >>= f == f x
-- c >>= return == c
-- c >>= (\x -> f x >>= g) == (c >>= f) >>= g

-- state Monad
r0 = runState (put 9) 5
r1 = runState (get >>= \s -> return (2*s)) 10
r2 = runState (get >>= \s -> put (s+1)) 10
r3 = runState (get >>= \s -> put (s+1) >> return (s^2)) 3

type LCGState = Word32

lcg :: LCGState -> (Integer, LCGState)
lcg s0 = (output, s1)
  where s1 = 1103515245 * s0 + 12345
        output = fromIntegral s1 * 2^16 `div` 2^32

getR :: State LCGState Integer
getR = get >>= \s0 -> let (x, s1) = lcg s0
                      in put s1 >> return x

addThree :: State LCGState Integer
addThree = getR >>= \a ->
  getR >>= \b ->
  getR >>= \c -> return (a+b+c)

z0 = runState addThree 5
z1 = runState addThree 5

amain = getLine >>= \line ->
  if line /= "quit"
     then putStrLn line >> amain
     else return ()

bmain :: IO ()
bmain = do
  line <- getLine
  if line /= "quit"
    then putStrLn line >> bmain
    else return ()

y0 = do x <- [1,2,3]
        y <- [7,8,9]
        return (x + y)

