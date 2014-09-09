--
-- from:
-- http://blog.sigfpe.com/2007/04/homeland-security-threat-level-monad.html
--

data Threat = LOW | GUARDED | ELEVATED | HIGH | SEVERE
  deriving (Eq,Ord,Enum,Show)

data T a = T Threat a deriving Show

instance Monad T where
  return x = T LOW x
  (T t x) >>= f = let T t' x' = f x in T (max t t') x'

level :: T x -> Threat
level (T x a) = x

g x y = y >>= (return . (+x))
h x y = x >>= (\x -> g x y)

