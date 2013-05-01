--
-- State.hs - learning monads thru haskell
--

-- basic type and fn def
inc :: Integer -> Integer
inc n = n + 1

fac 0 = 1
fac n = n * fac (n-1)

-- main fn

main = putStrLn "hello"
-- main = print (fac 42)

-- cons
xs = 1:(2:(3:[]))
iss = [1, 2, 3] == 1:2:3:[]

-- implement length of list
mylen :: [a] -> Integer
mylen [] = 0
mylen (x:xs) = mylen xs + 1

-- type def
-- data MyBool = False | True -- nullary, multi-ctor, union, sum
data MyPoint a = MyPoint a a -- tuple, binary cartesian, polymorphic, unary type ctor
  deriving Show

coord = MyPoint 5 7

-- note, data ctor at runtime, type ctor at compile time

-- recursive type
data Tr a = Lf a | Br (Tr a) (Tr a)
  deriving Show

-- return list of leaves left to right
fringe :: Tr a -> [a]
fringe (Lf x) = [x]
fringe (Br lft rgt) = fringe lft ++ fringe rgt

mytr = (Br (Lf 4) (Br (Lf 9) (Lf 2)))
x = fringe mytr

-- type synonyms
type MyStr   = [Char]
type Name    = String
data Address = None | String
type Person  = (Name, Address)

-- list comprehensions
cm  = [inc x | x <- [1, 2, 3, 4]]
cmm = [(x, y) | x <- [1, 2, 3, 4, 5], y <- [5, 6], x <= 2]

-- quick sort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x]
            ++ [x]
            ++ qsort [y | y <- xs, y >= x]

st = qsort [7, 3, 9, 3, 8]

-- impl of map, not fn app higher precedance than infix ':'
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

-- anonymous fn's
lam = mymap (\x -> x * x) [1, 5, 10]

-- impl concat
(^^^) :: [a] -> [a] -> [a]
[]     ^^^ ys = ys
(x:xs) ^^^ ys = x : xs ^^^ ys

-- note the type sig of fn composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- sections, turn infix to 2-arity fn
sec = (+) 5 7
secc = mymap (+2) [1, 11, 111]

add = (+)
ten = 5 `add` 5

-- strictness
ones = 1 : ones
nFrom n = n : nFrom (n + 1)

fib = 1 : 1 : [ a + b | (a, b) <- zip fib (tail fib) ]

-- impl of zip
zz :: [a] -> [b] -> [(a, b)]
zz (x:xs) (y:ys) = (x, y) : zip xs ys
zz _ _ = []

-- in? impl
inn :: Eq a => [a] -> a -> Bool
inn [] _ = False
inn (x:xs) e = x == e || inn xs e

dd = fmap inc [6, 7]

-- what is this fn?

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- given a value a, where a is a Monad, and a fn from a to a type b which is a Monad, return a b

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
-- same as above but args switched

-- 'f' is a 'type variable'
-- 'f a' is the type variable/ctor 'f' applied to type 'a'
class MyFunctor f where
      myfmap :: (a -> b) -> f a -> f b

-- below doesn't compile
-- types are not first class
-- myType = Lf Integer

instance Functor Tr where
  fmap f (Lf x) = Lf (f x)
  fmap f (Br t1 t2) = Br (fmap f t1) (fmap f t2)

cv = fmap inc mytr
