import qualified Data.Map as Map

main = do
    print "Hello"
    let x = 1+2+3+4+5
    print "The answer is"
    print (2*x)
    line <- getLine
    print (line ++ " end")

data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)

tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

ntree e = Node e EmptyTree EmptyTree

itree :: (Ord a) => a -> Tree a -> Tree a
itree e EmptyTree = ntree e
itree e (Node a l r)
  | e == a = Node e l r
  | e < a  = Node a (itree e l) r
  | e > a  = Node a l (itree e r)

data W a = W a deriving Show

return' :: a -> W a
return' a = W a

fmap' :: (a -> b) -> (W a -> W b)
fmap' f (W x) = W (f x)

a = W 3
b = fmap' (+2) a

bind' :: (a -> W b) -> (W a -> W b)
bind' f (W x) = f x

f :: Int -> W Int
f x = W (x+1)

c = bind' f (f 1)
d = bind' f (bind' f (f 1))

g :: Int -> W Int -> W Int
g n m = bind' (return' . (+n)) m

h :: W Int -> W Int -> W Int
h mx my = bind' (\x -> g x my) mx

gg = \x -> g x $ W 5
