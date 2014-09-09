--
-- from: http://learnyouahaskell.com/for-a-few-monads-more
--

import Control.Monad.State

type S = []

pop :: S a -> (a, S a)
pop (x:xs) = (x, xs)

push :: a -> S a -> (a, S a)
push a s = (a, a : s)

manip :: (Num a) => S a -> (a, S a)
manip s = let
  (c, s1) = push 3 s
  (d, s2) = pop s1
  in pop s2

data Car = Car { model :: String, year :: Int } deriving Show

main = putStr "woah.."

myfold :: (a -> b -> b) -> b -> [a] -> b
myfold f b [] = b
myfold f b (x:xs) = f x (myfold f b xs)
