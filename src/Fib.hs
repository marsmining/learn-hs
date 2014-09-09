{-# LANGUAGE BangPatterns #-}
module Fib where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Data.List (unfoldr)

-- from http://www.haskell.org/haskellwiki/The_Fibonacci_sequence

fib' :: (Eq a, Num a, Num a1) => a -> a1
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

{- def fib(n):
      a, b = 0, 1
      for _ in xrange(n):
          a, b = b, a + b
      return a  -}

-- tail recursive with bang patterns
fib'' :: (Eq a, Num a, Num t) => a -> t
fib'' m = go m (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)

-- monadic

fib :: (Enum a, Num t, Num a) => a -> t
fib n = flip evalState (0,1) $ do
  forM [0..(n-1)] $ \_ -> do
    (a,b) <- get
    put (b,a+b)
  (a,_) <- get
  return a

-- fib sequences

-- my dumb first try
fibs :: Int -> [Int]
fibs 0 = []
fibs 1 = [0]
fibs 2 = [0, 1]
fibs n = pf ++ [sum $ drop (l - 2) pf] where
           pf = fibs (n - 1)
           l = length pf

-- canonical
fibs' :: [Int]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

-- haskell tutorial
fibs'' :: [Int]
fibs'' = 1 : 1 : [ a+b | (a,b) <- zip fibs'' (tail fibs'') ]

-- unfoldr
fibs''' :: [Integer]
fibs''' = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

-- iterate
fibs'''' :: [Integer]
fibs'''' = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

-- infinite sequences example

numsFrom :: Int -> [Int]
numsFrom n = n : numsFrom (n + 1)

-- applicative style example

main' :: IO ()
main' = do
          str <- (++) <$> getLine <*> getLine
          print str

main'' :: IO ()
main'' = do
           str <- (++) <$> getLine <*> getLine
           print str

main :: IO ()
main = (++) <$> getLine <*> getLine >>= print
-- main = ((++) <$> getLine <*> getLine) >>= print

-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- in '(++) <$> getLine', (++) :: String -> (String -> String)
-- returns IO (String -> String)
-- without do, have IO String need to pull out String and call print
-- somefun :: Monad M => M a -> (a -> M b) -> M b

gg :: IO (String -> String)
gg = (++) <$> getLine

hh :: IO String
hh = gg <*> getLine

ii :: String -> String
ii = (++) "foo"

jj :: String -> String
jj = (++ "foo")

-- lazy eval

magic :: Int -> Int -> [Int]
magic 0 _ = []
magic m n = m : (magic n (m+n))

getIt :: [Int] -> Int -> Int
getIt [] _ = 0
getIt (x:_) 1 = x
getIt (_:xs) n = getIt xs (n-1)

