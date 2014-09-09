{-# LANGUAGE ScopedTypeVariables, RankNTypes, ExistentialQuantification #-}
module Forall where

-- ScopedTypeVariables
foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
    postProcess val
    where
        val :: b
        val = maybe onNothin onJust mval

-- RankNTypes
-- liftTup :: (t -> t1) -> (t, t) -> (t1, t1)     [inferred]
-- liftTup :: (x -> f x) -> (a, b) -> (f a, f b)  [logical, doesn't work]
liftTup :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
liftTup liftFun (a, b) = (liftFun a, liftFun b)

-- ExistentialQuantification
data EQList = forall a. EQList [a]
eqListLen :: EQList -> Int
eqListLen (EQList x) = length x
nn :: Int
nn = eqListLen $ EQList ["Hello", "World"]

main :: IO ()
main = do putStrLn "woot!"
          print $ liftTup (\x -> [x]) (True, False)
          print $ liftTup (\x -> [x]) ("a", "b")
          print $ liftTup (\x -> [x]) ("a", False)
          print $ "cat1: " ++ cat1 ++ ", " ++ (foldl (flip (:)) [] "cat")
          print $ "cat2: " ++ cat2 ++ ", " ++ (foldr (:) [] "cat")
          print $ "mrev: " ++ mrev "cat"

mrev :: [a] -> [a]
-- mrev [] = []
-- mrev (x:xs) = mrev xs ++ [x]
-- mrev = foldl (flip (:)) []
mrev [] = []
mrev xs = mrev' xs [] where
     mrev' [] acc = acc
     mrev' (y:ys) acc = mrev' ys (y:acc)
-- mrev (x:xs) = x : (mrev xs)   [wrong, returns "cat"]

-- left
cat1 :: String
cat1 = (ff (ff (ff [] 'c') 'a') 't') where
     ff = flip (:)
-- right
cat2 :: String
cat2 = (gg 'c' (gg 'a' (gg 't' []))) where
     gg = (:)

foldr'            :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     =  z
foldr' f z (x:xs) =  f x (foldr' f z xs)
-- [1, 2, 3]
-- 1 : [2 : [3 : []]]
-- foldr * 1 [1, 2, 3]
-- 1 * [2 * [3 * 1]]
-- f(1, f(2, f(3, e)))   [right]

-- LEFT
foldl'            :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z []     =  z
foldl' f z (x:xs) =  foldl' f (f z x) xs
-- f(f(f(e, 1), 2), 3)   [left]
-- foldl f z [x1, x2, xn] == (((z `f` x1) `f` x2) `f` x3) `f` x4
-- foldl f z [x1, x2, xn] == f(f(f(f z x1) x2) x3) x4

-- guards
abs' :: (Ord a, Num a) => a -> a
abs' n | n >= 0    =  n
       | otherwise = -n

-- length in terms of foldr
ln :: [a] -> Int
ln = foldr (\_ b -> b + 1) 0

