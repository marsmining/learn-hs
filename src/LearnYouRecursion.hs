max' :: (Ord a) => [a] -> a
max' [] = error "max of empty list"
max' [x] = x
max' (x:xs)
     | x > maxTail = x
     | otherwise = maxTail
     where maxTail = max' xs
rez = max' [55, 67, 7]

max'' :: (Ord a) => [a] -> a
max'' [] = error "max of empty list"
max'' [x] = x
max'' (x:xs) = max x (max'' xs)
rez' = max'' [5, 6, 1, 67]

rep' :: (Num i, Ord i) => i -> a -> [a]
rep' n x
     | n <= 0 = []
     | otherwise = x:rep' (n-1) x
rez'' = rep' 5 "fred"

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
      | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
trez = take' 3 "abcdefg"

rev' :: [a] -> [a]
rev' [] = []
rev' (x:xs) = rev' xs ++ [x]
rrez = rev' "abcde"

repeat' :: a -> [a]
repeat' x = x : repeat x
crez = take 30 (repeat' "q")

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zrez = zip' [1, 2, 3] "abcdef"

el' :: (Eq a) => a -> [a] -> Bool
el' _ [] = False
el' x [y] = x == y
el' x (y:ys)
    | x == y = True
    | otherwise = el' x ys
erez = el' 'd' "abcdefg"

v = (+ 9)
vv = v 7

comp100 :: (Num a, Eq a) => a -> Bool
comp100 = (== 100)
cc = comp100 23
cc' = comp100 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
arez = applyTwice (+5) 6

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zzrez = zipWith' (+) [1, 2, 3] [4, 5, 6]
zzrez' = zipWith (++) ["abc", "xyz"] ["rtl", "poo"]