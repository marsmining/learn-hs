
yell :: Int -> String
yell n = if n == 0 then "hiya" else yell (n - 1) ++ "a"

-- rev xs accu = if null xs then accu else rev (tail xs) (head xs : accu)

range a b accu = if a == b then accu else range (succ a) b (a : accu)

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

my_xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

evens xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

rez0 = evens my_xxs

-- which right triangle that has integers for all sides
-- and all sides equal to or smaller than 10 has a perimeter of 24?

triangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- range
fact :: Integer -> Integer
fact n = product [1..n]

-- recursive
fact' :: Integer -> Integer
fact' n = if n == 0 then 1 else n * fact (pred n)

-- pattern matching
pfact :: (Integral a) => a -> a
pfact 0 = 1
pfact n = n * pfact (n - 1)

-- destructuring
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "cannot take head of empty list"
head' (x:_) = x

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
         let sideArea = 2 * pi * r * h
             topArea = pi * r ^2
         in sideArea + 2 * topArea

max' :: (Ord a) => [a] -> a
max' [] = error "cannot call max' on empty list"
max' [x] = x
max' (x:xs)
     | x > maxTail = x
     | otherwise = maxTail
     where maxTail = max' xs

rep' :: (Num i, Ord i) => i -> a -> [a]
rep' n a
     | n <= 0 = []
     | otherwise = a : rep' (n - 1) a

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n xs
      | n <= 0 = []
      | null xs = []
      | otherwise = (head xs) : take' (n - 1) (tail xs)

take2' :: (Num i, Ord i) => i -> [a] -> [a]
take2' n [] = []
take2' n _ | n <= 0 = []
take2' n (x:xs) = x : take2' (n - 1) xs

rev' xs accu = if null xs then accu else rev' (tail xs) (head xs : accu)

rev2 :: [a] -> [a]
rev2 [] = []
rev2 (x:xs) = rev2 xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
-- elem' a (x:xs) = if a == x then True else elem' a xs
elem' a (x:xs)
      | a == x = True
      | otherwise = elem' a xs

-- quicksort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
