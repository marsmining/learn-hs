--
-- erik.hs - https://www.youtube.com/watch?v=fQU99SJdWGY
--

double x = x + x

quadruple = double . double


fact n = product [1..n]

avg xs = sum xs `div` length xs

abs1 :: Int -> Int
abs1 n | n >= 0 = n
       | otherwise = -n

not1 :: Bool -> Bool
not1 True = False
not1 False = True

ff = \x -> x + 1
fff = (*2)

-- https://www.youtube.com/watch?v=cdPyykm2-gg

concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs]

r1 n = [x | x <- [1..n], even x]

f1 :: Int -> [Int]
f1 n = [x | x <- [1..n], n `mod` x == 0]

p1 :: Int -> Bool
p1 n = f1 n == [1,n]

-- http://youtu.be/OrAVS4QbMqo?t=19m36s

type Parser a = String -> [(a, String)]

item :: Parser Char
item = \inp -> case inp of
     []     -> []
     (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []

return' :: a -> Parser a
return' v = \inp -> [(v,inp)]