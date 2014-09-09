module Real where

fl :: (a -> b -> a) -> a -> [b] -> a
fl step acc (x:xs) = fl step (step acc x) xs
fl _    acc []     = acc

fr :: (b -> a -> a) -> a -> [b] -> a
fr step acc (x:xs) = (step x (fr step acc xs))
fr _    acc []     = acc

-- map in terms of foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr step [] xs
    where step x ys = f x : ys

-- filter in terms of foldr
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldr step [] xs
    where step x ys | p x = x : myFilter p ys
                    | otherwise = myFilter p ys

-- foldl in terms of foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step acc xs = foldr f id xs $ acc
        where f x g a = g (step a x)

-- myFoldl ms1 [] [1, 2, 3]
-- foldr ff id [1, 2, 3] $ acc
--   where ff = (\x g a -> g (ms1 a x))
-- ff 1 (foldr ff id [2, 3]) $ acc
-- ff 1 (ff 2 (foldr ff id [3])) $ acc
-- ff 1 (ff 2 (ff 3 (foldr ff id []))) $ acc
-- ff 1 (ff 2 (ff 3 id)) $ acc

-- simple step function for testing
ms1 :: Show a => String -> a -> String
ms1 s n = s ++ show n

rez :: ((String, String, String), (String, String))
rez = ((foldl nf nz ns, fl nf nz ns, myFoldl nf nz ns),
       (foldr (flip nf) nz ns, fr (flip nf) nz ns))
       where nf = ms1
             nz = []
             ns = [1..5]

main :: IO ()
main = print rez

-- fl ms1 "" [1, 2, 3]
-- fr (flip ms1) "" [1, 2, 3]

-- given a list like below, foldr will..
-- 1 : (2 : (3 : []))
-- replace [] with acc, and : with step
-- 1 + (2 + (3 + 0 ))
