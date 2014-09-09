module Lists where

import Data.List (tails)
import Data.Char (isUpper)

-- fix second arg using infix function in a section
allLower :: String -> Bool
allLower = all (`elem` ['a'..'z'])

-- remember point-free
niceSum :: Num a => [a] -> a
-- niceSum xs = foldl (+) 0 xs
niceSum  = foldl (+) 0

-- as patterns
fg :: [[a]]
fg = tails []
suffixes :: [a] -> [[a]]
suffixes (x:xs) = (x:xs) : suffixes xs
suffixes _    = []
aspFixes :: [t] -> [[t]]
aspFixes xs@ (_:xs') = xs : aspFixes xs'
aspFixes _ = []

-- comp is right associative
-- result of right side must match input of left side
capCount :: String -> Int
capCount = length . filter (isUpper . head) . words


main :: IO ()
main = print $ capCount "Hello there, Mom!"
