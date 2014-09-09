
module Factorial where

import System.TimeIt
import Text.Printf

fac :: Int -> Int

-- fac n = if n == 0 then 1 else n * fac (n-1)

-- fac 0 = 1
-- fac n = n * fac (n-1)

-- fac n = foldr (*) 1 [1..n]

-- fac n = foldr (\x g n -> g (x*n)) id [1..n] 1

facs = scanl (*) 1 [1..]
fac n = facs !! n

main = do
  timeIt $ do
    printf "fact! %d\n" $ fac 20