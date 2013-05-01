import Data.List
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub
rez = numUniques "aabbbccccc"

frez = foldl (\a b -> a + b) 9 [6, 7, 5]

phoneBook =   
  [("betty","555-2938")  
  ,("bonnie","452-2928")  
  ,("patsy","493-2928")  
  ,("lucille","205-2928")  
  ,("wendy","939-8282")  
  ,("penny","853-2492")  
  ]  

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs
krez = findKey "wendy" phoneBook

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
frez' = findKey' "bonnie" phoneBook

