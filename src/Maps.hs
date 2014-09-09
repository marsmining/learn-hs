module Maps where

import qualified Data.Map as Map

mm :: Map.Map String Integer
mm = Map.fromList [("a", 56), ("b", 42)]

gg :: Map.Map Integer Integer
gg = Map.insert 4 42 Map.empty
