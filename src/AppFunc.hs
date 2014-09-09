{-
  AppFunc.hs
-}

module AppFunc where

import Control.Applicative

myAction :: IO String

{-
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b
-}

-- myAction = (++) <$> getLine <*> getLine

myAction = getLine >>= (\x -> getLine >>= (\y -> return $ x ++ y))

main = myAction

-- list as app functor
p0 = (+) <$> [1, 2, 3] <*> [10, 11, 12]

-- ziplist as app functor
p1 = getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [10, 11, 12]
