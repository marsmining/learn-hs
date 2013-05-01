--
-- LearnYouFunctors.hs
--

import Data.Char  
import Data.List  
import Control.Applicative  

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line

-- bad functor, id law doesn't hold

data CMaybe a = CNothing | CJust Int a deriving (Show)  

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust n a) = CJust (n+1) (f a)

d1 = Just "johntra"
d2 = Just "volta"

cc :: Maybe String -> Maybe String -> Maybe String
cc (Just a) (Just b) = Just (a ++ b)
cc _ _ = Nothing

d3 = cc d1 d2

d4 = (++) <$> d1 <*> d2