{-
  Applicative.hs
-}

module ApplicativeExercises where

import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Data.Map

{-

instance Applicative Maybe where
  pure x = Just x
  (<*>) (Just g) (Just x) = Just (g x)
  (<*>) _ _ = Nothing

-}

p0 = pure (+) <*> [2,3,4] <*> pure 4
p1 = (+) <$> [2,3,4] <*> pure 4

--
-- Applicative programming with effects
--

-- simple evaluator w/env for 'free' variables

data Expr v = Var v
            | Val Integer
            | Add (Expr v) (Expr v)
  deriving Show

type Env = Map

fetch :: Char -> Env Char Integer -> Integer
fetch c m = findWithDefault 0 c m

-- eval :: Expr Char -> Env Char Integer -> Integer
-- eval (Var x) e   = fetch x e
-- eval (Val i) e   = i
-- eval (Add p q) e = eval p e + eval q e

eval :: Expr Char -> Env Char Integer -> Integer
eval (Var x) e = fetch x e
eval (Val i) e   = i
eval (Add p q) e = eval p e + eval q e

w1 = Var 'a'
w2 = Add (Var 'a') (Add (Var 'c') (Val 400))
l9 :: Env Char Integer
l9 = fromList [('a', 6), ('b', 9), ('c', 3)]

