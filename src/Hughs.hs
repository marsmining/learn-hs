module Hughs where

newtype SM s a = SM (s -> (a, s))
