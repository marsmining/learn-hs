module OkasakiRB where

data Color = R | B deriving Show
data Tree elt = E | T Color (Tree elt) elt (Tree elt) deriving Show

type Set a = Tree a

empty :: Set elt
empty = E

member :: Ord elt => elt -> Set elt -> Bool
member _ E = False
member x (T _ a n b) | x <  n = member x a
                     | x == n = True
                     | x >  n = member x b

insert :: Ord elt => elt -> Set elt -> Set elt
insert x s = makeBlack (ins s)
  where ins E = T R E x E
        ins (T color a y b) | x <  y = balance color (ins a) y b
                            | x == y = T color a y b
                            | x >  y = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> Tree elt -> elt -> Tree elt -> Tree elt
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

-- examples

t0 :: Tree Int
t0 = (T B (T R (T B E 5 E) 6 (T B E 7 E)) 8 (T R E 9 (T B E 10 E)))
t1 :: Tree Char
t1 = (T B E 'x' (T R E 'y' E))
t2 :: Tree Int
t2 = foldr insert E [1..9]

main :: IO ()
main = print t2 >>
    print (member 5 t0) >>
    print (member 'x' t1) >>
    print (member 'a' t1)
