module Arrows where
import Control.Arrow
import qualified Control.Category as C

-- record syntax
-- data Make = Make { id :: Int, name :: String } deriving (Show)
-- data Part = Part { id :: Int, name :: String } deriving (Show)

data Make = Make Int String deriving Show
data Part = Part Int String deriving Show

make :: Int -> Maybe Make
make 1 = Just $ Make 1 "Suzuki"
make _ = Nothing

parts :: Make -> Maybe [Part]
parts (Make 1 _) = Just [Part 1 "Gearbox", Part 2 "Clutch cable"]
parts _ = Nothing

ff :: Int -> Maybe [Part]
ff x = do {
  m <- make x;
  parts m
  }

gg :: Int -> Maybe [Part]
gg x = make x >>= \m -> parts m

--
-- arrow tutorial
--

newtype SimpleFunc a b = SimpleFunc { runF :: (a -> b) }
instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f)
                         where mapFst g (a,b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
                         where mapSnd g (a,b) = (a, g b)
instance C.Category SimpleFunc where
  (.) (SimpleFunc g) (SimpleFunc f) = SimpleFunc (g . f)
  id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry       
-- arr (\op (x,y) -> x `op` y)
(***) :: Arrow cat => cat b c -> cat d c1 -> cat (b, d) (c, c1)
f *** g = first f >>> second g
(&&&) :: Arrow cat => cat a c -> cat a c1 -> cat a (c, c1)
f &&& g = split >>> first f >>> second g
-- = split >>> f *** g
liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op
-- = f &&& g >>> unsplit op

fx, gx, hx :: SimpleFunc Int Int
fx = arr (`div` 2)
gx = arr (\x -> x*3 + 1)
hx = liftA2 (+) fx gx
hOutput :: Int
hOutput = runF hx 8

plusminus, double, h2 :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])
double    = arr (* 2)
h2        = liftA2 (+) plusminus double 

h2Output :: [Int]
h2Output = runKleisli h2 8

main :: IO ()
main = do
   let
       prepend x = arr (x ++)
       append  x = arr (++ x)
       withId  t = returnA <+> t
       xform = (withId $ prepend "<") >>>
               (withId $ append ">") >>>
               (withId $ ((prepend "!") >>> (append "!")))
       xs = ["test", "foobar"] >>= (runKleisli xform)
   mapM_ putStrLn xs

--
-- simple adt example
--

data Profession = Fighter | Archer | Accountant deriving Show
data Race = Human | Elf | Orc | Goblin deriving Show
data PlayerCharacter = PlayerCharacter Race Profession deriving Show
orc :: PlayerCharacter
orc = PlayerCharacter Orc Fighter

-- simple newtype eg
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  
benny :: CharList
benny = CharList "benny"
charlie :: CharList
charlie = CharList "charlie"

-- making a tuple a functor with newtype
newtype Pair b a = Pair { getPair :: (a,b) }  
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)
-- getPair $ fmap (*100) (Pair (2,3))
-- > (200,3)
