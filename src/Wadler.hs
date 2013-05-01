--
-- Wadler.hs - understanding monads
--

data Term = Con Int | Div Term Term
  deriving Show

ans, ans2, err :: Term
ans  = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
ans2 = (Div (Div (Div (Con 1972 ) (Con 2 )) (Con 23 )) (Con 2))
err  = (Div (Con 1)(Con 0))

-- basic
e0 :: Term -> Int
e0 (Con a) = a
e0 (Div t u) = e0 t `quot` e0 u

-- w/error handling
data MM a = Raise Exception | Return a
  deriving Show
type Exception = String

e1 :: Term -> MM Int
e1 (Con a) = Return a
e1 (Div t u) = case e1 t of
                      Raise e -> Raise e
                      Return a ->
                        case e1 u of
                             Raise e -> Raise e
                             Return b ->
                               if b == 0
                                    then Raise "divide by zero"
                                    else Return (a `quot` b)

-- w/state
type SM a = State -> (a, State)
type State = Int

e2 :: Term -> State -> (Int, State)
e2 (Con a) x = (a, x)
e2 (Div t u) x = let (a, y) = e2 t x in
                 let (b, z) = e2 u y in
                 (a `quot` b, z + 1)

-- w/output
type IM a = (Output, a)
type Output = String

line :: Term -> Int -> Output
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n"

e3 :: Term -> IM Int
e3 (Con a)   = (line (Con a) a, a)
e3 (Div t u) = let (x, a) = e3 t in
               let (y, b) = e3 u in
               (x ++ y ++ line (Div t u) (a `quot` b), a `quot` b)

-- monadic evaluator

e4 :: Term -> IdM Int
e4 (Con a)   = return a
e4 (Div t u) = (e4 t) >>= \a -> (e4 u) >>= \b -> return (a `quot` b)

-- identity monad
data IdM a = IdM a
  deriving Show
instance Monad IdM where
  return x = IdM x
  (>>=) (IdM x) f = f x

-- exception monad
type Ex = String
data ExM a = Fire Ex | Ret a
  deriving Show
instance Monad ExM where
  return x = Ret x
  m >>= k = case m of
              Fire e -> Fire e
              Ret a  -> k a
fire :: Ex -> ExM a
fire e = Fire e

e5 :: Term -> ExM Int
e5 (Con a)   = return a
e5 (Div t u) = (e5 t) >>= \a -> (e5 u) >>= \b -> if b == 0
                                                    then fire "div by zero"
                                                    else return (a `quot` b)

-- state monad
type StM a = St -> (a, St)
type St = Int

unit :: a -> StM a
unit a = \x -> (a, x)

(***) :: StM a -> (a -> StM b) -> StM b
m *** k = \x -> let (a, y) = m x in
               let (b, z) = k a y in
               (b, z)
tick :: StM ()
tick = \x -> ((), (x+1))

e6 :: Term -> StM Int
e6 (Con a)   = unit a
e6 (Div t u) = (e6 t) *** \a -> (e6 u) *** \b -> tick *** \_ -> unit (a `quot` b)

-- output monad
data OutM a = OutM (Out, a)
  deriving Show
type Out = String

instance Monad OutM where
  return a = OutM ("", a)
  m >>= k = let OutM (x, a) = m in
            let OutM (y, b) = k a in
            OutM (x ++ y, b)

out :: Out -> OutM ()
out x = OutM (x, ())

e7 :: Term -> OutM Int
e7 (Con a)   = out(line(Con a) a) >>= \_ -> return a
e7 (Div t u) = (e7 t) >>= \a ->
  (e7 u) >>= \b ->
  out(line (Div t u) (a `quot` b)) >>= \_ ->
  return (a `quot` b)

