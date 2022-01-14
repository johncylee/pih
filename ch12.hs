{-# LANGUAGE TupleSections #-}

-- 1

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- 2 GHC.Base

-- instance Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> r -> b
  -- fmap = (.)

-- 3 GHC.Base

-- instance Applicative ((->) r) where
  -- pure :: a -> (r -> a)
  -- pure = const
  -- (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
  -- (<*>) f g x = f x (g x)

-- 4

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (Z xs) = Z (map f xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z (zipWith (\ g x -> g x) gs xs)

-- 5

-- 6 GHC.Base

-- instance Monad ((->) r) where
  -- >>= :: (r -> a) -> (a -> r -> b) -> r -> b
  -- f >>= k = \ r -> k (f r) r

-- 7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
-- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var x) = Var (f x)
  fmap _ (Val n) = Val n
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
-- pure :: a -> Expr a
  pure = Var
-- <*> :: Expr (a -> b) -> Expr a -> Expr b
  Var g <*> x = g <$> x
  Val n <*> _ = Val n
  Add l r <*> x = Add (l <*> x) (r <*> x)

instance Monad Expr where
-- >>= :: Expr a -> (a -> Expr b) -> Expr b
  Var x >>= g = g x
  Val n >>= _ = Val n
  Add l r >>= g = Add (l >>= g) (r >>= g)

-- Var x >>= g, g can be some sort of eval to replace expression x (type a) with
-- another value (type b).

-- 8

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do g <$> st

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (x, )
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    f <$> stx

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (
    \s -> let (x,s') = app st s in app (f x) s')
