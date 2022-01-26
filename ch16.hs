import Prelude hiding ((++), take, drop, Maybe(Nothing, Just))

-- 1

data Nat = Zero | Succ Nat
  deriving Show

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-
Show that add n (Succ m) = Succ (add n m), by induction on n.
base case:
add Zero (Succ m) = Succ m             { apply add }
                  = Succ (add Zero m)  { unapply add }
inductive case:
add (Succ n) (Succ m) = Succ (add n (Succ m))  { apply add }
                      = Succ (Succ (add n m))  { hypothesis }
                      = Succ (add (Succ n) m)  { unapply add }
-}

-- 2

add n Zero = n

{-
Show that add n m = add m n, by induction on n.
base case:
add Zero m = m           { apply add }
           = add m Zero  { unapply add }
inductive case:
add (Succ n) m = Succ (add n m)  { apply add }
               = Succ (add m n)  { hypothesis }
               = add m (Succ n)  { unapply add }
-}

-- 3

{-
Show that all (== x) (replicate n x), by induction on n >= 0
base case:
all (== x) (replicate 0 x) = all (== x) []
                           = True
inductive case:
all (== x) (replicate n+1 x) = all (== x) (x : replicate n x)
                             = (x == x) && all (== x) (replicate n x)
                             = True && True
-}

-- 4

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

{-
. Show that xs ++ [] = xs

base case:
[] ++ [] = []
inductive case:
(x:xs) ++ [] = x : (xs ++ [])
             = x : xs

. Show that xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

base case:
[] ++ (ys ++ zs) = ys ++ zs
                 = ([] ++ ys) ++ zs
inductive case:
(x:xs) ++ (ys ++ zs) = x : (xs ++ (ys ++ zs))  { apply ++ }
                     = x : ((xs ++ ys) ++ zs)  { hypothesis }
                     = x : (xs ++ ys) ++ zs    { unapply ++ }
                     = ((x:xs) ++ ys) ++ zs    { unapply ++ }
-}

-- 5

take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

{-
Show that take n xs ++ drop n xs = xs

base case: n = 0
take 0 xs ++ drop 0 xs = [] ++ xs = xs

base case: xs = []
take n [] ++ drop n [] = [] ++ [] = [] = xs

inductive case:
take n (x:xs) ++ drop n (x:xs) = (x : take (n-1) xs) ++ drop (n-1) xs
                               = x : (take (n-1) xs ++ drop (n-1) xs)
                               = x : xs  { hypothesis }
-}

-- 6

data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree a -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

{-
Show that for any Tree t, leaves t - nodes t = 1

base case:
leaves (Leaf x) - nodes (Leaf x) = 1 - 0 = 1

inductive case:
leaves (Node l r) - nodes (Node l r) = leaves l + leaves r - (1 + nodes l + nodes r)
                                     = (leaves l - nodes l) + (leaves r - nodes r) - 1
                                     = 1 + 1 - 1  { hypothesis }
                                     = 1
-}

-- 7

data Maybe a = Nothing | Just a

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

{-
Show that
  fmap id = id
  fmap (g . h) = fmap g . fmap h

fmap id Nothing = Nothing = id Nothing
fmap id (Just x) = Just x = id (Just x)

fmap (g . h) Nothing = Nothing
                     = fmap g (fmap h Nothing)
                     = (fmap g . fmap h) Nothing
fmap (g . h) (Just x) = Just (g (h x))
                      = fmap g (Just (h x))
                      = fmap g (fmap h (Just x))

-}

-- 8

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-
. Show that fmap id = id

fmap id (Leaf x) = Leaf (id x) = Leaf x = id (Leaf x)
fmap id (Node l r) = Node (fmap id l) (fmap id r)
                   = Node (id l) (id r)
                   = Node l r = id (Node l r)

. Show that fmap (g . h) = fmap g . fmap h

fmap (g . h) (Leaf x) = Leaf (g (h x))
                      = fmap g (Leaf (h x))
                      = fmap g (fmap h (Leaf x))
                      = (fmap g . fmap h) (Leaf x)

fmap (g . h) (Node l r) = Node (fmap (g . h) l) (fmap (g . h) r)
                        = Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
                        = Node (fmap g (fmap h l)) (fmap g (fmap h r))
                        = fmap g (Node (fmap h l) (fmap h r))
                        = fmap g (fmap h (Node l r))
                        = (fmap g . fmap h) (Node l r)
-}

-- 9

instance Applicative Maybe where
  pure = Just
  -- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  Just g <*> mx = fmap g mx

{-
1. Show that pure id <*> x = x

Just id <*> x = fmap id x = id x = x

2. Show that pure (g x) = pure g <*> pure x

Just (g x) = fmap g (Just x)    { unapply fmap }
           = Just g <*> Just x  { unapply <*> }
           = pure g <*> pure x

3. Show that x <*> pure y = pure (\g -> g y) <*> x

3.1 Let x = Nothing
Nothing <*> Just y = Nothing
                   = fmap (\g -> g y) <*> Nothing
                   = Just (\g -> g y) <*> Nothing

3.2 Let x = Just f
Just f <*> Just y = Just (f y)  { Refer to 2. }
                  = Just ((\g -> g y) f)
                  = Just (\g -> g y) <*> Just f

4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

4.1 If x or y or z is Nothing => Nothing = Nothing

4.2 Let x = Just x', y = Just y', z = Just z'

Just x' <*> (Just y' <*> Just z') = Just x' <*> Just (y' z')
                                  = Just (x' (y' z'))
                                  = Just ((x' . y') z')
                                  = Just (((.) x' y') z')
                                  = (Just (.) <*> Just x' <*> Just y') <*> Just z'
                                  = (pure (.) <*> x <*> y) <*> z
-}

-- 10

-- instance Monad [] where
--   xs >>= f = [y | x <- xs, y <- f x]

{-
. Show that return x >>= f   = f x

[x] >>= f = [y | x' <- [x], y <- f x'] = [y | y <- f x] = f x

. Show that mx >>= return    = mx

xs >>= return = [y | x <- xs, y <- pure x] = [x | x <- xs] = xs

. Show that (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

(xs >>= f) >>= g = [z | x <- xs, y <- f x, z <- g y]
                 = [z' | x <- xs, z' <- [z | y <- f x, z <- g y]]
                 = [z' | x' <- xs, z' <- (\x -> (f x >>= g)) x']
                 = xs >>= (\x -> (f x >>= g))
-}

-- 11

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
  deriving (Show)

exec :: Code -> Stack -> Stack
exec [] s            = s
exec (PUSH n:c) s    = exec c (n : s)
exec (ADD:c) (m:n:s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- exec (comp e) s = eval e : s

comp' :: Expr -> Code -> Code
comp' e c = comp e ++ c

{-
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp (Add x y) ++ c
                  = comp x ++ comp y ++ [ADD] ++ c
                  = comp x ++ (comp' y (ADD : c))
                  = comp' x (comp' y (ADD : c))
-}
