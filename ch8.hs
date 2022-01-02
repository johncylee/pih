{-# LANGUAGE UnicodeSyntax #-}


-- type, data, newtype, class

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

type Assoc k v = [(k, v)]  -- lookup tables k -> v

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Eq Prop Prop

type Subst = Assoc Char Bool  -- substitution table Char -> Bool

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eq p q) = vars p ++ vars q

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A')(Var 'B'))) (Var 'B')

find :: Eq k => k -> Assoc k v -> v
find x ((c,b):xs) | x == c = b
                  | otherwise = find x xs
-- find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q -- False iff False => True, 0 < 1
eval s (Or p q) = eval s p || eval s q
eval s (Eq p q) = eval s p == eval s q

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 1

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

-- 2

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
  (Node (Leaf 6) 7 (Leaf 9))

occurs_1 :: Ord a => a -> Tree a -> Bool
occurs_1 x (Leaf y) = x == y
occurs_1 x (Node l y r) | x == y = True
                        | x < y = occurs_1 x l
                        | otherwise = occurs_1 x r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r

-- 3

data TreeB a = LeafB a | NodeB (TreeB a) (TreeB a)

leaves :: TreeB a -> Int
leaves (LeafB _) = 1
leaves (NodeB l r) = leaves l + leaves r

balanced :: TreeB a -> Bool
balanced (LeafB _) = True
balanced (NodeB l r) = abs (leaves l - leaves r) <= 1
                       && balanced l && balanced r

-- 4

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> TreeB a
balance [x] = LeafB x
balance xs = NodeB (balance l) (balance r)
  where (l, r) = halve xs

-- 5

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add l r) = g (folde f g l) (folde f g r)

-- 6

evale :: Expr -> Int
evale = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7

class MyEq a where
  (==.), (/=.) :: a -> a -> Bool
  x /=. y = not (x ==. y)

data MyMaybe a = MyNothing | MyJust a

instance Eq a => MyEq (MyMaybe a) where
  MyNothing ==. MyNothing = True
  MyJust x ==. MyJust y = x == y
  _ ==. _ = False

instance Eq a => MyEq [a] where
  [] ==. [] = True
  (x:xs) ==. (y:ys) = x == y && xs ==. ys
  _ ==. _ = False

-- 8 (Or and Eq)

-- 9 ch8-9.hs
