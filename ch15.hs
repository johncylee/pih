import Data.Foldable

primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- 4

fibs :: [Integer]
fibs = map fst (next (0, 1))
  where next (x, y) = (x, y) : next (y, x+y)

-- fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

-- 5

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

toTree :: [a] -> Tree a
toTree = foldr f Leaf
  where f x Leaf = Node Leaf x Leaf
        f x (Node l y r) = Node Leaf x (Node l y r)

repeat' :: a -> Tree a
repeat' x = Node Leaf x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' n t = toTree (take n (toList t))

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

-- 6

sqroot :: Double -> Double
sqroot n = head [b | (a, b) <- zip series (tail series), abs (a - b) < 0.00001]
  where
    series = iterate next 1
    next a = (a + n / a) / 2
