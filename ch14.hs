import Data.Foldable

-- 1

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   mappend (x1, y1) (x2, y2) = (mappend x1 x2, mappend y1 y2)

-- 2

-- instance Monoid b => Monoid (a -> b) where
--   mempty = const mempty
--   mappend f g = \x -> f x `mappend` g x

-- 3

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Foldable Maybe' where
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing' = mempty
  fold (Just' x) = x
  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x
  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ z Nothing' = z
  foldr f z (Just' x) = f x z
  -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ z Nothing' = z
  foldl f z (Just' x) = f z x

instance Traversable Maybe' where
  -- traverse :: Applicative f => (a -> f b) -> Maybe' a -> f (Maybe' b)
  traverse _ Nothing' = pure Nothing'
  traverse g (Just' x) = Just' <$> g x

-- 4

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l
  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ z Leaf = z
  foldl f z (Node l x r) = foldl f (f (foldl f z l) x) r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse g (Node l x r) = Node <$> traverse g l <*> g x <*> traverse g r

-- 5

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p ta = filter p (foldMap (: []) ta)
-- filterF p ta = filter p (toList ta)
