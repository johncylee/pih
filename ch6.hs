-- 1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = product [1..n]

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

-- 3
(^) :: Int -> Int -> Int
_ ^ 0 = 1
a ^ b | b > 0 = a * (a Main.^ (b-1))

-- 4
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid (a-b) b
           | otherwise = euclid a (b-a)

-- 6
and :: [Bool] -> Bool
and [a] = a
and (True:xs) = Main.and xs
and (False:_) = False

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : Main.replicate (n-1) a

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs Main.!! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) | a == x = True
              | otherwise = Main.elem a xs

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8
halve :: Ord a => [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort list = merge xs ys
  where
    (h1, h2) = halve list
    xs = msort h1
    ys = msort h2

-- 9
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + (Main.sum xs)

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : Main.take (n-1) xs

last :: [a] -> a
last [x] = x
last (_:xs) = Main.last xs
