import Data.Char

map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : Main.map f xs

filter :: (a -> Bool) -> [a] -> [a]
-- filter p xs = [ x | x <- xs, p x ]

-- reverse order (little endian)

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- 1
-- map f (filter p xs)

-- 2
all :: (a -> Bool) -> [a] -> Bool
all p = and . Main.map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . Main.map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x : Main.takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = Main.dropWhile p xs
                   | otherwise = x:xs

-- 3
-- map f = foldr (\x xs -> f x : xs) []
filter p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- 5
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

-- 6: p 是停止條件, h 是 step, t 是轉換
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id

-- 7

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . Main.map (addparity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode :: [Bit] -> String
decode = Main.map (chr . bin2int . chkparity) . chop9

channel :: a -> a
channel = id

count :: Eq a => a -> [a] -> Int
count x = length . Main.filter (== x)

parity :: [Bit] -> Bit
parity xs | odd (count 1 xs) = 1
          | otherwise = 0

addparity :: [Bit] -> [Bit]
addparity xs = parity xs : xs

chkparity :: [Bit] -> [Bit]
chkparity [] = []
chkparity (x:xs) | x == parity xs = xs
                 | otherwise = error "parity error"

transmit :: String -> String
transmit = decode . channel . encode

-- 8
faultyChannel :: [a] -> [a]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

-- 10
luhnDouble :: Int -> Int
luhnDouble n | x > 9 = x - 9
             | otherwise = x
             where x = 2 * n

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
