import Data.Char

lowers :: String -> Int
lowers cs = sum [1 | c <- cs, c >= 'a' && c <= 'z']

-- ex.10
let2int :: Char -> Char -> Int
let2int a c = ord c - ord a

int2let :: Char -> Int -> Char
int2let a n = chr (ord a + n)

shift :: Int -> Char -> Char
shift n c | isAsciiLower c = int2let 'a' ((let2int 'a' c + n) `mod` 26)
          | isAsciiUpper c = int2let 'A' ((let2int 'A' c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

q1 = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
  where ns = [1..n]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum [y | y <- factors x, y /= x]]

q7 = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0, 1..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
