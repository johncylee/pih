qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

third :: [a] -> a
-- third xs = head (tail (tail xs))
-- third xs = xs !! 2
third (_:_:x:_) = x

safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else tail xs
{-
safetail xs | null xs = []
            | otherwise = tail xs
-}
safetail [] = []
safetail xs = tail xs

(||) :: Bool -> Bool -> Bool
{-
False || False = False
_ || _ = True
-}
{-
True || _ = True
False || b = b
-}
b || c | b == c = b
       | otherwise = True

(&&) :: Bool -> Bool -> Bool
-- x && y = if x then if y then True else False else False
x && y = if x then y else False

mult :: Int -> Int -> Int -> Int
-- mult x y z = x * y * z
mult = \x -> (\y -> (\z -> x * y * z))

rotate :: Int -> Int
rotate n = if n > 9 then n - 9 else n

luhnDouble :: Int -> Int
luhnDouble x = rotate (2 * x)

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [luhnDouble a,
                    rotate b,
                    luhnDouble c,
                    rotate d] `mod` 10 == 0
