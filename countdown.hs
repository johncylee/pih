-- subsets
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- permutations
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- all permutations from all subsets
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- ex.1
choices' :: [a] -> [[a]]
choices' ns = [ ps | ss <- subs ns, ps <- perms ss ]

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add l r = l + r
apply Sub l r = l - r
apply Mul l r = l * r
apply Div l r = l `div` r

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- all splits
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- all expressions
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- 1st improvement
-- moved the "valid" check from eval to combine'
type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r,apply o x y) | o <- ops, valid' o x y]

-- 2nd improvement
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- ex.2
removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y:ys) | x == y = ys
                   | otherwise = y:removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)

-- ex.3: non-termination

-- ex.4
-- length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns']
-- length [e | ns' <- choices [1,3,7,10,25,50], e <- exprs ns', eval e /= []]

-- ex.5
-- valid Add _ _ = True
-- valid Sub _ _ = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

-- ex.6


-- main
main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)
