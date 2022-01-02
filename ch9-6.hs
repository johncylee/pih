import Data.List

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
choices ns = [ ps | ss <- subs ns, ps <- perms ss ]

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x > 0 && x <= y
valid Sub x y = y > 0 && x > y
valid Mul x y = x > 1 && y > 1 && x <= y
valid Div x y = x > 1 && y > 1 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add l r = l + r
apply Sub l r = l - r
apply Mul l r = l * r
apply Div l r = l `div` r
apply Exp l r = l ^ r

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

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

type Result = (Expr,Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) =
  [(App o l r,apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine lx ry]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]

step :: Result -> [Result] -> [Result]
step r [] = [r]
step (e,a) ((re,ra):rs) | a < ra = [(e,a)]
                        | a == ra = (e,a):(re,ra):rs
                        | otherwise = (re,ra):rs

nearestSolutions :: [Int] -> Int -> [Expr]
nearestSolutions ns n =
  map fst (
  foldr step [] [(e, abs (m - n)) | ns' <- choices ns, (e,m) <- results ns'])

-- main
main :: IO ()
main = print (nearestSolutions [1,3,7,10,25,50] 765)
