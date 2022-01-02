data Expr = Val Int | Add Expr Expr | Multi Expr Expr
type Cont = [Op]
data Op = EVALADD Expr | EVALMULTI Expr | ADD Int | MULTI Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALADD y : c)
eval (Multi x y) c = eval x (EVALMULTI y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval y (ADD n : c)
exec (EVALMULTI y : c) n = eval y (MULTI n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULTI n : c) m = exec c (n * m)

value :: Expr -> Int
value e = eval e []
