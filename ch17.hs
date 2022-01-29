-- Wrong answer. Refer to [39].

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                   Just n -> case eval y of
                               Just m -> Just (n + m)
                               Nothing -> Nothing
                   Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                     Just n -> Just n
                     Nothing -> eval h

{-

Given:

exec (comp e) s = eval e : s

exec (comp' e c) s = exec c (eval e : s)

Calculate comp' and comp:

exec (comp' (Val n) c) s = exec c (eval (Val n) : s)
                         = exec c (Just n : s)

Let PUSH :: Maybe Int -> Code -> Code
exec (PUSH (Just n) c) s = exec c (Just n : s)  -- (1)

comp' (Val n) c = PUSH (Just n) c  -- (2)


exec (comp' (Add x y) c) s = exec c (eval (Add x y) : s)
                           = exec c (add (eval x) (eval y) : s)  -- (3)

Let ADD :: Code -> Code
exec (ADD c) (m : n : s) = exec c (add n m : s)  -- (4)

exec c (add (eval x) (eval y) : s)  -- cont. (3)
= exec (ADD c) (eval y : eval x : s)
= exec (comp' y (ADD c)) (eval x : s)
= exec (comp' x (comp' y (ADD c))) s

comp' (Add x y) c = comp' x (comp' y (ADD c))  -- (5)

exec (comp' Throw c) s = exec c (eval Throw : s)
                       = exec c (Just n : s)

comp' Throw c = PUSH Nothing c  -- (6)

exec (comp' (Catch x h) c) s = exec c (eval (Catch x h) : s)
                             = exec c (catch (eval x) (eval h) : s)  -- (7)

Let CATCH :: Code -> Code
exec (CATCH c) (m : n : s) = exec c (catch n m : s)  -- (8)

exec c (catch (eval x) (eval h) : s)  -- cont. (7)
= exec (CATCH c) (eval h : eval x : s)
= exec (comp' h (CATCH c)) (eval x : s)
= exec (comp' x (comp' h (CATCH c))) s

comp' (Catch x h) c = comp' x (comp' h (CATCH c))  -- (9)

exec (comp e) s = eval e : s
                = exec HALT (eval e : s)
                = exec (comp' e HALT) s  -- (10)
-}

data Code = HALT | PUSH (Maybe Int) Code | ADD Code | CATCH Code
type Stack = [Maybe Int]

comp :: Expr -> Code
comp e = comp' e HALT                              -- (10)

comp' :: Expr -> Code -> Code
comp' (Val n) c     = PUSH (Just n) c              -- (2)
comp' (Add x y) c   = comp' x (comp' y (ADD c))    -- (5)
comp' Throw c       = PUSH Nothing c               -- (6)
comp' (Catch x h) c = comp' x (comp' h (CATCH c))  -- (9)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n : s)                 -- (1)
exec (ADD c) (m:n:s) = exec c (add n m : s)        -- (4)
  where
    add :: Maybe Int -> Maybe Int -> Maybe Int
    add Nothing _ = Nothing
    add _ Nothing = Nothing
    add (Just x) (Just y) = Just (x + y)
exec (CATCH c) (m:n:s) = exec c (catch n m : s)    -- (8)
  where
    catch :: Maybe Int -> Maybe Int -> Maybe Int
    catch Nothing h = h
    catch (Just x) _ = Just x
