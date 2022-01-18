{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item = P (\case
             [] -> []
             (x:xs) -> [(x,xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P (\inp -> case parse p inp of
                 [] -> []
                 [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                    [] -> []
                    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [x] -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = (do char '-'
          n <- nat
          return (-n))
      <|> nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- 1

comment :: Parser ()
comment = do
  char '-'
  char '-'
  sat (/= '\n')
  return ()

-- 2

-- expr ::= expr + expr | term
-- term ::= term * term | factor
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 | ...

-- (2 + 3) + 4 or 2 + (3 + 4)

-- 3

-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= ( expr ) | nat
-- nat ::= 0 | 1 | 2 | ...

-- expr - (term + expr)
-- term - factor - nat - 2
-- expr - term - factor - nat - 3

-- expr - term - (factor * term) - (factor * (factor * term))
-- factor - nat - 2
-- factor - nat - 3
-- term - factor - nat - 4

-- expr - (term + expr) - (factor + expr) - ((expr) + expr)
--   - ((term + expr)) + expr
-- term - factor - nat - 2
-- expr - term - factor - nat - 3
-- expr - term - factor - nat - 4

-- 4

-- fail term + expr , then term
-- fail factor * term, then factor
-- fail ( expr ), then nat...

-- 5
type Expr = Int

expr :: Parser Expr
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e))
            <|>
            (do symbol "-"
                e <- expr
                return (t - e))
            <|> return t

term :: Parser Expr
term = do e <- expo
          (do symbol "*"
              t <- term
              return (e * t))
            <|>
            (do symbol "/"
                t <- term
                return (e `div` t))
            <|> return e

expo :: Parser Expr
expo = do f <- factor
          (do symbol "^"
              e <- expo
              return (f ^ e))
            <|> return f

factor :: Parser Expr
factor = (do symbol "("
             e <- expr
             symbol ")"
             return e)
         <|> integer

-- 6 same as 5

-- 7 same as 5

-- 8 ch13-8.hs, ch13-8-alt.hs

-- 9 ch13-9.hs
