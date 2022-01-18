{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

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

item :: Parser Char
item = P (\case
             [] -> []
             xs -> [(last xs, init xs)])

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
string s = do char x
              string xs
              return (xs ++ [x])
                where
                  x = last s
                  xs = init s

nat :: Parser Int
nat = do xs <- some digit
         return (read (reverse xs))

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

symbol :: String -> Parser String
symbol xs = token (string xs)

-- 8.a

-- GRAMMER: expr ::= (expr - | ϵ) nat
--          nat ::= 0 | 1 | 2 |...

expr :: Parser Int
expr = do n <- natural
          (do symbol "-"
              e <- expr
              return (e - n))
            <|> return n
