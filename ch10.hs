import Data.Char
import System.IO
import Control.Monad

strlen :: IO ()
strlen = do
  putStr "Enter a string: "
  xs <- getLine
  putStr "The string has "
  putStr (show (length xs))
  putStrLn " characters"

-- hangman
hangman :: IO ()
hangman = do
  putStr "Think of a word: "
  word <- sGetLine
  putStrLn "Try to guess it:"
  playH word

sGetLine :: IO String
sGetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sGetLine
      return (x : xs)

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

playH :: String -> IO ()
playH word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!!"
    else do
      putStrLn (match word guess)
      playH word

match :: String -> String -> String
match xs ys = [ if x `elem` ys then x else '-' | x <- xs ]

-- nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initBoard :: Board
initBoard = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = putStrLn (show row ++ ": " ++ concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = mapM_ (\(r, n) -> putRow r n) (zip [1 ..] board)

getDigit :: String -> IO Int
getDigit prompt = do
  putStrLn prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit!"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

playN :: Board -> Int -> IO ()
playN board player = do
  newline
  putBoard board
  if finished board then do
    newline
    putStrLn ("Player " ++ show (next player) ++ " wins!!")
  else do
    newline
    putStrLn ("Player " ++ show player)
    row <- getDigit "Enter a row number: "
    num <- getDigit "Stars to remove: "
    if valid board row num then
      playN (move board row num) (next player)
    else do
      newline
      putStrLn "ERROR: Invalid move"
      playN board player

nim :: IO ()
nim = playN initBoard 1

-- life
type Torus = [[Int]]

initTorus :: Torus
initTorus =
  [ [0, 0, 0, 0, 0]
  , [0, 0, 0, 1, 0]
  , [0, 1, 0, 1, 0]
  , [0, 0, 1, 1, 0]
  , [0, 0, 0, 0, 0]
  ]

getCell :: Torus -> Int -> Int -> Int
getCell torus x y = torus !! mod y 5 !! mod x 5

surround :: Torus -> Int -> Int -> Int
surround torus x y =
  sum [getCell torus (x + dx) (y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]] -
  getCell torus x y

nextCell :: Torus -> Int -> Int -> Int
nextCell torus x y
  | getCell torus x y == 1 && (surroundBy == 2 || surroundBy == 3) = 1
  | getCell torus x y == 0 && surroundBy == 3 = 1
  | otherwise = 0
  where
    surroundBy = surround torus x y

nextTorus :: Torus -> Torus
nextTorus torus = [[nextCell torus x y | x <- [0..4]] | y <- [0..4]]

showTorus :: Torus -> IO ()
showTorus torus = do
  mapM_ putStrLn [map mapCell row | row <- torus ]
  where mapCell x | x == 1 = '*'
                  | otherwise = ' '

-- 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 2
putBoard' :: Board -> IO ()
putBoard' = rBoard 1
  where
    rBoard :: Int -> Board -> IO ()
    rBoard _ [] = return ()
    rBoard n (x:xs) = do
      putRow n x
      rBoard (n + 1) xs

-- 3 just use putBoard

-- 4
adder :: IO ()
adder = do
  putStr "How many numbers? "
  x <- getLine
  xs <- replicateM (read x :: Int) getLine
  putStrLn ("The total is " ++ show (sum (map (\x -> read x :: Int) xs)))

-- 5 already done in 4

-- 6
readLineRaw :: IO String
readLineRaw = do
  x <- getCh
  if x == '\n' then do
    putChar x
    return []
  else if x == '\DEL' then do
    putStr "\b \b"
    xs <- readLineRaw
    return (x : xs)
  else do
    putChar x
    xs <- readLineRaw
    return (x : xs)

readLine :: IO String
readLine = do
  xs <- readLineRaw
  return (foldl handleDEL "" xs)

handleDEL :: String -> Char -> String
handleDEL [] '\DEL' = []
handleDEL xs '\DEL' = init xs
handleDEL xs x = xs ++ [x]
