import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

size :: Int
size = 3

type Grid = [[Player]]
data Player = O | B | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os > xs then X else O
  where
    flat = concat g
    os = length (filter (== O) flat)
    xs = length (filter (== X) flat)

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0..size-1] ]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = mapM_ print

valid :: Grid -> Int -> Bool
valid g i = i >= 0 && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop (xs ++ (p : ys)) | valid g i]
  where
    flatg = concat g
    xs = take i flatg
    ys = drop (i + 1) flatg

chop :: [Player] -> Grid
chop [] = []
chop ps = take size ps : chop (drop size ps)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  putGrid g
  i <- getNat (show p ++ ": ")
  case move g i p of
    [] -> do
      putStrLn "Invalid move"
      run g p
    [g'] ->
      if wins p g'
        then putStrLn (show p ++ " won!")
        else if full g'
               then putStrLn "Draw"
               else run g' (next p)

data Tree a = Node a [Tree a]
  deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. size ^ 2 - 1]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

play :: Grid -> Player -> IO ()
play g p = do
  -- cls
  -- goto (1,1)
  putGrid g
  play' g p

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do
                    putStrLn "ERROR: Invalid move"
                    play' g p
                  [g'] -> play g' (next p)
  | p == X = do putStr "Player X is thinking..."
                pick <- randomRIO (0, length bestmoves - 1)
                (play $! (bestmoves !! pick)) (next p)
                  where
                    bestmoves = bestmove' g p

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

-- 1
lengthTree :: Tree a -> Int
lengthTree (Node _ ts) = 1 + sum (map lengthTree ts)

-- 2
bestmove' :: Grid -> Player -> [Grid]
bestmove' g p = [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree
