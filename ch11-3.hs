import Data.Char
import Data.List
import System.IO

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

minimax :: Tree Grid -> Tree (Grid, Player, Int)
minimax (Node g [])
  | wins O g = Node (g, O, 0) []
  | wins X g = Node (g, X, 0) []
  | otherwise = Node (g, B, 0) []
minimax (Node g ts)
  | turn g == O = Node (g, pO, depthO + 1) ts'
  | turn g == X = Node (g, pX, depthX + 1) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p, _) _ <- ts']
    pO = minimum ps
    pX = maximum ps
    depthO = minimum [d | Node (_, p, d) _ <- ts', p == pO]
    depthX = minimum [d | Node (_, p, d) _ <- ts', p == pX]

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p', d') _ <- ts, p' == best, d' == d - 1]
  where
    tree = prune depth (gametree g p)
    Node (_, best, d) ts = minimax tree

play :: Grid -> Player -> IO ()
play g p = do
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
  | p == X = do putStrLn "Player X is thinking..."
                (play $! bestmove g p) (next p)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O
