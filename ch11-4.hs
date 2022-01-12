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

minimax :: Player -> Tree Grid -> Tree (Grid, Player, Int)
minimax _ (Node g [])
  | wins O g = Node (g, O, 0) []
  | wins X g = Node (g, X, 0) []
  | otherwise = Node (g, B, 0) []
minimax h (Node g ts)
  | turn g == O =
    if h == O then Node (g, pO, dO + 1) ts'
    else Node (g, pO, dO + 1) (filter (\(Node (_, _, d) _) -> d == dO) tsO)
  | turn g == X =
    if h == X then Node (g, pX, dX + 1) ts'
    else Node (g, pX, dX + 1) (filter (\(Node (_, _, d) _) -> d == dX) tsX)
  where
    ts' = map (minimax h) ts
    ps = [p' | Node (_, p', _) _ <- ts']
    pO = minimum ps
    pX = maximum ps
    tsO = filter (\(Node (_, p, _) _) -> p == pO) ts'
    tsX = filter (\(Node (_, p, _) _) -> p == pX) ts'
    dO = minimum [d | Node (_, _, d) _ <- tsO]
    dX = minimum [d | Node (_, _, d) _ <- tsX]

type MinimaxTree = Tree (Grid, Player, Int)

bestmove :: MinimaxTree -> MinimaxTree
bestmove (Node (_, _, _) ts) = head ts

-- human -> tree -> current_player
play :: Player -> MinimaxTree -> Player -> IO ()
play h t p = case t of
               (Node (g, _, _) _) -> do
                 putGrid g
                 play' h t p

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

gridEqual :: Grid -> MinimaxTree -> Bool
gridEqual x (Node (g,_,_) _) = x == g

findGrid :: Grid -> [MinimaxTree] -> MinimaxTree
findGrid g ts =
  case find (gridEqual g) ts of
    Just t -> t

play' :: Player -> MinimaxTree -> Player -> IO ()
play' h (Node (g, best, d) ts) p
  | d == 0 && best == B = putStrLn "It's a draw!\n"
  | d == 0 = putStrLn ("Player " ++ show best ++ " wins!\n")
  | p == h = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          play' h (Node (g, best, d) ts) p
        [g'] -> play h (findGrid g' ts) (next p)
  | otherwise = do
      putStrLn ("Player " ++ show p ++ " is thinking...")
      play h t' (next p)
      where t' = bestmove (Node (g, best, d) ts)

askFirst :: IO Player
askFirst = do putStr "Play first? "
              c <- getChar
              putChar '\n'
              case c of
                'y' -> return O
                'n' -> return X
                _ -> askFirst

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          human <- askFirst
          play human (minimax human (gametree empty O)) O
