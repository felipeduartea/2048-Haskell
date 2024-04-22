module Main where

import Prelude hiding (Left, Right)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)
import Data.List (transpose)
import System.Random (randomRIO)

data Move = Up | Down | Left | Right deriving (Eq, Show)
type Grid = [[Int]]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "Bem vind  o ao 2048! Use as teclas WASD para jogar"
    start >>= gameLoop
start :: IO Grid
start = do
    let initialGrid = replicate 4 [0, 0, 0, 0]  
    gridWithOneTile <- addRandomTile initialGrid
    addRandomTile gridWithOneTile

addRandomTile :: Grid -> IO Grid -- Serve para adicionar um bloco aleatório no grid

addRandomTile grid = do
    let empty = [(x, y) | x <- [0..3], y <- [0..3], grid !! x !! y == 0]
    if null empty then
        return grid
    else do
        index <- randomRIO (0, length empty - 1)
        num <- randomRIO (1, 10 :: Int)  
        let value = if num == 1 then 4 else 2  
        let (x, y) = empty !! index
        return $ updateGrid grid (x, y) value

updateGrid :: Grid -> (Int, Int) -> Int -> Grid -- Atualização do grid
updateGrid grid (x, y) val =
    take x grid ++
    [take y (grid !! x) ++ [val] ++ drop (y + 1) (grid !! x)] ++
    drop (x + 1) grid

moveTiles :: Move -> Grid -> Grid
moveTiles Left = map merge
moveTiles Right = map (reverse . merge . reverse)
moveTiles Up = transpose . moveTiles Left . transpose
moveTiles Down = transpose . moveTiles Right . transpose

merge :: [Int] -> [Int]
merge = fixLength . foldr combine [] . filter (/= 0)
  where
    combine x [] = [x]
    combine x (y:ys) | x == y = x * 2 : ys
                     | otherwise = x : y : ys
    fixLength xs = xs ++ replicate (4 - length xs) 0

gameLoop :: Grid -> IO () -- é onde ocorre a lógica de loop do nosso jogo
gameLoop grid
    | isGameOver grid = do
        printGrid grid
        putStrLn "Fim de jogo!"
    | otherwise = do
        printGrid grid
        move <- captureMove
        let newGrid = moveTiles move grid
        if newGrid /= grid
        then addRandomTile newGrid >>= gameLoop
        else gameLoop grid

isGameOver :: Grid -> Bool
isGameOver grid = not (any (== 2048) (concat grid) || anyMovesLeft grid)

anyMovesLeft :: Grid -> Bool
anyMovesLeft grid = any (\m -> moveTiles m grid /= grid) [Up, Down, Left, Right]

printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . unwords . map show)

captureMove :: IO Move
captureMove = do
    char <- getChar
    putStrLn ""  -- Usei para fazer ele pular uma linha entre a tecla informada pelo usuário e a matriz
    case lookup char [('w', Up), ('a', Left), ('s', Down), ('d', Right)] of
        Just move -> return move
        Nothing -> do
            putStrLn "Input in W, A, S, or D."
            captureMove