module Main where

import Prelude hiding (Left, Right)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin)
import Data.List (transpose)
import System.Random (randomRIO)

data Movimento = Cima | Baixo | Esquerda | Direita deriving (Eq, Show)
type Tabuleiro = [[Int]]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "Bem vindo ao 2048! Use as teclas WASD para jogar"
    iniciar >>= loopDeJogo

iniciar :: IO Tabuleiro
iniciar = do
    let gridInicial = replicate 4 [0, 0, 0, 0]  
    gridComUmBloco <- adicionarBlocoAleatorio gridInicial
    adicionarBlocoAleatorio gridComUmBloco

adicionarBlocoAleatorio :: Tabuleiro -> IO Tabuleiro
adicionarBlocoAleatorio tabuleiro = do
    let vazios = [(x, y) | x <- [0..3], y <- [0..3], tabuleiro !! x !! y == 0]
    if null vazios then
        return tabuleiro
    else do
        indice <- randomRIO (0, length vazios - 1)
        num <- randomRIO (1, 10 :: Int)  
        let valor = if num == 1 then 4 else 2  
        let (x, y) = vazios !! indice
        return $ atualizarTabuleiro tabuleiro (x, y) valor

atualizarTabuleiro :: Tabuleiro -> (Int, Int) -> Int -> Tabuleiro
atualizarTabuleiro tabuleiro (x, y) valor =
    take x tabuleiro ++
    [take y (tabuleiro !! x) ++ [valor] ++ drop (y + 1) (tabuleiro !! x)] ++
    drop (x + 1) tabuleiro

moverBlocos :: Movimento -> Tabuleiro -> Tabuleiro
moverBlocos Esquerda = map juntar
moverBlocos Direita = map (reverse . juntar . reverse)
moverBlocos Cima = transpose . moverBlocos Esquerda . transpose
moverBlocos Baixo = transpose . moverBlocos Direita . transpose

juntar :: [Int] -> [Int]
juntar = corrigirTamanho . foldr combinar [] . filter (/= 0)
  where
    combinar x [] = [x]
    combinar x (y:ys) | x == y = x * 2 : ys
                      | otherwise = x : y : ys
    corrigirTamanho xs = xs ++ replicate (4 - length xs) 0

loopDeJogo :: Tabuleiro -> IO ()
loopDeJogo tabuleiro
    | jogoTerminado tabuleiro = do
        imprimirTabuleiro tabuleiro
        putStrLn "Fim de jogo!"
    | otherwise = do
        imprimirTabuleiro tabuleiro
        movimento <- capturarMovimento
        let novoTabuleiro = moverBlocos movimento tabuleiro
        if novoTabuleiro /= tabuleiro
        then adicionarBlocoAleatorio novoTabuleiro >>= loopDeJogo
        else loopDeJogo tabuleiro

jogoTerminado :: Tabuleiro -> Bool
jogoTerminado tabuleiro = not (any (== 2048) (concat tabuleiro) || movimentosRestantes tabuleiro)

movimentosRestantes :: Tabuleiro -> Bool
movimentosRestantes tabuleiro = any (\m -> moverBlocos m tabuleiro /= tabuleiro) [Cima, Baixo, Esquerda, Direita]

imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro = mapM_ (putStrLn . unwords . map show)

capturarMovimento :: IO Movimento
capturarMovimento = do
    caractere <- getChar
    putStrLn ""  -- Usado para pular uma linha entre a tecla informada pelo usuário e o tabuleiro
    case lookup caractere [('w', Cima), ('a', Esquerda), ('s', Baixo), ('d', Direita)] of
        Just mov -> return mov
        Nothing -> do
            putStrLn "Digite W, A, S ou D."
            capturarMovimento
