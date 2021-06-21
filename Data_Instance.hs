module Data_Instance where

import Data.List
import Data.Char

-- Valor da celula pode ser bomba ou valor inteiro sendo em branco o valor 0.
data ValorCelula = Bomba | Numero Int deriving (Eq)

-- Especificacao das celulas, seus valores
data Celula = Celula{
  valor :: ValorCelula,
  fechado :: Bool
} deriving (Eq)

-- Especificacao do tabuleiro como lista de celulas 
data Tabuleiro = Tabuleiro{
  celulas :: [Celula]
} deriving (Eq)

-- Instance para o print de cada celula
instance Show Celula where
  show Celula { fechado = True } = " # "
  show Celula { fechado = False, valor = (Numero 0)} = "   "
  show Celula { valor = Bomba } = " * "
  show Celula { valor = (Numero x) } = " " ++ show x ++ " "

-- Instancia para printar o tabuleiro
instance Show Tabuleiro where
  show b = showTopo 20 ++ showCelulas (celulas b) 0 where
    linha n = ' ' : show n
    showTopo n = "\n\n  " ++ foldl' (\a b -> a ++ linha b ++ " ") "  " [0..9] ++ foldl' (\a b -> a ++ linha b ++ " ") "" [0..9] ++ "\n" --printa a primeira linha com os numeros de indicacao de colunas

    showCelulas [] _ = ""
    showCelulas ts n = foldl' (++) ( linha n ++ " |") (map show linha2) ++ "\n" ++ showCelulas resto (if n+1 < 10 then n + 1 else (n+1) - 10) where (linha2, resto) = splitAt 20 ts -- printa os numeros de indicao as linhas e o resto do tabuleiro linha por linha

-- Lista para a escolha dos vizinhos de cada posicao (x+1, x+(-1), x+0) com (y+(-1), y+1, y+0)
vizinhos :: [(Int, Int)]
vizinhos = [(-1,0), (1,0), (0,0), (0,1), (-1,1), (1,1), (-1,-1), (1,-1), (0,-1)]

-- Transforma a entrada em uma tupla
conversortupla :: String -> (Int,Int)
conversortupla string = read string :: (Int,Int)

-- Printar string sem aspas
printsa :: (Show string) => [[string]] -> IO()
printsa = mapM_ (putStrLn . filter (/= '"') . show)
