module Data_Instance where

import Data.List
import Data.Char

data ValorCelula = Bomba | Numero Int deriving (Eq)

data Celula = Celula{        -- Especificacao das celulas, seus valores
  valor :: ValorCelula,
  fechado :: Bool
} deriving (Eq)

data Tabuleiro = Tabuleiro{        -- Especificacao do teclado como lista dde celulas 
  celulas :: [Celula]
} deriving (Eq)

instance Show Celula where -- Funcao para especificar o print de cada celula
  show Celula { fechado = True } = " # "
  show Celula { fechado = False, valor = (Numero 0)} = "   "
  show Celula { valor = Bomba } = " * "
  show Celula { valor = (Numero x) } = " " ++ show x ++ " "

instance Show Tabuleiro where -- Instancia para printar o tabuleiro
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