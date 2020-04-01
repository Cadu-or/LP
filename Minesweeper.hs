module Main(main) where

import System.Random
import Data.List
import Data.Char


-- ################### Datas ##################### 

data ValorCelula = Bomba | Numero Int
                        deriving (Eq)


-- ##################################################################################

data Celula = Celula {        -- Especificacao das celulas, seus valores
  valor :: ValorCelula,
  fechado :: Bool
  
} deriving (Eq)

data Tabuleiro = Tabuleiro {        -- Especificacao do teclado como lista dde celulas 
  celulas :: [Celula]

} deriving (Eq)

-- ###########################################################################################

-- ################### Instance ##############################################################

instance Show Celula where -- Funcao para especificar o print de cada celula
  show Celula { fechado = True } = " # "
  show Celula { fechado = False, valor = (Numero 0)} = "   "
  show Celula { valor = Bomba } = " * "
  show Celula { valor = (Numero x) } = " " ++ show x ++ " "

instance Show Tabuleiro where -- Instancia para printar o tabuleiro
  show b = showTopo 20 ++ showCelulas (celulas b) 0 where
    align3 n = ' ' : show n
    showTopo n = "\n\n  " ++ foldl' (\a b -> a ++ align3 b ++ " ") "  " [0..9] ++ foldl' (\a b -> a ++ align3 b ++ " ") "" [0..9] ++ "\n" --printa a primeira linha com os numeros de indicacao de colunas
        
    showCelulas [] _ = ""
    showCelulas ts n = foldl' (++) ( align3 n ++ " |") (map show linha) ++ "\n" ++ showCelulas resto (if n+1 < 10 then n + 1 else (n+1) - 10) where (linha, resto) = splitAt 20 ts -- printa os numeros de indicao as linhas e o resto do tabuleiro linha por linha
  
-- #########################################################################################

-- ################# Verificacao de Celula Separadas #######################################

novaCelula :: Celula      -- Criacao de celula normal 
novaCelula = Celula { valor = Numero 0, fechado = False}
  
novaBomba :: Celula     -- Criacao de bomba
novaBomba = Celula { valor = Bomba, fechado = False}
  
adicionarCelula :: Celula -> Int -> Celula    --Adicionar celula no tabuleiro
adicionarCelula (Celula Bomba fechado) _ = Celula Bomba True
adicionarCelula (Celula (Numero x) fechado) n = Celula (Numero (x+n)) True

verificaBomba :: Maybe Celula -> Bool         -- Verifica se a celula eh bomba 
verificaBomba (Just celula) = (valor celula == Bomba)
verificaBomba _ = False
  
retiraCelula :: Tabuleiro -> Int -> Int -> Maybe Celula -- Retira a celula para verificacao de implementacao no tabuleiro
retiraCelula b y x 
  | y >= 0 && x >= 0 && y < 20 && x < 20  = Just $ celulas b !! (x*20 + y) -- Condicao para verificacao da celula
  | otherwise = Nothing

retiraCelula2 :: Tabuleiro -> Int -> Celula    -- Retiraa celula so que retorna uma celula ao inves de uma maybe
retiraCelula2 tabuleiro posicao = celulas tabuleiro !! posicao

verificaZero :: Maybe Celula -> Bool  -- Verifica se a celula eh zero, para verificacao de expandir 0
verificaZero (Just t) = (valor t == Numero 0) && not (fechado t)
verificaZero _= False

verificarFechado :: Celula -> Bool   -- Verifica se a Celula esta fechada
verificarFechado (Celula v f)
  | f == True = True
  | otherwise = False

verificaAbertoeZero :: Celula -> Bool  -- Verifica se a celula aberta eh zero
verificaAbertoeZero (Celula v f)
  | v == (Numero 0) && f == False = True
  | otherwise = False

verificaBombaFechada :: Celula -> Bool -- Verfifica se a bomba esta fechada
verificaBombaFechada (Celula v f)
  | v == (Bomba) = True && f == True
  | otherwise = False

-- ############################ VERIFICACAO DE ESTADO DO JOGO##################################

verificaPerdeu :: Celula -> Bool     -- Verifica se a pessoa perdeu o jogo
verificaPerdeu (Celula v f)
  | v == Bomba = True
  | otherwise = False

percorreTabuleiro2 :: Tabuleiro -> Int -> Int -> Int  -- percorre o tabuleiro paraa verificar as celulas fechadas e retorna o inteiro de ganhou
percorreTabuleiro2 (Tabuleiro tabuleiro) contador posicao = loop tabuleiro contador posicao where
  loop _ contador 0 = contador
  loop (l:ls) contador posicao = if verificarFechado l == True then loop ls (contador+1) (posicao-1) else loop ls contador (posicao-1)

-- #################### Construcao do Tabuleiro ##########################################

novoTabuleiro :: Tabuleiro                                                        --Criacao do tabuleiro de celulas
novoTabuleiro = Tabuleiro {celulas = [novaCelula| i <- [1..20], j <- [1..20]]}

buildTabuleiro :: StdGen -> Tabuleiro
buildTabuleiro rng = setValor $ bombasAleatorias novoTabuleiro 40 rng             -- Criacao do tabuleiro

bombasAleatorias :: Tabuleiro -> Int -> StdGen -> Tabuleiro                       --Colocar bombas no tabuleiro
bombasAleatorias tabuleiro qnt gen = let
                                  loop x _ 0 = x
                                  loop (x:xs) rng qnt                      -- Recebe um valor muito pequeno, quanto mais o loop acontece maior a possibilidade de bombas aparecerem
                                    | rngValue < (fromIntegral (qnt) / fromIntegral (length (x:xs))) = novaBomba : loop xs rng' (qnt - 1) 
                                    | otherwise = x : loop xs rng' qnt     
                                    
                                    where (rngValue, rng') = randomR (0, 1) rng :: (Double, StdGen)
          
                                  tabuleiro' = loop (celulas tabuleiro) gen qnt
                                in
                                  Tabuleiro tabuleiro'                      -- Rettorna o tabuleiro com as bombas


  
setValor :: Tabuleiro -> Tabuleiro
setValor tabuleiro = let
                      nextCoord x y     -- Escolher proxima coordenada
                        | x == 19 = (0, y + 1)    -- Se o x estiver preenchido, passa para linha a baixo
                        | otherwise  = (x + 1, y) -- Incrementa o x
                      verificaProx x y vizinhos = retiraCelula tabuleiro (x + fst vizinhos) (y + snd vizinhos)  -- Testar laterais e diagonais com o auxilio da lista "vizinhos"
                   
                      loop [] _ _ = [] -- Verificacao de cada elemento da lista de posicoes
                      loop (l:ls) x y = adicionarCelula l (length (filter verificaBomba $ map (verificaProx x y) vizinhos)) : loop ls x' y' where (x', y') = nextCoord x y
                                          -- adiciona | posicao | inteiro | verifica se a lista criado pelo map das posicoes proximas sao bombas | "set" uma proximacoordenada
                      tabuleiro' = loop (celulas tabuleiro) 0 0
                     in
                      Tabuleiro tabuleiro' -- Retorna Tabuleiro


vizinhos :: [(Int, Int)]    -- Lista para a escolha dos vizinhos de cada posicao (x+1, x+(-1), x+0) com (y+(-1), y+1, y+0) 
vizinhos = [(-1,0), (1,0), (0,0), (0,1), (-1,1), (1,1), (-1,-1), (1,-1), (0,-1)]

-- ##################################################################################
-- ####################### Funcionalidade do jogo ###################################

abreCelula :: Celula -> Celula                  -- Abre a celula
abreCelula (Celula v _) = Celula v False        

percorreTabuleiro :: Tabuleiro -> Int -> Tabuleiro    -- Percorre o tabuleiro paraa encontrar a celula que deve ser aberta
percorreTabuleiro (Tabuleiro tabuleiro) posicao = Tabuleiro (loop tabuleiro posicao) where
  loop [] _ = []
  loop (l:ls) 0 = abreCelula l:ls
  loop (l:ls) posicao = l : loop ls (posicao-1)

movimentacao :: Tabuleiro -> Int -> Tabuleiro      -- Recebe a posicao e o tabuleiro e manda para a funcao de percorrer para achar a posicao correta
movimentacao tabuleiro posicao = percorreTabuleiro tabuleiro posicao
          
oitoMovimentos0s :: Tabuleiro -> Tabuleiro                           
oitoMovimentos0s tabuleiro = let   
                      nextCoord x y                         -- Escolhe as proximas coordenadas
                        | x == 19 = (0, y + 1)
                        | otherwise  = (x + 1, y)                 
                      verificaProx x y vizinhos = retiraCelula tabuleiro (x + fst vizinhos) (y + snd vizinhos) -- Verifica de acordo com a lista dos 8 vizinhos
            
                      loop []_ _= []
                      loop (Celula (Numero n) fechado:ts) x y -- Receber a primeira celula da lista de celulas
                        | any verificaZero $ map (verificaProx x y) vizinhos = Celula (Numero n) False : loop ts x' y' where (x', y') = nextCoord x y
                          -- Any retorna verdadeiro se houver pelo menos um do valor especificado, Celula recebe fechado como falso | set proxima coordenada
                      loop (t:ts) x y = t : loop ts x' y' where (x', y') = nextCoord x y
        
                      tabuleiro' = loop (celulas tabuleiro) 0 0 -- Inicia a funcoo loop com as coordenadas 0 0
                    in
                      Tabuleiro tabuleiro' -- Retorna o tabuleiro
-- ###################################################################################

loop8Movimentos :: Int -> Tabuleiro -> Tabuleiro --Funcao recursiva para abrir os vizinhos do zero
loop8Movimentos n tabuleiro
  | n == 0 = tabuleiro
  | n > 0 = loop8Movimentos (n-1) $ oitoMovimentos0s tabuleiro

conversortupla :: String -> (Int,Int)       -- Funcao para pegar entrada e tranformar em tupla
conversortupla string = read string :: (Int,Int)

loopJogo :: Tabuleiro -> IO ()                 -- Loop de inicializacao do jogo
loopJogo tabuleiro = do
          print tabuleiro
          print "Digite as coordenadas (x,y) :"
          coord <- getLine                      --Funcao que le do tecladoas coordenadas
          let
            tupla = conversortupla coord
            x = fst tupla                       --Separa as coordenadas x e y das tuplas
            y = snd tupla
          let
            posicao = ((x * 20) + y)
            tabuleiro' = movimentacao tabuleiro posicao                               -- Inicia a movimentacao, depois o loop para abrir os zeros, no pior dos casos 20 vezes
            ehzero = verificaAbertoeZero $ retiraCelula2 tabuleiro' posicao            -- Verifica zero pra implementar o loop dos 8 movimentos
            perdeu = verificaPerdeu $ retiraCelula2 tabuleiro' posicao                 -- Verifica se a celula escolhida é bomba
            ganhou = percorreTabuleiro2 tabuleiro' 0 400                               -- Verifica quantas celulas estao fechadas 
          
          if perdeu                                                                    -- Se abriu bomba
            then do 
              print tabuleiro' 
              print "Voce Perdeu."
            else 
            if ganhou == 40                                                           -- Verifica a quantidade de espacos abertos
              then do
                print tabuleiro'
                print "Parabéns Você Ganhou!" 
            else
            if ehzero                                                                 -- Verifica zero caso for ele abre os adjacentes
              then do
                let tabuleiro'' = loop8Movimentos 40 tabuleiro'                       -- Chama o loop com 40 execucoes
                loopJogo tabuleiro''
            else
              loopJogo tabuleiro'
       
                                                                                      
  


main :: IO ()
main = do 
  rng <- newStdGen                                                                      -- Set no random

  loopJogo $ buildTabuleiro rng                                                         -- Cria o tabuleiro 
