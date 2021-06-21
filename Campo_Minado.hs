module Main(main) where
import Data_Instance
import System.Random

-- Funcoes que fazem a verificacao de cada celula separada

-- Criacao de celula comum
novaCelula :: Celula
novaCelula = Celula {valor = Numero 0, fechado = False}

-- Criacao da bomba
novaBomba :: Celula
novaBomba = Celula {valor = Bomba, fechado = False}

-- Adicao de celula no tabuleiro
adicionarCelula :: Celula -> Int -> Celula
adicionarCelula (Celula Bomba fechado) _ = Celula Bomba True
adicionarCelula (Celula (Numero x) fechado) n = Celula (Numero (x+n)) True

-- Verificar se a celula eh uma bomba
verificaBomba :: Maybe Celula -> Bool
verificaBomba (Just celula) = (valor celula == Bomba)
verificaBomba _ = False

-- Retira a celula para verificacao de implementacao no tabuleiro
retiraCelula :: Tabuleiro -> Int -> Int -> Maybe Celula
retiraCelula b y x 
  | y >= 0 && x >= 0 && y < 20 && x < 20  = Just $ celulas b !! (x*20 + y) -- Condicao para verificacao da celula
  | otherwise = Nothing

-- Retorna uma celula
retiraCelula2 :: Tabuleiro -> Int -> Celula
retiraCelula2 tabuleiro posicao = celulas tabuleiro !! posicao

-- Verifica se a celula eh zero para a expancao
verificaZero :: Maybe Celula -> Bool  
verificaZero (Just t) = (valor t == Numero 0) && not (fechado t)
verificaZero _= False

-- Verifica se a Celula esta fechada
verificarFechado :: Celula -> Bool
verificarFechado (Celula v f)
  | f == True = True
  | otherwise = False

-- Verifica se a celula aberta eh zero
verificaAbertoeZero :: Celula -> Bool
verificaAbertoeZero (Celula v f)
  | v == (Numero 0) && f == False = True
  | otherwise = False

-- Verfifica se a bomba esta fechada
verificaBombaFechada :: Celula -> Bool
verificaBombaFechada (Celula v f)
  | v == (Bomba) = True && f == True
  | otherwise = False

-- Estados de jogo

-- Perdeu o jogo
verificaPerdeu :: Celula -> Bool
verificaPerdeu (Celula v f)
  | v == Bomba = True
  | otherwise = False

-- Retorna quantas celulas estao fechadas, se retornar a quantidade de bombas ganhou o jogo
percorreTabuleiro2 :: Tabuleiro -> Int -> Int -> Int
percorreTabuleiro2 (Tabuleiro tabuleiro) contador posicao = loop tabuleiro contador posicao where
  loop _ contador 0 = contador
  loop (l:ls) contador posicao = if verificarFechado l == True then loop ls (contador+1) (posicao-1) else loop ls contador (posicao-1)

-- Construcao do Tabuleiro

-- Criacao do tabuleiro
novoTabuleiro :: Tabuleiro
novoTabuleiro = Tabuleiro {celulas = [novaCelula | i <- [1..20], j <- [1..20]]}

buildTabuleiro :: StdGen -> Tabuleiro
buildTabuleiro rng = setValor $ bombasAleatorias novoTabuleiro 40 rng

-- Inserir bombas no tabuleiro
bombasAleatorias :: Tabuleiro -> Int -> StdGen -> Tabuleiro
bombasAleatorias tabuleiro qnt gen = let
                                  loop x _ 0 = x
                                  loop (x:xs) rng qnt  -- Recebe um valor muito pequeno, quanto mais o loop acontece maior a possibilidade de bombas aparecerem
                                    | rngValue < (fromIntegral (qnt) / fromIntegral (length (x:xs))) = novaBomba : loop xs rng' (qnt - 1) 
                                    | otherwise = x : loop xs rng' qnt
                                    
                                    where (rngValue, rng') = randomR (0, 1) rng :: (Double, StdGen)
          
                                  tabuleiro' = loop (celulas tabuleiro) gen qnt
                                in
                                  Tabuleiro tabuleiro'     -- Retorna o tabuleiro com as bombas

-- Inserir valores nas celulas
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


-- Funcionalidades do jogo

-- Abrir uma celula
abreCelula :: Celula -> Celula
abreCelula (Celula v _) = Celula v False        

-- Percorre o tabuleiro
percorreTabuleiro :: Tabuleiro -> Int -> Tabuleiro
percorreTabuleiro (Tabuleiro tabuleiro) posicao = Tabuleiro (loop tabuleiro posicao) where
  loop [] _ = []
  loop (l:ls) 0 = abreCelula l:ls
  loop (l:ls) posicao = l : loop ls (posicao-1)

-- Recebe a posicao e o tabuleiro e manda para a funcao de percorrer
movimentacao :: Tabuleiro -> Int -> Tabuleiro
movimentacao tabuleiro posicao = percorreTabuleiro tabuleiro posicao

-- Abre as celulas em branco adjacentes
oitoMovimentos0s :: Tabuleiro -> Tabuleiro                    
oitoMovimentos0s tabuleiro = let
                      nextCoord x y   -- Escolhe as proximas coordenadas
                        | x == 19 = (0, y + 1)
                        | otherwise  = (x + 1, y)                 
                      verificaProx x y vizinhos = retiraCelula tabuleiro (x + fst vizinhos) (y + snd vizinhos) -- Verifica de acordo com a lista dos 8 vizinhos

                      loop []_ _= []
                      loop (Celula (Numero n) fechado:ts) x y -- Receber a primeira celula da lista de celulas
                        | any verificaZero $ map (verificaProx x y) vizinhos = Celula (Numero n) False : loop ts x' y' where (x', y') = nextCoord x y
                          -- Any retorna verdadeiro se houver pelo menos um do valor especificado, Celula recebe fechado como falso | set proxima coordenada
                      loop (t:ts) x y = t : loop ts x' y' where (x', y') = nextCoord x y

                      tabuleiro_atu = loop (celulas tabuleiro) 0 0 -- Inicia a funcoo loop com as coordenadas 0 0
                      
                    in
                      let tabuleiro_ant = tabuleiro
                        in
                          if tabuleiro_ant == (Tabuleiro tabuleiro_atu) 
                            then do 
                              Tabuleiro tabuleiro_atu
                            else 
                              oitoMovimentos0s (Tabuleiro tabuleiro_atu) -- Recursiva 

-- Inicializacao do jogo
loopJogo :: Tabuleiro -> IO ()
loopJogo tabuleiro = do
          print tabuleiro
          printsa(["Digite as coordenadas (x,y):"])
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
              printsa(["Voce Perdeu."])
            else 
            if ganhou == 40                                                           -- Verifica a quantidade de espacos abertos
              then do
                print tabuleiro'
                printsa(["Parabéns Você Ganhou!"]) 
            else
            if ehzero                                                                 -- Verifica zero caso for ele abre os adjacentes
              then do
                let tabuleiro'' = oitoMovimentos0s tabuleiro'                       -- Chama o loop com 40 execucoes
                loopJogo tabuleiro''
            else
              loopJogo tabuleiro'

main :: IO()
main = do 
  rng <- newStdGen               -- Set random
  loopJogo $ buildTabuleiro rng  -- Cria o tabuleiro 