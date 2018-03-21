---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------           SOLVER              ------------------- 
---------------------------------------------------------------------
------------       A77789         MIGUEL MAGALHAES       ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Solver
Description : Módulo que permite a resolução de um jogo /Sokoban/.
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo que permite a resolução de um jogo __/Sokoban/__. Este módulo recebe um input e certos comandos. O input é o mapa em si com a 
posição do boneco e as posições das caixas, e de seguida uma das funções gera comandos por ordem crescente de tamanho e uma outra testa
se estes mesmos comandos estão corretos. No caso de um estar correto é apresentado como solução.
-}

module Main where

import Data.Char
import qualified Data.Text as T
import Control.Monad
import Data.Time

-- | Lista com o tabuleiro.
type Tabuleiro  = [String]

-- | Lista com as coordenadas.
type Coords     = [String]

-- | Posições (x,y) do boneco.
type Posicao    = (Int,Int)

-- | /Input/ constituído pelo tabuleiro e pelas coordenadas.
type Input      = [String]

-- | String com o movimento pretendido.
type Movimento  = String

-- | Número de movimentos efetuados.
type TickCount  = Int

-- | String com os comandos, ex: ULRD.
type Comandos   = String

-- | Linha do tabuleiro.
type Linha      = String


---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Função /Main/.
main = do start <- getCurrentTime
          inp <- getContents
          putStr ((trySolution (lines inp) 0) ++ "\n")
          stop <- getCurrentTime
          print $ diffUTCTime stop start

-- | Testa as soluções geradas numa função auxiliar ('comands') até encontrar a solução do jogo.
trySolution tab n
  | jogada tab (comands !! n) == "1" = (comands !! n)
  | otherwise = trySolution tab (n+1)

-- | Gera soluções por ordem crescente de tamanho.
comands = [0..] >>= flip replicateM "URLD"

-- | Fornce o tabuleiro à função 'checkMov'.
jogada :: Input -> Comandos -> String
jogada l [] = "0"
jogada l c = output
    where (tab,coor) = divideTab l
          comandos   = c
          tabCCaixas = colocCaixas tab (stringCoorCart coor) 1
          posBoneco  = converterEmDuplos (head coor)
          output     = checkMov (rmBoneco tabCCaixas) 0 comandos posBoneco

-- | Fornece uma String com o tabuleiro de jogo após todas as movimentações e as implicações destas movimentações terem sido realizadas.
checkMov :: Tabuleiro -> TickCount -> Comandos -> Posicao -> String
checkMov l p c b
    | existemH tabCBoneco == 0   = "1"
    | null c                     = "0"
    | b == move l b [head c]     = checkMov l p (tail c) b
    | otherwise                  = checkMov movimentacao (p+1) (tail c) (move l b [head c])
    where tabCBoneco  = colocCaixas (rmBoneco l) [b] 1
          movimentacao = (mvBoxAux tabCBoneco b [(head c)])


-- | Fornece o tabuleido de jogo após o Sokoban executar um movimento juntamente com uma caixa.
mvBoxAux :: Tabuleiro -> Posicao -> Movimento -> Tabuleiro
mvBoxAux tab (a,b) "U"
    | (tab !! up !! a) == ' ' = tab
    | (tab !! up !! a) == '.' = colocAux (colocCaixas (rmBoneco tab) [(a,b)] 1) (a,b+1) '.'
    | otherwise               = colocCaixas (colocCaixas (rmBoneco tab) [(a,b)] 1) [(a,b+2)] 0
    where up = length tab - b - 2
mvBoxAux tab (a,b) "D"
    | (tab !! down !! a) == ' ' = tab
    | (tab !! down !! a) == '.' = colocAux (colocCaixas (rmBoneco tab) [(a,b)] 1) (a,b-1) '.'
    | otherwise                 = colocCaixas (colocCaixas (rmBoneco tab) [(a,b)] 1) [(a,b-2)] 0
    where down = length tab - b
mvBoxAux tab (a,b) "R"
    | (tab !! keepX !! right) == ' ' = tab
    | (tab !! keepX !! right) == '.' = colocAux (colocCaixas (rmBoneco tab) [(a,b)] 1) (a+1,b) '.'
    | otherwise                      = colocCaixas (colocCaixas (rmBoneco tab) [(a,b)] 1) [(a+2,b)] 0
    where right = a + 1
          keepX = length tab - b - 1
mvBoxAux tab (a,b) "L"
    | (tab !! keepX !! left) == ' ' = tab
    | (tab !! keepX !! left) == '.' = colocAux (colocCaixas (rmBoneco tab) [(a,b)] 1) (a-1,b) '.'
    | otherwise                     = colocCaixas (colocCaixas (rmBoneco tab) [(a,b)] 1) [(a-2,b)] 0
    where left = a - 1
          keepX = length tab - b - 1


-- | Verifica se o jogo terminou e, por isso, se todas as caixas estão nos locais de arrumação.
existemH :: Tabuleiro -> Int
existemH = foldr ((+) . existemHAux) 0

-- |Função auxiliar da 'existemH'.
existemHAux :: Linha -> Int
existemHAux [] = 0
existemHAux (h:t)
    | h == 'H'  = 1 + existemHAux t
    | otherwise = existemHAux t


-- | Remove o Sokoban do tabuleiro.
rmBoneco :: Tabuleiro -> Tabuleiro
rmBoneco t = map rmBonecoAux t
rmBonecoAux :: Linha -> Linha
rmBonecoAux [] = []
rmBonecoAux (h:t)
    | h == 'o'  = (' ':t)
    | h == 'O'  = ('.':t)
    | otherwise = h : rmBonecoAux t


------------------------------------------------------------------
-- * __Movimentações__
------------------------------------------------------------------

-- ** Verificação
-- | Movimentação realizada. Aqui é verificado se exite algum obstáculo do tipo de
--   uma parede, duas caixas seguidas ou uma caixa com um muro.
move :: Tabuleiro -> Posicao -> Movimento -> Posicao
move tab (a,b) "U"
    | up == '#' || (elem up "IH" && elem up2 "#IH") = (a,b)
    | otherwise                                 = (a,b+1)
    where up = tab !! (length tab - b - 2) !! a
          up2 = tab !! (length tab - b - 3) !! a
move tab (a,b) "D"
    | down == '#' || (elem down "IH" && elem down2 "#IH") = (a,b)
    | otherwise                                       = (a,b-1)
    where down = tab !! (length tab - b) !! a
          down2 = tab !! (length tab - b + 1) !! a
move tab (a,b) "R"
    | right == '#' || (elem right "IH" && elem right2 "#IH") = (a,b)
    | otherwise                                          = (a+1,b)
    where right = tab !! keepY !! (a + 1)
          right2 = tab !! keepY !! (a + 2) 
          keepY = length tab - b - 1
move tab (a,b) "L"
    | left == '#' || (elem left "IH" && elem left2 "#IH") = (a,b)
    | otherwise                                       = (a-1,b)
    where left = tab !! keepY !! (a - 1)
          left2 = tab !! keepY !! (a - 2)
          keepY = length tab - b - 1

------------------------------------------------------------------
-- * __Funções auxiliares__
------------------------------------------------------------------

-- ** Divisão do /Input/
-- | Função da Tarefa 1 (validation).
divideTab :: Input -> (Tabuleiro, Coords)
divideTab [] = ( [] , [])
divideTab l | head(last l) `elem` ['L','U','R','D'] = divideTab (init l)
divideTab ([]:ts) = let (tab,coor) = divideTab (ts)
                    in ( []:tab , coor )
divideTab l@((h:t):ts)
    | isDigit h = ( [] , l )
    | otherwise = let (tab,coor) = divideTab (ts)
                  in ( (h:t):tab , coor )

-- ** Colocação das coordenadas
-- *** Colocação geral
-- | Função da Tarefa 2 (visualize).
colocCaixas :: Tabuleiro -> [Posicao] -> Int -> Tabuleiro
colocCaixas tab [] _ = tab
colocCaixas tab ((a,b):tc) 1
    | elem (tab !! (length tab - b - 1) !! a) ".I" = colocCaixas (colocAux tab (a,b) 'O') tc 0
    | otherwise = colocCaixas (colocAux tab (a,b) 'o') tc 0
colocCaixas tab ((a,b):tc) 0
    | elem (tab !! (length tab - b - 1) !! a) "." = colocCaixas (colocAux tab (a,b) 'I') tc 0
    | otherwise = colocCaixas (colocAux tab (a,b) 'H') tc 0
-- *** Troca por letra
-- | Função da Tarefa 2 (visualize).
colocAux :: Tabuleiro -> Posicao -> Char -> Tabuleiro
colocAux tab (a,b) p = (take ((length tab) - b - 1) tab) ++ [(take (a) line) ++ [p] ++ (drop (a + 1) line)]++ (drop ((length tab) - b) tab) 
    where line = (tab !! (length tab - b - 1))

-- ** Converte lista de duplos
-- | Função da Tarefa 2 (visualize).
stringCoorCart :: Tabuleiro -> [Posicao]
stringCoorCart t = map converterEmDuplos t

-- ** Converte em Duplos
-- | Função da Tarefa 1 (validation).
converterEmDuplos :: String -> Posicao
converterEmDuplos hc = let [a,b] = words hc
                        in (read a, read b)


