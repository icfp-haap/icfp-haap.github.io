---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------          TAREFA 6             ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Move
Description : Módulo que diz respeito à movimentação do /Sokoban/.
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo adapatado da tarefa 4 e que funciona como esqueleto do jogo Sokoban. 
Este módulo diz respeito à __movimentação__ do Sokoban. Após receber o Input e os comandos necessários
à movimentação do Sokoban, a função move fornece um tabuleiro com a posição final do Sokoban após essa
movimentação.

-}
module Main where

import Data.Char
import qualified Data.Text as T

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

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

main = do inp <- getContents
          cmds <- readFile "content/levels/cmds.in"
          putStr (outStr (moves (lines inp) cmds))


-- | Função principal do movimento do Sokoban.
moves :: Input -> Comandos -> Tabuleiro
moves l c = transformCar (init tabWOutput) (0,0)
    where (tab,coor) = divideTab l
          comandos   = c
          tabCCaixas = addOutsideWall $ colocCaixas tab (stringCoorCart coor) 1
          posBoneco  = converterEmDuplos (head coor)
          tabWOutput = checkMov (rmBoneco tabCCaixas) 0 comandos (fst posBoneco, snd posBoneco + 1) 

-- | Exibe o tabuleiro de jogo após todas as movimentações e as implicações destas movimentações terem sido realizadas.
checkMov :: Tabuleiro -> TickCount -> Comandos -> Posicao -> Tabuleiro
checkMov l p c b
    | existemH tabCBoneco == 0                  = tabCBoneco ++ ["FIM " ++ show p]
    | null c                                    = tabCBoneco ++ ["INCOMPLETO " ++ show p]
    | b == move l b [head c] && [head c] /= "B" = checkMov l p (tail c) b
    | otherwise                                 = checkMov movimentacao (p+1) (tail c) (move l b [head c])
    where tabCBoneco  = colocCaixas (rmBoneco l) [b] 1
          movimentacao = if [head c] == "B"
                          then bomba l b
                          else (mvBoxAux tabCBoneco b [(head c)])
                          

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

-- | No caso de o jogador poder usar a bomba, a função remove paredes num raio de 1 caixa.
bomba :: Tabuleiro -> Posicao -> Tabuleiro
bomba tab (a,b) = bombaAux "ul" (a,b) $
                  bombaAux "ur" (a,b) $
                  bombaAux "dr" (a,b) $
                  bombaAux "dl" (a,b) $
                  bombaAux "r" (a,b) $
                  bombaAux "l" (a,b) $
                  bombaAux "u" (a,b) $
                  bombaAux "d" (a,b) $
                  (colocCaixas (rmBoneco tab) [(a,b)] 1)
  where bombaAux "d"  (a,b) tab = removeWall tab (a,b-1)
        bombaAux "u"  (a,b) tab = removeWall tab (a,b+1)
        bombaAux "l"  (a,b) tab = removeWall tab (a-1,b)
        bombaAux "r"  (a,b) tab = removeWall tab (a+1,b)
        bombaAux "ur" (a,b) tab = removeWall tab (a+1,b+1)
        bombaAux "ul" (a,b) tab = removeWall tab (a-1,b+1)
        bombaAux "dr" (a,b) tab = removeWall tab (a+1,b-1)
        bombaAux "dl" (a,b) tab = removeWall tab (a-1,b-1)

-- | Função auxiliar da função 'bomba' que remove as parede envolventes do Sokoban num raio 
--   de uma caixa.
removeWall :: Tabuleiro -> Posicao -> Tabuleiro
removeWall tab (a,b) = if (tab !! (length tab - b - 1) !! a) == '#'
                        then colocAux tab (a,b) ' '
                        else tab

-- | Adiciona uma parede à volta do mapa para ser possível utilizar a bomba.
--   Neste caso permite a permanência de uma parede.
addOutsideWall :: Tabuleiro -> Tabuleiro
addOutsideWall tab = map (++"#") $ reverse $ map (++"#") $ ([head tab] ++ reverse tab ++ [head tab])


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

-- | Função auxiliar.
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
-- | Movimentação realizada é verificação se tem algum obstáculo do tipo de
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
move tab (a,b) "B" = (a,b)

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

--------------------------------------------------------------------
-- * __Remoção de '#' "redundantes"__
--------------------------------------------------------------------

-- ** Marcação de redundantes
-- | Depois de verificados se redundantes, os cardinais que o sejam, são 
--   transformados em '@' para serem posteriormente transformados em espaços.
--   Os que não forem redundantes continuam inalterados.
--
-- ==== __/Exemplos de utilização/__
-- >>> transformCar ["########","####   #","####   #","########"] (0,0)
-- ["@@@#####","@@@#   #","@@@#   #","@@@#####"]
transformCar :: Tabuleiro -> Posicao -> Tabuleiro
transformCar tab (a,b)
    | a == (length $ head tab) = transformCar tab (0,b+1)
    | b == (length tab) = tab
    | checkCar tab (a,b) = transformCar correctionT (a+1,b)
    | otherwise = transformCar tab (a+1,b)
    where line = tab !! b
          correctionT = (take b tab) ++ [(take a line) ++ ['@'] ++ (drop (a+1) line)] ++ (drop (b+1) tab)

-- ** Verifica redundância
-- | Verifica se o cardinal é redundante ou não, ou seja, verifica se a toda à volta
--   existem apenas cardinais ou arrobas (@ que correspondem a cardinais já
--   marcados como redundantes).
--
-- ==== __/Exemplos de utilização/__
-- >>> checkCar ["@@@#####","@@##   #","####   #","########"] ( 1 , 2 )
-- True
checkCar :: Tabuleiro -> Posicao -> Bool
checkCar t (a,b) = t !! b !! a == '#' && and [dir,esq,cima,baixo,cimD,cimE,baixD,baixE]
    where dir    | r > 0 && r < w                   = t !! b !! r `elem` "#@" 
                 | otherwise                        = True
          esq    | l > 0 && l < w                   = t !! b !! l `elem` "#@"
                 | otherwise                        = True
          cima   | u > 0 && u < h                   = t !! u !! a `elem` "#@"
                 | otherwise                        = True
          baixo  | d > 0 && d < h                   = t !! d !! a `elem` "#@"
                 | otherwise                        = True
          cimD   | u > 0 && u < h && r > 0 && r < w = t !! u !! r `elem` "#@" 
                 | otherwise                        = True
          cimE   | u > 0 && u < h && l > 0 && l < w = t !! u !! l `elem` "#@"  
                 | otherwise                        = True
          baixD  | d > 0 && d < h && r > 0 && r < w = t !! d !! r `elem` "#@"  
                 | otherwise                        = True
          baixE  | d > 0 && d < h && l > 0 && l < w = t !! d !! l `elem` "#@" 
                 | otherwise                        = True
          w      = length (head t) - 1 -- width
          h      = length t - 1 -- height
          u      = b-1 -- up 
          d      = b+1 -- down
          l      = a-1 -- left
          r      = a+1 -- right
 

