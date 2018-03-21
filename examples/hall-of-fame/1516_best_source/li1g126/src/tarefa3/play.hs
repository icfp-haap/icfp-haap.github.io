---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------        TAREFA 3 - PLAY        ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Play
Description : Módulo para uma jogada num tabuleiro de /Sokoban./
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo para uma jogada num tabuleiro de __/Sokoban/__. Este módulo
recebe todas as coordenadas e o tabuleiro em si e retorna a posição
do boneco após a movintação, caso esta não seja posível retorna a 
posição atual.
-}

module Main where

import Data.Char

-- | Lista com o tabuleiro.
type Tabuleiro  = [String]

-- | Lista com as coordenadas.
type Coords     = [String]

-- | Posições (x,y).
type Posicao    = (Int,Int)

-- | /Output/ com a posição final.
type Output     = String

-- | /Input/ constituído pelo tabuleiro e pelas coordenadas.
type Input      = [String]

-- | String com o movimento pretendido.
type Movimento  = String

---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Divide linhas do /output/.
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

-- | Função /Main/.
main = do inp <- getContents
          putStr (outStr (tarefa3 (lines inp)))

-- | Função geral.
{-|
* Nesta parte da tarefa, o objetivo será determinar qual a posição final do boneco após a execução de um movimento.
Encontram-se abaixo indicados exemplos do resultado final.

    >         Input                     Output
    >
    >  #####################     |       8 2
    >  #### ..  #          #     |
    >  ####               ##     |
    >  ###     #           #     |
    >  #####################     |
    >  8 2
    >  8 3
    >  3 1
    >  D
    >         Input                     Output
    >
    >  #####################     |       12 2
    >  #### ..             #     |        
    >  ####               ##     |        
    >  ###                 #     |        
    >  #####################     |        
    >  12 1
    >  8 3
    >  3 1
    >  U
-}
tarefa3 :: Input -> [Output]
tarefa3 p = [play p]

------------------------------------------------------------------
-- * __Movimentações__
------------------------------------------------------------------

-- ** Realiza movimentação
-- | Movimentação realizada num tabuleiro onde estão colocadas as caixas
--   e as paredes.
play :: Tabuleiro -> Output
play l = output $ move tabuleiro posBoneco commmand
    where (tab,coor) = divideTab l
          commmand   = last l
          posBoneco  = (converterEmDuplos (head coor))
          tabuleiro  = (colocElem tab (stringCoorCart coor) 1)

-- ** Verificação
-- | É verificado se existe algum obstáculo, isto é,
--   uma parede, duas caixas seguidas ou uma caixa com um muro.
--
-- ==== __/Exemplo de utilização/__
--
-- >>> move ["#####","#.  #","#####"] (3,1) "L"
-- "2 1"
move :: Tabuleiro -> Posicao -> Movimento -> Posicao
move tab (a,b) "U"
    | up == '#' || (elem up "IH" && elem up2 "#IH") = (a,b)
    | otherwise                                     = (a,b+1)
    where up = tab !! (length tab - b - 2) !! a
          up2 = tab !! (length tab - b - 3) !! a
move tab (a,b) "D"
    | down == '#' || (elem down "IH" && elem down2 "#IH") = (a,b)
    | otherwise                                           = (a,b-1)
    where down = tab !! (length tab - b) !! a
          down2 = tab !! (length tab - b + 1) !! a
move tab (a,b) "R"
    | right == '#' || (elem right "IH" && elem right2 "#IH") = (a,b)
    | otherwise                                              = (a+1,b)
    where right = tab !! keepY !! (a + 1)
          right2 = tab !! keepY !! (a + 2) 
          keepY = length tab - b - 1
move tab (a,b) "L"
    | left == '#' || (elem left "IH" && elem left2 "#IH") = (a,b)
    | otherwise                                           = (a-1,b)
    where left = tab !! keepY !! (a - 1)
          left2 = tab !! keepY !! (a - 2)
          keepY = length tab - b - 1

-- ** Print /Output/
-- | Converte par ordenado em string que separa os número com os espaços.
--
-- ==== __/Exemplo de utilização/__
--
-- >>> output (3,2)
-- "3 2"
output :: Posicao -> Output
output (a,b) = show a ++ " " ++ show b

------------------------------------------------------------------
-- * __Funções auxiliares__
------------------------------------------------------------------

-- ** Divisão do /Input/
-- | Função da Tarefa 1 (/validation/).
divideTab :: Input -> (Tabuleiro, Coords)
divideTab [] = ( [] , [])
divideTab l | last l `elem` ["L","U","R","D",""] = divideTab (init l)
divideTab ([]:ts) = let (tab,coor) = divideTab (ts)
                    in ( []:tab , coor )
divideTab l@((h:t):ts)
    | isDigit h = ( [] , l )
    | otherwise = let (tab,coor) = divideTab (ts)
                  in ( (h:t):tab , coor )

-- ** Colocação das coordenadas
-- *** Colocação geral
-- | Função da Tarefa 2 (/visualize/).
colocElem :: Tabuleiro -> [Posicao] -> Int -> Tabuleiro
colocElem tab [] _ = tab
colocElem tab ((a,b):tc) 1 = colocElem (colocAux tab (a,b) 'o') tc 0
colocElem tab ((a,b):tc) 0
    | (tab !! (length tab - b - 1) !! a) == '.' = colocElem (colocAux tab (a,b) 'I') tc 0
    | otherwise = colocElem (colocAux tab (a,b) 'H') tc 0
-- *** Troca por letra
-- | Função da Tarefa 2 (/visualize/).
colocAux :: Tabuleiro -> Posicao -> Char -> Tabuleiro
colocAux tab (a,b) p = (take ((length tab) - b - 1) tab) ++ [(take (a) line) ++ [p] ++ (drop (a + 1) line)]++ (drop ((length tab) - b) tab) 
    where line = (tab !! (length tab - b - 1))

-- ** Converte lista de duplos
-- | Função da Tarefa 2 (/visualize/).
stringCoorCart :: Tabuleiro -> [Posicao]
stringCoorCart t = map converterEmDuplos t

-- ** Converte em Duplos
-- | Função da Tarefa 1 (/validation/).
converterEmDuplos :: String -> Posicao
converterEmDuplos hc = let [a,b] = words hc
                        in (read a, read b)
