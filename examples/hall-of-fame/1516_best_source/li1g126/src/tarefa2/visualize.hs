---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------     TAREFA 2 - VISUALIZE      ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Visualize
Description : Módulo para "vizualização" de tabuleiros de /Sokoban./
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo de "vizualização" dos tabuleiros de __/Sokoban/__. Este módulo
faz a remoção dos caracteres '#' redundantes e faz a coloção do boneco
e das caixas no tabuleiro. Retorna o tabuleiro após estas alterações 
realizadas.
-}

module Main where

import Data.Char
import qualified Data.Text as T

-- | Lista com o tabuleiro.
type Tabuleiro  = [String]

-- | Lista com as coordenadas.
type Coords     = [String]

-- | Posições (x,y).
type Posicao    = (Int,Int)

-- | /Input/ constituído pelo tabuleiro e pelas coordenadas.
type Input      = [String]


---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Divide linhas do /Output/.
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

-- | Função /Main/.
main = do inp <- getContents
          putStr (outStr (tarefa2 (lines inp)))

-- | Função geral.
{-|
* Nesta parte da tarefa, o objetivo será produzir um mapa mais simplificado, onde
serão __removidos__ os cardinais redundantes e ainda __adicionadas__ as caixas
e o boneco. Encontram-se abaixo indicados dois exemplos do resultado final.

    >         Input                 Tabuleiro simplificado
    >
    >  ###################     |         #####
    >  #####   ###########     |         #   #
    >  #####   ###########     |         #H  #
    >  #####   ###########     |       ###  H##
    >  ###      ##########     |       #  H H #
    >  ### # ## ##########     |     ### # ## #   ######
    >  #   # ## #####  ..#     |     #   # ## #####  ..#
    >  #               ..#     |     # H  H          ..#
    >  ##### ### # ##  ..#     |     ##### ### #o##  ..#
    >  #####     #########     |         #     #########
    >  ###################     |         #######
    >  11 2
    >  5 8
    >  7 7
    >  5 6
    >  7 6
    >  2 3
    >  5 3
    >
    >         Input                 Tabuleiro simplificado
    >
    >  ################        |            ########
    >  #########      #        |            #      #
    >  #########  #  ##        |         ####  #  ##
    >  ######        ##        |         #      H #
    >  ##########    ##        |         #####    #
    >  #########   # ##        |    #########  H# ##
    >  #.     ##      #        |    #.     ## H    #
    >  ##.            #        |    ##.   o        #
    >  #.     #########        |    #.     #########
    >  ################        |    ########
    >  6 2
    >  10 3
    >  11 4
    >  12 6
-}

tarefa2 :: Input -> Tabuleiro
tarefa2 l = rmCar tabCCaixas
    where (tab,coor) = divideTab l
          tabCCaixas = (colocElem tab (stringCoorCart coor) 1)

--------------------------------------------------------------------
-- * __Remoção de '#' "redundantes"__
--------------------------------------------------------------------

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

-- ** Marcação de redundantes
-- | Depois de verificados se são redundantes, os cardinais são 
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

-- ** Redundantes para espaço
-- | Após a validação, os '#' redundantes (transformados em __'@'__) são __substituídos__
--   por espaços.
--
-- ==== __/Exemplos de utilização/__
-- >>> rmCar ["@@@#####","@@@#   #","@@@#   #","@@@#####"]
-- ["   #####","   #   #","   #   #","   #####"]
rmCar :: Tabuleiro -> Tabuleiro
rmCar tab = let f '@' = ' '; f c = c; 
            in (map.map) f (transformCar tab (0,0))
 
------------------------------------------------------------------
-- * __Colocação das coordenadas__
------------------------------------------------------------------

-- ** Colocação geral
-- | Função que marca no tabuleiro as posições do boneco e das caixas. Caso
--   seja a primeira vez a correr a função, a posição do boneco
--   corresponde ao __''o''__ , senão é verificada se a posição é
--   um ponto. Neste caso é colocado um __''I''__ correspondente à posição final
--   de uma caixa, ou um __''H''__ (caixa em posição não-final).
--
-- ==== __/Exemplos de utilização/__
-- >>> colocElem ["########","####  .#","####  .#","########"] [(4,1),(4,2),(6,2)] 1
-- ["########","####H I#","####o .#","########"]
colocElem :: Tabuleiro -> [Posicao] -> Int -> Tabuleiro
colocElem tab [] _ = tab
colocElem tab ((a,b):tc) 1 = colocElem (colocAux tab (a,b) 'o') tc 0
colocElem tab ((a,b):tc) 0
    | (tab !! (length tab - b - 1) !! a) == '.' = colocElem (colocAux tab (a,b) 'I') tc 0
    | otherwise                                 = colocElem (colocAux tab (a,b) 'H') tc 0

-- ** Troca por Letra
-- | Efetua-se a troca de um lugar (coordenada) pela letra correspondente
--   mencionada anteriormente.
--
-- ==== __/Exemplos de utilização/__
-- >>> colocAux ["########","####  .#","####o .#","########"] ( 4 , 2 ) 'H'
-- ["########","####H .#","####o .#","########"]
colocAux :: Tabuleiro -> Posicao -> Char -> Tabuleiro
colocAux tab (a,b) p = (take ((length tab) - b - 1) tab) ++ [(take (a) line) ++ [p] ++ (drop (a + 1) line)]++ (drop ((length tab) - b) tab) 
    where line = (tab !! (length tab - b - 1))

------------------------------------------------------------------
-- * __Funções auxiliares__
------------------------------------------------------------------

-- ** Divisão do /Input/
-- | Função da Tarefa 1 (/validation/).
divideTab :: Input -> (Tabuleiro, Coords)
divideTab [] = ( [] , [])
divideTab l | head(last l) `elem` ['L','U','R','D'] = divideTab (init l)
divideTab ([]:ts) = let (tab,coor) = divideTab ts
                    in ( []:tab , coor )
divideTab l@((h:t):ts)
    | isDigit h = ( [] , l )
    | otherwise = let (tab,coor) = divideTab ts
                  in ( (h:t):tab , coor )

-- ** Converte lista de duplos
-- | Converte as coordenadas numa lista de duplos de inteiros.
--
-- ==== __/Exemplos de utilização/__
-- >>> stringCoorCart ["4 1","4 2","6 2"]
-- [(4,1),(4,2),(6,2)]
stringCoorCart :: Tabuleiro -> [Posicao]
stringCoorCart t = map converterEmDuplos t

-- ** Converte em duplos
-- | Função da Tarefa 1 (validation).
converterEmDuplos :: String -> Posicao
converterEmDuplos ht = let [a,b] = words ht
                        in (read a, read b)



