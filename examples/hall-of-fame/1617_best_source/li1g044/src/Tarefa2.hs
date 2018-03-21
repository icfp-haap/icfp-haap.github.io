{-|
Module      : Main
Description : Módulo Haskell que determina o efeito de um comando no estado do jogo
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Módulo Haskell responsável por determinar o efeito dos comandos dos jogadores e pela alteração do mapa em conformidade com tais comandos.

Recebe, na ordem referida, a lista representativa do mapa de jogo, o identificador do jogador que realizou o comando e o comando e devolve a lista com o mapa de jogo resultante do comando.

O identificador do jogador é um 'Int' do conjunto [0,1,2,3].

Os comandos aceites são @U@ (/Up/), @D@ (/Down/), @L@ (/Left/), @R@ (/Right/) e @B@ (/Bomb/).

-}
module Tarefa2 where

import Data.Char
import Data.List
import System.Environment


-- MOVIMENTO

{- |
A função 'pCoord' recebe o mapa de jogo e o identificador do jogador (@p@) e devolve a posição inicial deste, 
na forma de um par de coordenadas @(x,y)@.
-}
pCoord :: [String] -> Int -> (Int,Int)
pCoord ((h:y:t):ts) p | h==intToDigit p = mkPar t
                      | h/=intToDigit p = pCoord ts p
    where mkPar :: String -> (Int,Int)
          mkPar l = let a = par1 l
                        b = par2 l
                        in (read a,read b)
-- | 'par1' é uma função auxiliar de 'pCoord'; determina o @x@ do par de coordenadas.
par1 :: String -> String
par1 (h:t) | isDigit h = h:par1 t
           | otherwise = []
-- | 'par2' é uma função auxiliar de 'pCoord'; determina o @y@ do par de coordenadas.
par2 :: String -> String
par2 l = aux$drop 1 (dropWhile (/=' ') l)
            where aux [a] = [a]
                  aux (h:t) | isDigit h = h:aux t
                            | otherwise = []

{- |
A função 'newCoord' recebe o par de coordenadas inicial (output de 'pCoord') e um comando de movimento (@''U''@, @''D''@, @''L''@, @''R''@)
e devolve as coordenadas (x,y) da posição para onde o jogador se quer mover.
-}
newCoord :: (Int,Int) -> Char -> (Int,Int)
newCoord (x,y) c | c=='U' = (x,y-1)
                 | c=='D' = (x,y+1)
                 | c=='L' = (x-1,y)
                 | c=='R' = (x+1,y)
                 | otherwise = (x,y)


{- |
'checkPos3' é uma __função auxiliar__ de 'checkPos'. Recebe o mapa de jogo e as coordenadas (x,y) para as quais o jogador se quer mover.

Verifica se, nas coordenadas para as quais o jogador se quer mover, existe um /power up/ 
@Bombs@ ( @+@ ), @Flames@ ( @!@ ) ou uma __bomba__ ( @*@ ), devolvendo o @Char@ correspondente ao caso que se verifique.

Se nenhum destes casos se verificar, devolve um __espaço__ (@' '@).
-}
checkPos3 :: [String] -> (Int,Int) -> Char
checkPos3 [] (x,y) = ' '
checkPos3 ((h:t):ts) (x,y) | '+':' ':[]++show x++" "++show y`elem`((h:t):ts) = '+'
                           | '!':' ':[]++show x++" "++show y`elem`((h:t):ts) = '!'
                           | h=='*' = if ('*':' ':[]++show x++" "++show y++drop (length (" "++show x++" "++show y)) t)`elem`((h:t):ts) then '*' else checkPos3 ts (x,y)
                           | otherwise = checkPos3 ts (x,y)

{- |
'checkPos2' é uma __função auxiliar__ de 'checkPos'. Recebe o mapa de jogo e as coordenadas (x,y) para as quais o jogador se quer mover.

Determina que caratér se encontra nas coordenadas (x,y) e devolve-o.
-}
checkPos2 :: [String] -> (Int,Int) -> Char
checkPos2 l (x,y) = (l!!y)!!x

{- |
'checkPos' é a função que verifica o que se encontra no mapa nas coordenadas (x,y).
Recebe o mapa de jogo e as coordenadas (x,y) para as quais o jogador se quer mover.
-}
checkPos :: [String] -> (Int,Int) -> Char
checkPos l (x,y) = if checkPos2 l (x,y) == ' ' then checkPos3 l (x,y)
                   else checkPos2 l (x,y)




{- |
'changeCoord' é a função que determina se o jogador se pode mover para (x,y) e modifica o mapa de jogo de acordo 
com a célula para onde ele se move.
Recebe o mapa de jogo, o identificador do jogador, as coordendas (x,y) para as quais o jogador se quer mover e o 
output de 'checkPos' (@c@).

A função determina se o jogador se pode mover para (x,y):

* Caso @c@ seja @#@ (pedra) ou  @?@ (tijolo), 
o jogador não se pode mover para essa célula e 'changeCoord' devolve o mapa de jogo original.

* Caso @c@ seja um /power up/, 'changeCoord' permite que o jogador se mova para essa célula, adiciona o /power up/ à lista
do jogador e remove o /power up/ do mapa.

* Em qualquer outro caso, 'changeCoord' permite que o jogador se mova para a nova célula (invoca 'replacePlayerCoords').
-}
changeCoord :: [String] -> Int -> (Int,Int) -> Char -> [String]
changeCoord l j (x,y) c | c=='#' || c=='?' = l
                        | c=='+' || c=='!' = removePU (replacePlayerCoords (changePlayerPU l j c) j (x,y)) (x,y) c
                        | c=='0' || c=='1' || c=='2' || c=='3' = replacePlayerCoords l j (x,y)
                        | c==' ' || c=='*' = replacePlayerCoords l j (x,y)

{- |
'removePU' é uma __função auxiliar__ de 'changeCoord'. Recebe o mapa de jogo, as coordenadas (x,y) para as quais o jogador se 
quer mover e @'+'@ ou @'!'@, dependendo de 'changeCoord'.

Remove o /power up/ @c@ com as coordenadas (x,y) do mapa de jogo.
-}
removePU :: [String] -> (Int,Int) -> Char -> [String]
removePU [] (x,y) c = []
removePU ((h:t):ts) (x,y) c | h==c && (c:' ':[]++show x++" "++show y)==(h:t) = ts
                            | otherwise = (h:t):removePU ts (x,y) c


{- |
'replacePlayerCoords' é uma __função auxiliar__ de 'changeCoord'. Recebe o mapa de jogo, o identificador do jogador e 
as coordenadas (x,y) para as quais o jogador se quer mover.

Altera as coordenadas originais do jogador @p@ para as novas coordenadas (x,y).
-}
replacePlayerCoords :: [String] -> Int -> (Int,Int) -> [String]
replacePlayerCoords ((h:t):ts) p (x,y) | h==(intToDigit p) = [show p++" "++show x++" "++show y++" "++(unwords (drop 2 (words t)))] ++ ts
                                       | otherwise = (h:t):(replacePlayerCoords ts p (x,y))

{- |
'changePlayerPU' é uma __função auxiliar__ de 'changeCoord'. Recebe o mapa de jogo, o identificador do jogador (@j@) e o
identificador de um /power up/ (@+@ ou @!@).

Adiciona um /power up/ __@p@__ à lista do jogador @j@.
-}
changePlayerPU :: [String] -> Int -> Char -> [String]
changePlayerPU ((h:t):ts) j p | h==(intToDigit j) = if hasPowerUp (h:t) then ((order (h:t++[p])):ts) else (h:t++' ':[p]):ts
                              | otherwise = (h:t):(changePlayerPU ts j p)

-- |Determina se o jogador já tem pelo menos um powerup; FUNÇÃO AUXILIAR DE @changePlayerPU@
{- |
'hasPowerUp' é uma __função auxiliar__ de 'changePlayerPU'. Recebe a lista do jogador @j@.

Verifica se o jogador @j@ já tem pelo menos um /power up/.
-}
hasPowerUp :: String -> Bool
hasPowerUp [] = False
hasPowerUp (h:t) | h=='+' = True
                 | h=='!' = True
                 | otherwise = hasPowerUp t
-- |Ordena os powerups na string de um jogador j (0 1 1 ++!+) -> (0 1 1 +++!); FUNÇÃO AUXILIAR DE @changePlayerPU@
{- |
'order' é uma __função auxiliar__ de 'changePlayerPU'. Recebe a lista do jogador @j@ com os /power ups/ potencialmente desorganizados.

Devolve a lista do jogador @j@ com os /power ups/ do jogador @j@.
-}
order :: String -> String
order [] = []
order (h:t) | h=='!' = if '+'`elem`t then order (t++[h]) else (h:t)
            | otherwise = h:order t

-- BOMBAS
{- |
'placeBomb' é a função que adiciona __bombas__ ao mapa de jogo.

Fá-lo de modo a que as bombas fiquem organizadas (na lista representativa do mapa do jogo) por ordem crescente da coordenada y e, caso
duas bombas tenham a mesma coordenada y, organiza-as por ordem crescente da coordenada x.

Recebe o mapa de jogo, as coordenadas (x,y) onde irá colocar a bomba e o identificador do jogador que a colocou.
-}
placeBomb :: [String] -> (Int,Int) -> Int -> [String]
placeBomb ((h:t):ts) (x,y) p | h=='*' = if y < (read ((words (h:t))!!2) :: Int)
                                        then ('*':' ':[]++show x++" "++show y++' ':intToDigit p:' ':[]++raio ((h:t):ts) p++' ':'1':'0':[]):(h:t):ts
                                        else placeBombAux1 ((h:t):ts) (x,y) p
                             | isDigit h = ('*':' ':[]++show x++" "++show y++' ':intToDigit p:' ':[]++raio ((h:t):ts) p++' ':'1':'0':[]):(h:t):ts
                             | otherwise = (h:t):placeBomb ts (x,y) p

{- |
'placeBombAux1' é uma __função auxiliar__ de 'placeBomb'. Recebe o mapa de jogo,  as coordenadas (x,y) 
onde irá colocar a bomba e o identificador do jogador que a colocou.

Ajuda colocar as bombas de forma ordenada.
-}
placeBombAux1 :: [String] -> (Int,Int) -> Int -> [String]
placeBombAux1 ((h:t):ts) (x,y) p | h=='*' = if y > (read ((words (h:t))!!2) :: Int)
                                   then (h:t):placeBombAux1 ts (x,y) p
                                   else if y == (read ((words (h:t))!!2) :: Int)
                                        then placeBombAux2 ((h:t):ts) (x,y) p
                                        else ('*':' ':[]++show x++" "++show y++' ':intToDigit p:' ':[]++raio ((h:t):ts) p++' ':'1':'0':[]):(h:t):ts
                                 | isDigit h = ('*':' ':[]++show x++" "++show y++' ':intToDigit p:' ':[]++raio ((h:t):ts) p++' ':'1':'0':[]):(h:t):ts
{- |
'placeBombAux2' é uma __função auxiliar__ de 'placeBomb'. Recebe o mapa de jogo,  as coordenadas (x,y) 
onde irá colocar a bomba e o identificador do jogador que a colocou.

Ajuda colocar as bombas de forma ordenada.
-}
placeBombAux2 :: [String] -> (Int,Int) -> Int -> [String]
placeBombAux2 ((h:t):ts) (x,y) p | x < (read ((words (h:t))!!1) :: Int) = ('*':' ':[]++show x++" "++show y++' ':intToDigit p:' ':[]++raio ((h:t):ts) p++' ':'1':'0':[]):(h:t):ts
                                 | otherwise = (h:t):placeBombAux1 ts (x,y) p

{- |
'raio' é uma __função auxiliar__ de 'placeBomb'. Recebe o mapa de jogo e o identificador do jogador que colocou a bomba.

Determina o raio da bomba, baseando-se na quantidade de /power ups/ __Bombs__ que o jogador tem.
-}
raio :: [String] -> Int -> String
raio ((h:t):ts) p | h==intToDigit p = show ((conta (h:t) '!')+1)
                  | otherwise = raio ts p
{- |
'conta' é uma __função auxiliar__ de 'raio' e de 'checkAllowedBombs'. Recebe a lista do jogador que colocou a bomba.

Conta o número de /power ups/ __x__ (@+@ ou @!@, depende do caso) que o jogador tem.
-}
conta :: String -> Char -> Int
conta [] x = 0
conta (h:t) x | h==x = 1+conta t x
              | otherwise = conta t x

{- |
'checkAllowedBombs' é uma __função auxiliar__ de 'canPlaceBomb'. Recebe o mapa de jogo e o identificador do jogador que colocou a bomba.

Devolve o número de bombas que o jogador pode colocar.
-}
checkAllowedBombs :: [String] -> Int -> Int
checkAllowedBombs ((h:t):ts) j | h==intToDigit j = (conta t '+') + 1
                               | otherwise = checkAllowedBombs ts j

{- |
'checkActiveBombs' é uma __função auxiliar__ de 'canPlaceBomb'. Recebe o mapa de jogo e o identificador do jogador que colocou a bomba.

Devolve o número de bombas que o jogador tem ativas (por explodir).
-}
checkActiveBombs :: [String] -> Int -> Int
checkActiveBombs [] j = 0
checkActiveBombs ((h:t):ts) j | h=='*' && take 1 ((drop 3 (words (h:t)))) == [[intToDigit j]] = 1 + checkActiveBombs ts j
                              | otherwise = checkActiveBombs ts j

{- |
'canPlaceBomb' é a função que autoriza ou nega a colocação de uma bomba pelo jogador.

Recebe o mapa de jogo e o identificador do jogador que colocou a bomba.
-}
canPlaceBomb :: [String] -> Int -> Bool
canPlaceBomb l j = if (checkAllowedBombs l j > checkActiveBombs l j) && (checkPos l (pCoord l j))/='*' then True else False


{- |
'move' é a __função principal__ que agrega todas as outras funções para modificar o mapa de jogo de acordo com os comandos do jogador

Recebe o mapa de jogo, o identificador do jogador para o qual o comando vai fazer efeito e o comando.
-}
move :: [String] -> Int -> Char -> [String]
move [] j c = []
move l j c | c=='B' = if canPlaceBomb l j then placeBomb l (pCoord l j) j else l
           | otherwise = changeCoord l j (newCoord (pCoord l j) c) (checkPos l (newCoord (pCoord l j) c))

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
