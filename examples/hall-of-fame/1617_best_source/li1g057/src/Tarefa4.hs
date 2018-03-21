module Tarefa4 where

import Data.Char
import System.Environment
import Text.Read
import Data.Maybe

{-|===Função:
@A função 'avanca' retorna o mapa com a passagem do tempo@

===Variáveis:
@__::[String]:__Mapa atual
__-> Int:__Tempo para o fim da partida@-}
avanca :: [String] -> Int -> [String]
avanca m t = drawSpiral (next (listOfExplosion $ reduceTimeOfBombs m)) t

main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

{-|===Função:
@A função 'mapSize' devolve um Int que representa o tamanho do mapa@

===Variáveis:
@__::[String]:__ Mapa do jogo@ -}
mapSize :: [String] -> Int
mapSize [] = 0
mapSize (h:t) = length h

{-|===Função:
@A função 'radiusDir' dá o raio da bomba numa determinada direção@

===Variáveis:
@__:: [String]:__Mapa do jogo
__-> (Int,Int):__Coordenada da bomba
__-> (Int,Int):__Direção da explosão Ex: (1,0) para a explosão ser para a direita
__-> Int:__Auxiliar [deve começar por 0]
__-> Int:__Raio da bomba@-}
radiusDir :: [String] -> (Int,Int) -> (Int,Int) -> Int -> Int -> Int
radiusDir m (x,y) (dx,dy) i r | i == r = 0
                              | value == ' ' = 1 + radiusDir m (x,y) (dx,dy) (i+1) r
                              | value == '#' = 0
                              | elem (x,y) (listOfPowerUps m) = 1
                              | otherwise = 1
                              where value = (m!!(y + dy * i))!!(x + dx * i)

{-|===Função:
@A função 'radius' dá o raio da bomba em todas as direções@

===Variáveis:
@__::[String]:__Mapa do jogo
__-> (Int,Int):__Coordenadas da bomba
__-> Int:__Raio da bomba@-}
radius :: [String] -> (Int,Int) -> Int -> (Int,Int,Int,Int)
radius m (x,y) r = (rad right,rad left,rad down,rad up)
                  where rad d = radiusDir m (x,y) d 1 (r+1)
                        right = (1,0)
                        left = (-1,0)
                        down = (0,1)
                        up = (0,-1)

{-|===Função:
@A função 'reduceTimeOfBombs' reduz o tempo das bombas no mapa@

===Variáveis:
@__:: [String]:__Mapa do jogo@-}
reduceTimeOfBombs :: [String] -> [String]
reduceTimeOfBombs [] = []
reduceTimeOfBombs ((x:xs):y) = if x == '*' then (unwords $ newBombLine):reduceTimeOfBombs y
                                           else (x:xs):reduceTimeOfBombs y
                                where bombLine = words xs
                                      newBombLine = "*":(init bombLine) ++ [show (read (last bombLine) - 1)]

{-|===Função:
@A função 'drawAtPoint' muda o valor de vários pontos no mapa@

===Variáveis:
@__:: [[a]]:__Mapa
__-> (Int,Int):__Coordenada a mudar
__-> a:__Novo valor@-}
drawAtPoint :: [[a]] -> (Int,Int) -> a -> [[a]]
drawAtPoint [] _ _ = []
drawAtPoint (l:ls) (x,0) c = (take x l ++ [c] ++ drop (x+1) l):ls
drawAtPoint (l:ls) (x,y) c = l:drawAtPoint ls (x,y-1) c

{-|===Função:
@A função 'explosion' retorna o mapa com o novo risco causado pelas bombas@

===Variáveis:
@__::(Int,Int):__Coordenadas da bomba
__-> (Int,Int,Int,Int):__Raio em todas as direções da bomba
__-> [String]:__Mapa do jogo@-}
explosion :: (Int,Int) -> (Int,Int,Int,Int) -> [String] -> [String]
explosion (x,y) (0,0,0,0) m = explodeBombs (x,y) $ destroyBricks (x,y) $ killPlayers (x,y) 3 m
explosion (x,y) (r,l,d,u) m | r > 0 = explode (x + r,y) (r-1,l,d,u)
                            | l > 0 = explode (x - l,y) (r,l-1,d,u)
                            | d > 0 = explode (x,y + d) (r,l,d-1,u)
                            | u > 0 = explode (x,y - u) (r,l,d,u-1)
                            where explode a b = explodeBombs a $ destroyBricks a $ killPlayers a 3 (explosion (x,y) b m)

{-|===Função:
@A função 'getPositions' devolve a posição de todas as bombas no mapa@

===Variáveis:
@__:: Char:__Bomba (*), Power-up Flames (!) ou Power-Up Bombs
__-> [String]:__Mapa do jogo@-}
getPositions :: Char -> [String] -> [(Int,Int)]
getPositions c [] = []
getPositions c ((x:xs):y) | x == c = (read ((words xs) !! 0),read ((words xs) !! 1)):getPositions x y
                          | otherwise = getPositions c y
{-|===Função:
@A função 'killPlayers' mata os jogadores@

===Variáveis:
@__:: (Int,Int):__Posição do jogador
__-> Int:__Auxiliar que começa em 3 (começa no player 3, depois player 2, depois player e por último player 0)
__-> [String]:__Mapa do jogo@-}
killPlayers :: (Int,Int) -> Int -> [String] -> [String]
killPlayers _ (-1) m = m
killPlayers (x,y) p m = if [(x,y)] == getPositions (intToDigit p) m then killPlayers (x,y) (p-1) (removeLineOfPlayer p m)
                                                                    else killPlayers (x,y) (p-1) m

{-|===Função:
@A função 'removeLineOfPlayer' devolve o mapa sem a linha de um jogador@

===Variáveis:
@__:: Int:__Número do jogador
__-> [String]:__Mapa do jogo@-}
removeLineOfPlayer :: Int -> [String] -> [String]
removeLineOfPlayer p ((x:xs):y) = if show p == [x] then y else (x:xs):removeLineOfPlayer p y

{-|===Função:
@A função 'destroyBricks' retira os tijolos@

===Variáveis:
@__:: (Int,Int):__Posição a retirar
__-> [String]:__Mapa do jogo@-}
destroyBricks :: (Int,Int) -> [String] -> [String]
destroyBricks (x,y) m = if (m!!y)!!x == '?' then drawAtPoint m (x,y) ' ' else destroyPowerUps (x,y) m

{-|===Função:
@A função 'destroyPowerUps' retira os power-ups que se encontram num determinado ponto@

===Variáveis:
@__:: (Int,Int):__Posição do Power-up
__-> [String]:__Mapa do jogo@-}
destroyPowerUps :: (Int,Int) -> [String] -> [String]
destroyPowerUps _ [] = []
destroyPowerUps (x,y) ((h:hs):t) = if b then t else (h:hs):destroyPowerUps (x,y) t
                                  where a = words hs
                                        b = if h == '+' || h == '!' then a!!0 == show x && a!!1 == show y else False

{-|===Função:
@A função 'destroyBombs' retira a bomba do mapa numa determinada posição@

===Variáveis:
@__:: (Int,Int):__ Coordenada da bomba
__-> [String]:__Mapa do jogo@-}
destroyBombs :: (Int,Int) -> [String] -> [String]
destroyBombs _ [] = []
destroyBombs (x,y) ((h:hs):t) = if b then t else (h:hs):destroyPowerUps (x,y) t
                                where a = words hs
                                      b = if h == '*' then a!!0 == show x && a!!1 == show y else False

{-|===Função:
@A função 'explodeBombs' explode uma bomba@

===Variáveis:
@__:: (Int,Int):__Coordenada da bomba
__-> [String]:__ Mapa do jogo@-}
explodeBombs :: (Int,Int) -> [String] -> [String]
explodeBombs _ [] = []
explodeBombs (x,y) ((h:hs):t) = if b then unwords (["*"] ++ init a ++ ["1"]):explodeBombs (x,y) t else (h:hs):explodeBombs (x,y) t
                                where a = words hs
                                      b = if h == '*' then a!!0 == show x && a!!1 == show y else False

{-|===Função:
@A função 'listOfExplosion' lista as bombas que vão explodir@

===Variáveis:
@__:: [String]:__ Mapa do jogo@-}
listOfExplosion :: [String] -> ([(Int,Int,Int)],[String])
listOfExplosion [] = ([],[])
listOfExplosion ((h:hs):t) = if b then ((read (a!!0),read (a!!1),read (a!!3)):c,d) else (c,(h:hs):d)
                                  where a = words hs
                                        b = if h == '*' then last a == "0" else False
                                        (c,d) = listOfExplosion t

{-|===Função:
@A função 'listOfPowerUps' devolve uma lista de power-ups do mapa@

===Variáveis:
@__:: [String]:__Mapa do jogo@-}
listOfPowerUps :: [String] -> [(Int,Int)]
listOfPowerUps [] = []
listOfPowerUps ((h:hs):t) = if h == '+' || h == '!' then (read (a!!0),read (a!!1)):listOfPowerUps t else listOfPowerUps t
                            where a = words hs

{-|===Função:
@A função 'startSpiralMap' inicia a espiral (com Booleanos)@

===Variáveis:
@__:: Int:__Tamanho do Mapa
__-> Int:__Tamanho do Mapa@-}
startSpiralMap :: Int -> Int -> [[Bool]]
startSpiralMap 0 _ = []
startSpiralMap n s = if n == s || n == 1 then (replicate s False):startSpiralMap (n-1) s
                                         else (False:(replicate (s-2) True) ++ [False]):startSpiralMap (n-1) s

{-|===Função:
@A função 'spiral' produz a espiral@

===Variáveis:
@__:: (Int,Int):__Coordenada inicial
__-> [[Bool]]:__Mapa inicial da espiral
__-> Int:__Auxiliar [deve começar por 0]
__-> Char:__Direção da espiral@-}
spiral :: (Int,Int) -> [[Bool]] -> Int -> Char -> (Int,Int)
spiral (x,y) m n d | n == (size-2)^2 = (x,y)
                   | d == 'R' = if empty (x+1,y) then spiral (x+1,y) s (n+1) d else spiral (x,y) m n 'D'
                   | d == 'D' = if empty (x,y+1) then spiral (x,y+1) s (n+1) d else spiral (x,y) m n 'L'
                   | d == 'L' = if empty (x-1,y) then spiral (x-1,y) s (n+1) d else spiral (x,y) m n 'U'
                   | d == 'U' = if empty (x,y-1) then spiral (x,y-1) s (n+1) d else spiral (x,y) m n 'R'
                    where empty (x,y) = (m!!y)!!x
                          s = drawAtPoint m (x,y) False
                          size = length $ head m

{-|===Função:
@A função 'drawSpiral' desenha a espiral com números de 1 a 9 consoante estarem mais próximos ou mais longe da espiral@

===Variáveis:
@__:: [String]:__Mapa de risco
__-> Int:__Tempo para o fim da partida@-}
drawSpiral :: [String] -> Int -> [String]
drawSpiral m t = if t > (size-2)^2 then m
                                   else destroyBombs p $ destroyPowerUps p $ killPlayers p 3 (drawAtPoint m p '#')
                 where p = spiral (1,1) (startSpiralMap size size) t 'R'
                       size = length $ head m

{-|===Função:
@A função 'next' explode as bombas@

===Variáveis:
@__:: ([(Int,Int,Int),[String]]):__Posição da bomba, tempo e mapa do jogo@-}
next :: ([(Int,Int,Int)],[String]) -> [String]
next ([],m) = m
next (((x,y,t):z),m) = next (z, (explosion (x,y) (radius m (x,y) t) m))
