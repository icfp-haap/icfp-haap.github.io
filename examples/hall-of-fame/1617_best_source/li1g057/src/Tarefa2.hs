module Tarefa2 where

import Data.Char
import System.Environment
{-|===Função:
@A função 'move' verifica se o jogador está a jogar. Em caso afirmativo, retorna o novo estado de jogo, caso contrário, retorna o mapa como estava@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> Char:__ Ação do jogador@ -}
move :: [String] -> Int -> Char -> [String]
move [] p d = []
move l p d | elem (intToDigit p) h = play l p d
           | otherwise = l
           where h = map head l

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

{-|===Função:
@A função 'play' imprime o mapa com o novo estado do jogo caso a ação seja possivel, caso contrário, retorna o mesmo mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> Char:__ Ação do jogador@ -}
play :: [String] -> Int -> Char -> [String]
play [] _ _ = []
play l p d | d == 'U' = if canMove l (x,y-1) then removeLine (drawAtPlayer l l p (x,y-1) powers) (powerUpLine l (x,y-1)) else l
           | d == 'D' = if canMove l (x,y+1) then removeLine (drawAtPlayer l l p (x,y+1) powers) (powerUpLine l (x,y+1)) else l
           | d == 'L' = if canMove l (x-1,y) then removeLine (drawAtPlayer l l p (x-1,y) powers) (powerUpLine l (x-1,y)) else l
           | d == 'R' = if canMove l (x+1,y) then removeLine (drawAtPlayer l l p (x+1,y) powers) (powerUpLine l (x+1,y)) else l
           | d == 'B' = if canPlant l (x,y) && numBombs p l < b then insertLine l  (bombLine l (x,y)) ("* " ++ intToString x ++ " " ++ intToString y ++ " " ++ [intToDigit p] ++ " " ++ r ++ " 10") else l
             where (x, y, powers) = if length (getData l p) > 3 then (stringToInt((getData l p) !! 1), stringToInt((getData l p) !! 2),(getData l p) !! 3)
                                                               else (stringToInt((getData l p) !! 1), stringToInt((getData l p) !! 2),[])
                   r = show ( 1 + length ( filter (=='!' ) powers ))
                   b = 1 + length ( filter (=='+' ) powers)


{-|===Função:
@A função 'numBombs' conta o número de bombas que um determinado jogador tem em campo@

===Variáveis:
@__:: Int:__ Número do jogador
__-> [String]:__ Mapa do jogo@ -}
numBombs :: Int -> [String] -> Int
numBombs p m = length $ filter (\(h:t) -> if h == '*' then (words t)!!2 == [intToDigit p] else False) m

{-|===Função:
@A função 'getData' devolve uma lista de String que corresponde à informação do jogo@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador@ -}
getData :: [String] -> Int -> [String]
getData [] _ = []
getData ((x:xs):y) p | x == intToDigit(p) = words (x:xs)
                     | otherwise = getData y p
{-|===Função:
@A função 'getBombs' devolve um lista com pares de Int's que representam a posição das bombas do jogo@ 

===Variáveis: 
@__:: [String]:__ Mapa do jogo@ -}
getBombs :: [String] -> [(Int,Int)]
getBombs [] = []
getBombs ((x:xs):y) | x == '*' = (stringToInt ((words xs) !! 0),stringToInt ((words xs) !! 1)):getBombs y
                    | otherwise = getBombs y
{-|===Função:
@A função 'getPowerUpBombs' devolve uma lista com pares de Int's que representam a posição dos power-ups bombs@

===Variáveis:
@__:: [String]:__ Mapa do jogo@ -}
getPowerUpBombs :: [String] -> [(Int,Int)]
getPowerUpBombs [] = []
getPowerUpBombs ((x:xs):y) | x == '+' = (stringToInt ((words xs) !! 0),stringToInt ((words xs) !! 1)):getPowerUpBombs y
                           | otherwise = getPowerUpBombs y

{-|===Função:
@A função 'getPowerUpFlames' devolve uma lista de pares de Int's que representam a posição dos power-ups flames@

===Variáveis:
@__:: [String]:__ Mapa do jogo@ -}
getPowerUpFlames :: [String] -> [(Int,Int)]
getPowerUpFlames [] = []
getPowerUpFlames ((x:xs):y) | x == '!' = (stringToInt ((words xs) !! 0),stringToInt ((words xs) !! 1)):getPowerUpFlames y
                            | otherwise = getPowerUpFlames y

{-|===Função:
@A função 'stringToInt' converte uma String num Int@

===Variáveis:
@__:: String:__ String@ -}
stringToInt :: String -> Int
stringToInt [] = 0
stringToInt (h:t) = (digitToInt h) * 10^(length t) + stringToInt t

{-|===Função: 
@A função 'intToString' converte um Int numa String@

===Variáveis:
@__:: Int:__ Int@ -}
intToString :: Int -> String
intToString x | x > 9 = intToString (div x 10) ++ [intToDigit (mod x 10)]
              | otherwise = [intToDigit(x)]

{-|===Função:
@A função 'canMove' verifica se a possição para qual o jogador se quer mover está livre@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__ Possível nova posição do jogador@ -}
canMove :: [String] -> (Int,Int) -> Bool
canMove [] _ = False
canMove l (x,y) | (l!!y)!!x == ' ' = True
                | otherwise = False

{-|===Função:
@A função 'canPlant' verifica se é possível plantar uma bomba numa posição do mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__ Nova posição do jogador@ -}
canPlant :: [String] -> (Int,Int) -> Bool
canPlant [] _ = False
canPlant l (x,y) = not $ elem (x,y) (getBombs l)

{-|===Função:
@A função 'addPower' vai adicionar um power-up se a sua nova posição coincidir com um dos power-ups@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> [String]:__ Power-ups do jogador
__-> (Int,Int):__ Nova posição do jogador@ -}
addPower :: [String] -> String -> (Int,Int) -> String
addPower l [] (x,y)| elem (x,y) (getPowerUpBombs l) = " +"
                   | elem (x,y) (getPowerUpFlames l) = " !"
                   | otherwise = []
addPower l p (x,y) | elem (x,y) (getPowerUpBombs l) = " +" ++ p
                   | elem (x,y) (getPowerUpFlames l) = " " ++ p ++ "!"
                   | otherwise = " " ++ p

{-|===Função:
@A função 'drawAtPlayer' vai atualizar a linha do jogador que fez uma ação@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> (Int,Int):__ Nova posição do jogador
__-> String:__ Power-ups do jogador@ -}
drawAtPlayer :: [String] -> [String] -> Int -> (Int,Int) -> String -> [String]
drawAtPlayer _ [] _ _ _ = []
drawAtPlayer m ((a:as):b) p (x,y) powers | a == intToDigit p = (intToString p ++ " " ++ intToString x ++ " " ++ intToString y ++ (addPower m powers (x,y))):b
                                         | otherwise = (a:as):drawAtPlayer m b p (x,y) powers

{-|===Função:
@A função 'removeLine' vai remover uma linha do mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Posição da linha que se pretende mover@ -}
removeLine :: [String] -> Int -> [String]
removeLine [] _ = []
removeLine (h:t) 0 = t
removeLine (h:t) n = h:removeLine t (n-1)

{-|===Função:
@A função 'insertLine' adiciona uma linha ao mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Posição da nova linha 
__-> String:__ Nova linha do mapa@ -}
insertLine :: [String] -> Int -> String -> [String]
insertLine [] _ s = s:[]
insertLine (h:t) 0 s = s:h:t
insertLine (h:t) i s = h:insertLine t (i-1) s

{-|===Função:
@A função 'bombLine' devolve um Int que corresponde à linha onde se vai encontrar a nova bomba (ordenada)@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__ Coordenadas da nova bomba@ -}
bombLine :: [String] -> (Int,Int) -> Int
bombLine [] (x,y) = 0
bombLine ((a:as):b) (x,y) | isDigit a = 0
                          | a == '*' = if first (x,y) (a:as) then 0 else 1 + bombLine b (x,y)
                          | otherwise = 1 + bombLine b (x,y)

{-|===Função:
@A função 'first' verifica se a nova bomba aparece antes da bomba já colocada@

===Variáveis:
@__:: (Int,Int):__ Coordenadas da nova bomba
__-> String:__ Linha do mapa do jogo de uma bomba já colocada@ -}
first :: (Int,Int) -> String -> Bool
first (x,y) a | y < y2 = True
              | x < x2 && y == y2 = True
              | otherwise = False
              where x2 = stringToInt ( words a !! 1 )
                    y2 = stringToInt ( words a !! 2 )

{-|===Função:
@A função 'powerUpLine' devolve um Int que representa a linha do power-up que foi apanhado@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__ Coordenada do power-up no mapa do jogo@ -}
powerUpLine :: [String] -> (Int,Int) -> Int
powerUpLine [] _ = 0
powerUpLine (h:t) (x,y) = if a then 0
                              else 1 + powerUpLine t (x,y)
                        where a = h == ("! " ++ intToString x ++ " " ++ intToString y) || h == ("+ " ++ intToString x ++ " " ++ intToString y)

