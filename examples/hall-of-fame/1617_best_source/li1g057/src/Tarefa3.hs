module Tarefa3 where

import System.Environment
import Data.Char
import Data.List
{-|===Função:
@A função 'encode' devolve uma String correspondente à compactação do mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo@ -}
encode :: [String] -> String
encode l = (compressMap2 ('=',-1) $ compressMap (0,0) l $ unlines l)
{-|===Função:
@A função 'decode' devolve uma lista de String's que corresponde à descompactação do mapa@

===Variáveis:
@__:: String:__ Mapa compactada@ -}
decode :: String -> [String]
decode l = mapAllTogether l (size-1) ++ bombDetector l (size-1) ++ flameDetector l (size-1) ++ playerBombDetector l 0 ++ sort (playerDetector l 0)
           where size = sizeOfMap $ numOfMap l

main :: IO ()
main = do
          a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"

{-|===Função:
@A função 'stringToInt' converte uma String num Int@

===Variáveis:
@__:: String:__ String@ -}
stringToInt :: String -> Int
stringToInt [] = 0
stringToInt (h:t) = (digitToInt h) * 10^(length t) + stringToInt t

{-| ===Função:
@A função 'compressMap' vai trocar o mapa do jogo por caracteres para ser compactado@

===Variáveis:
@__:: (Int,Int):__ Coordenadas do jogo
__-> [String]:__ Mapa do jogo
__-> String:__ Mapa do jogo@

===Observação:
@A função 'compressMap' tem como entrada dois mapas, um é mapa auxiliar, e outro é para ser analisado recursivamente@-}
compressMap :: (Int,Int) -> [String] -> String -> String
compressMap (x,y) m [] = []
compressMap (x,y) m ('\n':t) = compressMap (0,y+1 ) m t
compressMap (x,y) m ('#':t) = compressMap (x+1,y) m t
compressMap (x,y) m (h:t) | numOfPlayers (x,y) m > 0 = pp ++ ("KLMN" !! n):compressMap (x,y) (removePlayer n m) (h:t)
                          | elem (x,y) (getPositions '+' m) = if h == '?' then 'C':compressMap (x+1,y) m t else 'E':compressMap (x+1,y) m t
                          | elem (x,y) (getPositions '!' m) = if h == '?' then 'D':compressMap (x+1,y) m t else 'F':compressMap (x+1,y) m t
                          | elem (x,y) (getPositions '*' m) = if bp == "0" then br ++ "G" ++ bt ++ compressMap (x+1,y) m t
                                                         else if bp == "1" then br ++ "H" ++ bt ++ compressMap (x+1,y) m t
                                                         else if bp == "2" then br ++ "I" ++ bt ++ compressMap (x+1,y) m t
                                                                           else br ++ "J" ++ bt ++ compressMap (x+1,y) m t
                          | h == ' ' = 'A':compressMap (x+1,y) m t
                          | h == '?' = 'B':compressMap (x+1,y) m t
                          | otherwise = []
                          where b = getData '*' (x,y) m
                                bp = b!!0
                                br = b!!1
                                bt = if b!!2 == "10" then "0" else b!!2
                                n = getPlayer (x,y) m
                                p = getData (intToDigit n) (x,y) m
                                pb = if length p > 0 then length $ filter (=='+') (p !! 0) else 0
                                pf = if length p > 0 then length $ filter (=='!') (p !! 0) else 0
                                pp = if pb == 0 && pf == 0 then ""
                                                           else if pb == pf then show pb
                                                           else show pb ++ " " ++ show pf
{-|===Função:
@A função 'compressMap2' vai juntar elementos iguais transformando-os num número@

===Variáveis:
@__:: (Char,Int):__ Auxiliar que contém o último caracter e o número de vezes que já se repetiu
__-> String:__ Proveniente do 'compressMap'@

===Observação:
@Inicialmente, os valores de (Char,Int) devem ser ('=',-1) que dá início à função@ -}
compressMap2 :: (Char,Int) -> String -> String
compressMap2 ('=',_) [] = []
compressMap2 (c,i) [] = if i == 1 then [c] else show i ++ [c]
compressMap2 (c,i) (h:t) | elem h "ABCDEF" && c == '=' = compressMap2 (h,1) t
                         | elem h "ABCDEF" && h == c = compressMap2 (h,i+1) t
                         | elem h "ABCDEF" = if i == 1 then c:compressMap2 (h,1) t else show i ++ c:compressMap2 (h,1) t
                         | elem c "ABCDEF" = if i == 1 then c:h:compressMap2 ('=',-1) t else show i ++ c:h:compressMap2 ('=',-1) t
                         | otherwise = h:compressMap2 ('=',-1) t

{-|===Função:
@A função 'getPositions' vai devolver uma lista com pares de Int's que representam as posições do caracter inicial da linha@

===Variáveis:
@__:: Char:__ Caracter inicial da linha
__-> [String]:__ Mapa do jogo@ -}
getPositions :: Char -> [String] -> [(Int,Int)]
getPositions c [] = []
getPositions c ((x:xs):y) | x == c = (stringToInt ((words xs) !! 0),stringToInt ((words xs) !! 1)):getPositions x y
                          | otherwise = getPositions c y

{-|===Função:
@A função 'numOfPlayers' devolve um Int que corresponde ao número de jogadores@

===Variáveis:
@__:: (Int,Int):__ Posição do jogador
__-> [String]:__ Mapa do jogo@ -}
numOfPlayers :: (Int,Int) -> [String] -> Int
numOfPlayers (x,y) m = length $ filter (\(h:t) -> elem h "0123" && (words t)!!0 == show x && (words t)!!1 == show y) m

{-|===Função:
@A função 'getPlayer' devolve o número do jogador nessa posição@

===Variáveis:
@__:: (Int,Int):__ Posição no mapa
__-> [String]:__ Mapa do jogo@

===Observação:
@- No caso de haver mais do que um jogador nessa posição, a função devolve o número do menor jogador
- Se não houver nenhum jogador nessa posição, a função devolve -1@ -}
getPlayer :: (Int,Int) -> [String] -> Int
getPlayer (x,y) m = if elem (x,y) (getPositions '0' m) then 0
               else if elem (x,y) (getPositions '1' m) then 1
               else if elem (x,y) (getPositions '2' m) then 2
               else if elem (x,y) (getPositions '3' m) then 3
               else -1
{-|===Função:
@A função 'getData' devolve uma lista de String's que representa a informação de uma bomba (posição, jogador que a colocou, raio e tempo) ou os power-ups de um jogador@

===Variáveis:
@__:: Char:__ Bomba (*) ou o número do jogador
__-> (Int,Int):__ Posição no mapa da bomba ou do jogador
__-> [String]:__ Mapa do jogo@ -}
getData :: Char -> (Int,Int) -> [String] -> [String]
getData _ _ [] = []
getData e (x,y) (h:t) | [[e],show x,show y] == v = w
                      | otherwise = getData e (x,y) t
                      where v = take 3 $ words h
                            w = drop 3 $ words h

{-|===Função:
@A função 'removePlayer' remove um jogador do mapa@

===Variáveis:
@__:: Int:__ número do jogador
__-> [String]:__ Mapa do jogo@ -}
removePlayer :: Int -> [String] -> [String]
removePlayer _ [] = []
removePlayer n m = filter (\(h:t)->h /= intToDigit n) m

{-|===Função:
@A função 'numOfMap' devolve um Int que corresponde ao número de elementos não pedra do mapa@ 

===Variáveis:
@__:: String:__ Mapa comprimido@-}
numOfMap :: String -> Int
numOfMap x = length $ filter (\h->elem h "ABCDEFGHIJ") $ decompressMap "" x

{-|===Função:
@A função 'sizeOfMap' devolve um Int que corresponde ao tamanho do mapa@

===Variáveis:
@__:: Int:__ Resultado da função 'numOfMap'@ -}
sizeOfMap :: Int -> Int
sizeOfMap n = round $ (5/3) + 2/3 * sqrt ( 3 * x + 1 )
              where x = fromIntegral n
                    y = round $ (5/3) + 2/3 * sqrt ( 3 * x + 1 )
{-|===Função:
@A função 'decompressMap' devolve uma String correspondente ao mapa descomprimido@

===Variáveis:
@__:: String:__ Auxiliar 
__-> String:__ Mapa comprimido@

===Observação:
@A variável auxiliar deve ser "" ou []@-}
decompressMap :: String -> String -> String
decompressMap i [] = i
decompressMap i (h:t) | isDigit h = decompressMap (i ++ [h]) t
                      | elem h "GHIJ" = i ++ h:(head t):decompressMap "" (tail t)
                      | elem h "ABCDEF" = if i == "" then h:decompressMap "" t else replicate (stringToInt i) h ++ decompressMap "" t
                      | otherwise = i ++ h:decompressMap "" t

{-|===Função:
@A função 'mapGenerator' vai gerir um mapa@

===Variáveis:
@__:: Bool:__ Modo Normal ,True, ou Modo com Power-ups e bombas, False
__-> String:__  Mapa comprimido@-}
mapGenerator :: Bool -> String -> String
mapGenerator _ [] = []
mapGenerator True (h:t) | elem h "AEFGHIJ" = ' ':mapGenerator True t
                        | elem h "BCD" = '?':mapGenerator True t
                        | otherwise = mapGenerator True t
mapGenerator False (h:t) | elem h "AB" = ' ':mapGenerator False t
                         | elem h "CE" = '+':mapGenerator False t
                         | elem h "DF" = '!':mapGenerator False t
                         | elem h "GHIJ" = '*':mapGenerator False t
                         | otherwise = mapGenerator False t

{-|===Função:
@A função 'bombsMap' devolve uma lista de informações sobre as bombas (posição, jogador que a plantou, raio e tempo)@

===Variáveis:
@__:: (Int,Int):__ Posição no mapa
__-> Int:__ Tamanho do mapa
__-> String:__ Auxiliar
__-> String:__ Mapa do jogo comprimido@

===Observação: 
@A variável auxiliar tem de ser uma lista vazia@ -}
bombsMap :: (Int,Int) -> Int -> String -> String -> [(String,String,String,String,String)]
bombsMap _ _ _ [] = []
bombsMap (x,y) size i (h:t) | x > size - 2 = bombsMap (correct,y+1) size "" (h:t)
                            | isDigit h = bombsMap (x,y) size (i ++ [h]) t
                            | h == 'G' = (show x,show y,"0",i,time):bombsMap (x+jump,y) size "" (drop 1 t)
                            | h == 'H' = (show x,show y,"1",i,time):bombsMap (x+jump,y) size "" (drop 1 t)
                            | h == 'I' = (show x,show y,"2",i,time):bombsMap (x+jump,y) size "" (drop 1 t)
                            | h == 'J' = (show x,show y,"3",i,time):bombsMap (x+jump,y) size "" (drop 1 t)
                            | elem h "ABCDEF" = bombsMap (x+jump,y) size "" t
                            | otherwise = bombsMap (x,y) size "" t
                            where time = if (head t) == '0' then "10" else [head t]
                                  jump = if odd y then 1 else 2
                                  correct = if odd y then 1 else 1
{-|===Função:
@A função 'playerMap' devolve uma lista de informações sobre o jogador@

===Variáveis:
@__:: (Int,Int):__ Posição do mapa
__-> Int:__ Tamanho do mapa
__-> String:__ Auxiliar
__-> String:__ Auxiliar
__-> String:__ Mapa do jogo comprimido@

===Observação:
@As auxiliares devem ser listas vazias@ -}
playerMap :: (Int,Int) -> Int -> String -> String -> String -> [(String,String,String,String,String)]
playerMap _ _ _ _ [] = []
playerMap (x,y) size b f (h:t) | x > size - 2 = playerMap (correct,y+1) size "" "" (h:t)
                               | isDigit h && f == "" = playerMap (x,y) size (b ++ [h]) "" t
                               | isDigit h = playerMap (x,y) size b (f ++ [h]) t
                               | h == ' ' = playerMap (x,y) size b [head t] (drop 1 t)
                               | h == 'K' = ("0",show x,show y,bombs,flames):playerMap (x,y) size "" "" t
                               | h == 'M' = ("2",show x,show y,bombs,flames):playerMap (x,y) size "" "" t
                               | h == 'N' = ("3",show x,show y,bombs,flames):playerMap (x,y) size "" "" t
                               | h == 'L' = ("1",show x,show y,bombs,flames):playerMap (x,y) size "" "" t
                               | elem h "ABCDEFGHIJ" = playerMap (x+jump,y) size "" "" t
                               | otherwise = playerMap (x,y) size "" "" t
                               where bombs = if b == "" then "" else ' ':replicate (stringToInt b) '+'
                                     flames | b == "" = if f == "" then "" else ' ':replicate (stringToInt f) '!'
                                            | f == "" = replicate (stringToInt b) '!'
                                            | otherwise = replicate (stringToInt f) '!'
                                     jump = if odd y then 1 else 2
                                     correct = if odd y then 1 else 1
{-|===Função:
@A função 'mapAllTogether' devolve uma lista de Strings que representa o mapa completo do jogo@

===Variáveis:
@__:: String:__ Mapa comprimido
__-> Int (l):__ Linha do mapa@ -}
mapAllTogether :: String -> Int -> [String]
mapAllTogether m 0 = [randomMap size m 0 True]
                   where size = sizeOfMap $ numOfMap m
mapAllTogether m l = mapAllTogether m (l-1) ++ [randomMap size m l True]
                   where size = sizeOfMap $ numOfMap m

{-|===Função: 
@A função 'randomMap' devolve uma String que representa o mapa do jogo@
 
===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> String: Mapa do jogo comprimido 
__-> Int:__ Linha do mapa 
__-> Bool:__ Bool para ser usado na função auxiliar 'tradutor'@ -}
randomMap :: Int -> String -> Int -> Bool -> String
randomMap size m l b | l == size = []
                     | l == 0 || l == (size - 1) = replicate size '#'
                     | odd l = "#" ++ fillOpenSpace v l size ++ "#"
                     | even l = "#" ++ intersperse '#' (fillOpenSpace v l size) ++ "#"
                      where v = mapGenerator b $ decompressMap "" m

{-|===Função: 
@A função 'fillOpenSpace' ocupa os espaços não pedras com elementos do mapa de uma determinada linha@

===Variáveis:
@__:: String:__ Lista de elementos do mapa
__-> Int:__ Número da linha do mapa
__-> Int:__ Tamanho do mapa@ -}
fillOpenSpace :: String -> Int -> Int -> String
fillOpenSpace l 0 size = ""
fillOpenSpace l n size = take (numOpenSpaceLine n size) $ drop (gathered n size ) l

{-|===Função: 
@A função 'numOpenSpaceLine' devolve um Int que representa o número de espaços vazios de uma linha sabendo que há espaços vazios já predefinidos@

===Variáveis:
@__:: Int:__ Linha do mapa 
__-> Int:__ Tamanho do mapa@ -}
numOpenSpaceLine :: Int -> Int -> Int
numOpenSpaceLine _ 5 = 0
numOpenSpaceLine l size | l == 0 || l == (size - 1) = 0
                        | odd l = size - 2
                        | even l = size - 2 - ((div size 2) - 1)

{-|===Função:
@A função 'gathered' é uma função auxiliar para a função fillOpenSpace, devolve o número de elementos que a função drop tem de dropar. A função vai verificar às 
linhas anteriores quantos elementos tem para depois a 'fillOpenSpace' dropar esses elementos todos@

===Variáveis:
@__:: Int:__ Linha do mapa
__-> Int:__ Tamanho do mapa@ -}
gathered :: Int -> Int -> Int
gathered _ 5 = 0
gathered l size | l == 0 = 0
                | l == 1 || l == (size - 1) = gathered (l-1) size
                | odd l = numOpenSpaceLine (l-1) size + gathered (l-1) size
                | even l = numOpenSpaceLine (l-1) size + gathered (l-1) size

{-|===Função: 
@A função 'bombDetector' vai percorrer o mapa e vai detetar onde existem bombas. De seguida, vai colocar abaixo do mapa, a posição das bombas@

===Variáveis:
@__:: String:__ Mapa do jogo 
__-> Int:__ Linha do mapa@ -}
bombDetector :: String -> Int -> [String]
bombDetector m 0 = []
bombDetector m l = bombDetector m (l-1) ++ map (\h -> "+ " ++ show h ++ " " ++ show l) (elemIndices '+' $ randomMap size m l False)
                 where size = sizeOfMap $ numOfMap m
{-|===Função:
@A função 'flameDetector' vai percorrer o mapa e vai detetar onde existem flames. De seguida, vai colocar abaixo do mapa, as posições das flames@

===Variáveis:
@__:: String:__ Mapa do jogo 
__-> Int:__ Linha do mapa@ -}
flameDetector :: String -> Int -> [String]
flameDetector m 0 = []
flameDetector m l = flameDetector m (l-1) ++ map (\h -> "! " ++ show h ++ " " ++ show l) (elemIndices '!' $ randomMap size m l False)
                  where size = sizeOfMap $ numOfMap m
{-|===Função:
@A função 'playerBombDetector' devolve uma lista de String's correspondente à informação das bombas já processadas@

===Variáveis:
@__:: String:__ Mapa do jogo 
__-> Int:__ Linha do mapa@ -}
playerBombDetector :: String -> Int -> [String]
playerBombDetector m l = if length list > l then ("* " ++ x ++ " " ++ y ++ " " ++ p ++ " " ++ r ++ " " ++ t):playerBombDetector m (l+1) else []
                  where size = sizeOfMap $ numOfMap m
                        list = bombsMap (1,1) size "" $ decompressMap "" m
                        (x,y,p,r,t) = list !! l

{-|===Função:
@A função 'playerDetector' devolve uma lista de String's correspondente à informação dos jogadores já processadas@

===Variáveis:
@__:: String:__ Mapa do jogo 
__-> Int:__ Linha do mapa@-}
playerDetector :: String -> Int -> [String]
playerDetector m l = if length list > l then (p ++ " " ++ x ++ " " ++ y ++ b ++ f):playerDetector m (l+1) else []
                   where size = sizeOfMap $ numOfMap m
                         list = playerMap (1,1) size "" "" $ decompressMap "" m
                         (p,x,y,b,f) = list !! l
