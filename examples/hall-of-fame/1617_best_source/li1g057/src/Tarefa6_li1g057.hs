  module Tarefa6_li1g057 where

import Data.Char
import Data.List

{-|===Função:
@A função 'bot' devolve o próximo movimento do bot@ 

===Variáveis:
@__:: [String]:__ Mapa do jogo 
__-> Int:__ Número do jogador
__-> Int:__ Tempo que falta até ao final da partida@-}
bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = movePlayer mapa player ticks 10

{-|===Função:
@A função 'drawAtPoint' muda o valor de um ponto no mapa@

===Variáveis:
@__:: [[a]]:__ Mapa do jogo
__-> (Int,Int):__ Coordenadas do ponto a mudar
__-> a:__ Novo valor@-}
drawAtPoint :: [[a]] -> (Int,Int) -> a -> [[a]]
drawAtPoint [] _ _ = []
drawAtPoint (l:ls) (x,0) c = (take x l ++ [c] ++ drop (x+1) l):ls
drawAtPoint (l:ls) (x,y) c = l:drawAtPoint ls (x,y-1) c

{-|===Função:
@A função 'drawAtPoints' muda o valor de vários pontos no mapa@

===Variáveis:
@__:: [[a]]:__ Mapa do jogo
__-> [(Int,Int)]:__ Lista de coordenadas a mudar
__-> a:__ Novo valor@-}
drawAtPoints :: [[a]] -> [(Int,Int)] -> a -> [[a]]
drawAtPoints m [] _ = m
drawAtPoints m (h:t) c = drawAtPoint (drawAtPoints m t c) h c

{-|===Função:
@A função 'emptyZone' converte o mapa num mapa inicial de risco, atribuindo 0 aos pontos vazios e 10 aos restantes@

===Variáveis:
@__:: [String]:__ Mapa do jogo@-}
emptyZone :: [String] -> [[Int]]
emptyZone [] = []
emptyZone (('#':h):t) = (map (\x->if x == ' ' then 0 else 10) ('#':h)):emptyZone t
emptyZone _ = []

{-|===Função:
@A função 'radiusDir' dá o raio da bomba numa determinada direção@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__ Coordenada da bomba
__-> (Int,Int):__ Direção da explusão Ex: (1,0) para a explosão ser para a direita
__-> Int:__ Auxiliar [deve começar por 0]
__-> Int:__ Raio da bomba@-}
radiusDir :: [String] -> (Int,Int) -> (Int,Int) -> Int -> Int -> Int
radiusDir m (x,y) (dx,dy) i r | i == r = 0
                              | value == ' ' = 1 + radiusDir m (x,y) (dx,dy) (i+1) r
                              | otherwise = 0
                              where value = (m!!(y + dy * i))!!(x + dx * i)
{-|===Função:
@A função 'radius' dá o raio da bomba em todas as direções@

===Variáveis:
@__::[String]:__ Mapa do jogo
__-> (Int,Int):__ Coordenadas da bomba
__-> Int:__ Raio da bomba@-}
radius :: [String] -> (Int,Int) -> Int -> (Int,Int,Int,Int)
radius m (x,y) r = (rad right,rad left,rad down,rad up)
                  where rad d = radiusDir m (x,y) d 1 (r+1)
                        right = (1,0)
                        left = (-1,0)
                        down = (0,1)
                        up = (0,-1)
{-|===Função:
@A função 'list' lista a posição de todas as bombas, o seu raio e o tempo que falta para arrebentar@

===Variáveis:
@__:: [String]:__ Mapa do jogo@-}
list :: [String] -> [(Int,Int,Int,Int)]
list [] = []
list ((h:hs):t) | h == '*' = (read (a!!0),read (a!!1),read (a!!3),read (a!!4)):list t
                | otherwise = list t
                  where a = words hs
{-|===Função:
@A função 'listOfPowers' lista a posição de todos os power-ups@

===Variáveis:
@__::[String]:__ Mapa do jogo@-}
listOfPowers :: [String] -> [(Int,Int)]
listOfPowers [] = []
listOfPowers ((h:hs):t) | h == '+' || h == '!' = (read (a!!0),read (a!!1)):listOfPowers t
                        | otherwise = listOfPowers t
                          where a = words hs
{-|===Função:
@A função 'listOfBricks' lista a posição de todos os tijolos@

===Variáveis:
@__::[String]:__ Mapa do jogo
__-> Int:__ Linha a verificar@-}
listOfBricks :: [String] -> Int -> [(Int,Int)]
listOfBricks [] y = []
listOfBricks (h:t) y = map (\x->(x,y)) (elemIndices '?' h) ++ listOfBricks t (y+1)

{-|===Função:
@A função 'explosion' retorna o mapa com o novo risco causado pelas bombas@

===Variáveis:
@__::(Int,Int):__ Coordenadas da bomba
__-> (Int,Int,Int,Int):__ Raio em todas as direções da bomba
__-> Int:__ Tempo que falta para a explosão
__-> [[Int]]:__ Mapa de risco@-}
explosion :: (Int,Int) -> (Int,Int,Int,Int) -> Int -> [[Int]] -> [[Int]]
explosion (x,y) (0,0,0,0) t m = if t > ((m!!y)!!x) then drawAtPoint m (x,y) t else m
explosion (x,y) (r,l,d,u) t m | r > 0 = explode (x + r,y) (r-1,l,d,u)
                              | l > 0 = explode (x - l,y) (r,l-1,d,u)
                              | d > 0 = explode (x,y + d) (r,l,d-1,u)
                              | u > 0 = explode (x,y - u) (r,l,d,u-1)
                              where explode (a,b) c = if t > ((m!!b)!!a) then draw (a,b) c else explosion (x,y) c t m
                                    draw a b = drawAtPoint (explosion (x,y) b t m) a t
{-|===Função:
@A função 'warningZones' retorna o novo mapa de risco@

===Variáveis:
@__::[[Int]]:__ Mapa de risco antes das bombas
__-> [String]:__ Mapa do jogo
__-> (Int,Int,Int,Int)]:__ Lista das propriedades das bombas (posição, raio e tempo para a explosão)@-}
warningZones :: [[Int]] -> [String] -> [(Int,Int,Int,Int)] -> [[Int]]
warningZones n m [] = n
warningZones n m ((x,y,r,t):ts) = warningZones (explosion (x,y) rad (11 - t) n) m ts
                                  where rad = radius m (x,y) r

{-|===Função:
@A função 'brickPriority' retorna o mapa de risco reduzindo para metade todos os blocos adjacentes ao tijolo@

===Variáveis:
@__:: [[Int]]:__ Mapa de risco
__-> [(Int,Int)]:__ Lista das coordenadas dos tijolos@-}
brickPriority :: [[Int]] -> [(Int,Int)] -> [[Int]]
brickPriority m [] = m
brickPriority m ((x,y):t) = drawAtPoint ( drawAtPoint ( drawAtPoint ( drawAtPoint (brickPriority m t) (x,y-1) u) (x,y+1) d ) (x-1,y) r ) (x+1,y) l
                            where p (x,y) = ((brickPriority m t)!!x)!!y
                                  u = div (p (x,y-1)) 2
                                  d = div (p (x,y+1)) 2
                                  r = div (p (x+1,y)) 2
                                  l = div (p (x-1,y)) 2

{-|===Função:
@A função 'movePlayer' retorna o próximo movimento do bot@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> Int:__ Tempo que falta até ao fim da partida
__-> Int:__ Número de passos a verificar@-}
movePlayer :: [String] -> Int -> Int -> Int -> Maybe Char
movePlayer mapa player ticks lvl | list == [] = if list2 == [] then Nothing else Just d2
                                 | otherwise = Just d
                                   where p = playerPosition mapa player
                                         l = moveOptions mapa player ticks p [] 0 lvl
                                         mapOfRisk = riskMap mapa ticks
                                         mapOfGoal = goalMap mapa
                                         goal (x,y) = (mapOfGoal!!y)!!x
                                         risk (x,y) = (mapOfRisk!!y)!!x
                                         risk2 (x,y) = ((dangerMap mapa ticks)!!y)!!x
                                         list = [(goal pos,d) | (pos,d,l) <- filter (\(pos,_,_)->risk pos) l]
                                         list2 = [(risk2 pos,goal pos,d) | (pos,d,l) <- l]
                                         (_,d) = minimum (list)
                                         (_,_,d2) = minimum (list2)

{-|===Função:
@A função 'moveOptions' lista as posições que o bot pode andar, em que direção ir e quantos passos foram precisos@

===Variáveis:
@__::[String]:__ Mapa do jogador
__-> Int:__ Número do jogador
__-> Int:__ Tempo que falta até ao termino da partida
__-> (Int,Int):__ Coordenada do jogador
__-> [((Int,Int),Char,Int)]:__ Auxiliar [deve começar por []]
__-> Int:__ Auxiliar [deve começar por 0]
__-> Int:__ Número de passos a verificar@-}
moveOptions :: [String] -> Int -> Int -> (Int,Int) -> [((Int,Int),Char,Int)] -> Int -> Int -> [((Int,Int),Char,Int)]
moveOptions m pl t (x,y) pos i lvl | i == 0 = moveOptions m pl t (x,y) [(a,b,1) | (a,b) <- filter (\(a,b)-> p a) [((x,y),'B'),((x,y - 1),'U'),((x,y + 1),'D'),((x + 1,y),'R'),((x - 1,y),'L')]] 1 lvl
                                   | i > lvl || new == [] = (new ++ pos)
                                   | otherwise = moveOptions m pl t (x,y) (new ++ pos) (i+1) lvl
                                     where p (x,y) = ((mOpen!!y)!!x)
                                           u n = map (\((x,y),a,b)->((x,y - 1),a,b)) n
                                           d n = map (\((x,y),a,b)->((x,y + 1),a,b)) n
                                           r n = map (\((x,y),a,b)->((x + 1,y),a,b)) n
                                           l n = map (\((x,y),a,b)->((x - 1,y),a,b)) n
                                           mOpen = canMove m t i 0
                                           s = length (mOpen!!0)
                                           coord list = [x | (x,_,_) <- list]
                                           n = map (\(a,b,c)->(a,b,c+1)) $ filter (\(_,_,c)-> c == i) pos
                                           newPos = u n ++ d n ++ r n ++ l n
                                           new = removeDuplicates $ filter (\(a,_,_)-> p a && not (elem a (coord pos))) newPos
                                           removeDuplicates [] = []
                                           removeDuplicates ((a,b,c):t) = (a,b,c) : removeDuplicates (filter (\(d,_,_)-> not (a == d)) t)

{-|===Função:
@A função 'canMove' indica se o jogador pode ir para determinada posição@

===Variáveis:
@__:: [String]:__ Mapa do jogador
__-> Int:__ Tempo que falta até ao fim da partida
__-> Int:__ Número de passos a calcular
__-> Int:__ Auxiliar [deve começar por 0]@-}
canMove :: [String] -> Int -> Int -> Int -> [[Bool]]
canMove m t i z | i == z = map (map (<9) ) n
                | otherwise = canMove (avanca m (t-z)) t i (z+1)
                  where n = dangerMap m (t+z)

{-|===Função:
@A função 'dangerMap' retorna o mapa de risco@

===Variáveis:
@__::[String]:__ Mapa do jogo
__-> Int:__ Tempo que falta até ao termino da partida@-}
dangerMap :: [String] -> Int -> [[Int]]
dangerMap m t = warningZones (maxOfMaps spiralMap (emptyZone m)) m (list m)
                where
                   size = length (m!!0)
                   spiralMap = drawSpiral (spiralIndex (1,1) (startSpiralIndex size size) ((size-2)^2) 'R') t
                   maxOfMaps [] [] = []
                   maxOfMaps (a:as) (b:bs) = (map (\(a,b)->max a b) $ zip a b):maxOfMaps as bs

{-|===Função:
@A função 'riskMap' retorna o mapa com posições seguras@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Tempo que falta até ao termino da partida@-}
riskMap :: [String] -> Int -> [[Bool]]
riskMap m t = map (map (\h->if h > 0 then False else True)) $ dangerMap m t

{-|===Função:
@A função 'goalMap' retorna o mapa com as melhores posições para o jogador@

===Variáveis:
@__:: [String]:__ Mapa do jogador@-}
goalMap :: [String] -> [[Int]]
goalMap m = brickPriority n2 (listOfBricks m 0)
            where powers = listOfPowers m
                  p z (x,y) = (z!!y)!!x
                  n2 = drawAtPoints n powers 0
                  n = spiralIndex (1,1) (startSpiralIndex size size) ((size-2)^2) 'R'
                  size = length $ m!!0

{-|===Função:
@A função 'startSpiralIndex' inicia a espiral (com números)@

===Variáveis:
@__::Int:__ Tamanho do mapa
__-> Int:__ Tamanho do mapa@-}
startSpiralIndex :: Int -> Int -> [[Int]]
startSpiralIndex 0 _ = []
startSpiralIndex n s = if n == s || n == 1 then (replicate s wall):startSpiralIndex (n-1) s
                                           else (wall:(replicate (s-2) 0) ++ [wall]):startSpiralIndex (n-1) s
                       where wall = (s-2)^2+1

{-|===Função:
@A função 'spiralIndex' produz a espiral atribuindo um número a cada ponto@

===Variáveis:
@__::(Int,Int):__ Coordenada inicial
__-> [[Int]]:__ Mapa inicial da espiral
__-> Int:__ Tempo que falta para o fim da partida
__-> Char:__ Direção da espiral@-}
spiralIndex :: (Int,Int) -> [[Int]] -> Int -> Char -> [[Int]]
spiralIndex (x,y) m n d | n == 1 = s
                        | d == 'R' = if empty (x+1,y) then spiralIndex (x+1,y) s (n-1) d else spiralIndex (x,y) m n 'D'
                        | d == 'D' = if empty (x,y+1) then spiralIndex (x,y+1) s (n-1) d else spiralIndex (x,y) m n 'L'
                        | d == 'L' = if empty (x-1,y) then spiralIndex (x-1,y) s (n-1) d else spiralIndex (x,y) m n 'U'
                        | d == 'U' = if empty (x,y-1) then spiralIndex (x,y-1) s (n-1) d else spiralIndex (x,y) m n 'R'
                          where empty (x,y) = ((m!!y)!!x) == 0
                                s = drawAtPoint m (x,y) n
                                size = length $ head m

{-|===Função:
@A função 'startSpiralMap' inicia a espiral (com Booleanos)@

===Variáveis:
@__:: Int:__ Tamanho do Mapa
__-> Int:__ Tamanho do Mapa@-}
startSpiralMap :: Int -> Int -> [[Bool]]
startSpiralMap 0 _ = []
startSpiralMap n s = if n == s || n == 1 then (replicate s False):startSpiralMap (n-1) s
                                         else (False:(replicate (s-2) True) ++ [False]):startSpiralMap (n-1) s

{-|===Função:
@A função 'spiral' produz a espiral@

===Variáveis:
@__:: (Int,Int):__ Coordenada inicial
__-> [[Bool]]:__ Mapa inicial da espiral
__-> Int:__ Auxiliar [deve começar por 0]
__-> Char:__ Direção da espiral@-}
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
@__:: [[Int]]:__ Mapa de risco
__-> Int:__ Tempo para o fim da partida@-}
drawSpiral :: [[Int]] -> Int -> [[Int]]
drawSpiral m t = map (map (\h->if h < t then if h < t-9 then 0 else 10 - (t-h) else 10)) $ m

{-|===Função:
@A função 'playerPosition' retorna a posiçao do jogador@

===Variáveis:
@__:: [String]:__ Mapa do jogador
__-> Int:__ Número do jogador@-}
playerPosition :: [String] -> Int -> (Int,Int)
playerPosition [] _ = (0,0)
playerPosition ((h:t):ts) p = if b then (x,y) else playerPosition ts p
                         where
                           a = words t
                           b = if isDigit h then digitToInt h == p else False
                           (x,y) = (read (a!!0),read (a!!1))

{-|===Função:
@A função 'avanca' retorna o mapa com a passagem do tempo@

===Variáveis:
@__::[String]:__ Mapa atual
__-> Int:__ Tempo para o fim da partida@-}
avanca :: [String] -> Int -> [String]
avanca m t = drawSpiralM (next (listOfExplosion $ reduceTimeOfBombs m)) t

{-|===Função:
@A função 'drawSpiralM' coloca a pedra na espiral@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Tempo para o fim da partida@-}
drawSpiralM :: [String] -> Int -> [String]
drawSpiralM m t = if t > (size-2)^2 then m
                                    else destroyBombs p $ (drawAtPoint m p '#')
                  where p = spiral (1,1) (startSpiralMap size size) t 'R'
                        size = length $ head m

{-|===Função:
@A função 'destroyBombs' retira a bomba do mapa numa determinada poisção@

===Variáveis:
@__:: (Int,Int):__ Coordenada da bomba
__-> [String]:__ Mapa do jogo@-}
destroyBombs :: (Int,Int) -> [String] -> [String]
destroyBombs _ [] = []
destroyBombs (x,y) ((h:hs):t) = if b then t else (h:hs):t
                                where a = words hs
                                      b = if h == '*' then a!!0 == show x && a!!1 == show y else False

{-|===Função:
@A função 'next' explode as bombas@

===Variáveis:
@__:: ([(Int,Int,Int),[String]]):__ Posição da bomba, tempo e mapa do jogo@-}
next :: ([(Int,Int,Int)],[String]) -> [String]
next ([],m) = m
next (((x,y,t):z),m) = next (z, (explosionM (x,y) (radius m (x,y) t) m))

{-|===Função:
@A função 'reduceTimeOfBombs' reduz o tempo das bombas no mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo@-}
reduceTimeOfBombs :: [String] -> [String]
reduceTimeOfBombs [] = []
reduceTimeOfBombs ((x:xs):y) = if x == '*' then (unwords $ newBombLine):reduceTimeOfBombs y
                                           else (x:xs):reduceTimeOfBombs y
                                where bombLine = words xs
                                      newBombLine = "*":(init bombLine) ++ [show (read (last bombLine) - 1)]

{-|===Função:
@A função 'explodeBombs' explode uma bomba@

===Variáveis:
@__:: (Int,Int):__ Coordenada da bomba
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
@A função 'destroyBricks' retira os tijolos@

===Variáveis:
@__:: (Int,Int):__ Posição a retirar
__-> [String]:__ Mapa do jogo@-}
destroyBricks :: (Int,Int) -> [String] -> [String]
destroyBricks (x,y) m = if (m!!y)!!x == '?' then drawAtPoint m (x,y) ' ' else m

{-|===Função:
@A função 'explosionM' explode a bomba em todas as direções@

===Variáveis:
@__:: (Int,Int):__ Posição da bomba
__-> (Int,Int,Int,Int):__ Raio da bomba nas direções
__-> [String]:__ Mapa do jogo@-}
explosionM :: (Int,Int) -> (Int,Int,Int,Int) -> [String] -> [String]
explosionM (x,y) (0,0,0,0) m = m
explosionM (x,y) (r,l,d,u) m | r > 0 = explode (x + r,y) (r-1,l,d,u)
                             | l > 0 = explode (x - l,y) (r,l-1,d,u)
                             | d > 0 = explode (x,y + d) (r,l,d-1,u)
                             | u > 0 = explode (x,y - u) (r,l,d,u-1)
                               where explode a b = explodeBombs a $ destroyBricks a $ explosionM (x,y) b m
