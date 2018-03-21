{-|
Module      : Main
Description : Módulo Haskell que implementa um bot para jogar Bomberman
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Módulo Haskell que implementa um bot definido por uma certa estratégia de jogo.
-}

module Tarefa6_li1g044 where

import Bomberman


-- | Função que deteta o tamanho do mapa
mapSize :: [String] -> Int
mapSize (h:t) = length h

-- | Encontra informação relativa a um jogador
encontraJogador :: Int -> [Jogador] -> Jogador
encontraJogador _ [x] = x
encontraJogador x (h:t) | a == x = h
                        | otherwise = encontraJogador x t
                where (a,_,_) = h

-- | Função que move o jogador para o centro
moveToCenter :: (Int,Int) -> Int -> Mapa -> Maybe Char
moveToCenter (x,y) ms mapa | odd ms && podeMover (x,y) mapa 'U' && abs (x-ms) <= abs (y-ms) && y>ms = Just 'U'
                           | odd ms && podeMover (x,y) mapa 'D' && abs (x-ms) <= abs (y-ms) && y<ms = Just 'D'
                           | odd ms && podeMover (x,y) mapa 'L' && abs (y-ms) < abs (x-ms) && x>ms = Just 'L'
                           | odd ms && podeMover (x,y) mapa 'R' && abs (y-ms) < abs (x-ms) && x<ms = Just 'R'
                           | odd ms && podeMover (x,y) mapa 'D' && y<ms = Just 'D'
                           | odd ms && podeMover (x,y) mapa 'U' && y>ms = Just 'U'
                           | odd ms && podeMover (x,y) mapa 'R' && x<ms = Just 'R'
                           | odd ms && podeMover (x,y) mapa 'L' && x>ms = Just 'L'
                           | even ms && podeMover (x,y) mapa 'U' && abs (x-(ms-1)) <= abs (y-ms) && y>(ms) = Just 'U'
                           | even ms && podeMover (x,y) mapa 'D' && abs (x-(ms-1)) <= abs (y-ms) && y<(ms) = Just 'D'
                           | even ms && podeMover (x,y) mapa 'L' && abs (y-ms) <= abs (x-(ms-1)) && x>(ms-1) = Just 'L'
                           | even ms && podeMover (x,y) mapa 'R' && abs (y-ms) <= abs (x-(ms-1)) && x<(ms-1) = Just 'R'
                           | even ms && podeMover (x,y) mapa 'D' && y<(ms) = Just 'D'
                           | even ms && podeMover (x,y) mapa 'U' && y>(ms) = Just 'U'
                           | even ms && podeMover (x,y) mapa 'R' && x<(ms-1) = Just 'R'
                           | even ms && podeMover (x,y) mapa 'L' && x>(ms-1) = Just 'L'
                           | even ms && podeMover (x,y) mapa 'D' && x /= (ms-1) = Just 'D'
                           | otherwise = Nothing

-- | Função que dita se o bot pode mover para uma dada posição
podeMover :: (Int,Int) -> Mapa -> Char -> Bool
podeMover (x,y) mapa c | c=='U' && y<1 = False
                       | c=='D' && y>=(length$head mapa)-1 = False
                       | c=='L' && x<1 = False
                       | c=='R' && x>=(length$head mapa)-1 = False
                       | c=='U' = checkPos2 mapa (x,y-1) == ' ' 
                       | c=='D' = checkPos2 mapa (x,y+1) == ' '
                       | c=='L' = checkPos2 mapa (x-1,y) == ' '
                       | otherwise = checkPos2 mapa (x+1,y) == ' '

-- | Função que decide se o bot deve colocar uma bomba
poeBomba :: (Int,Int) -> Mapa -> Bool
poeBomba (x,y) m = checkPos2 m (x-1,y) == '?' || checkPos2 m (x+1,y) == '?' || checkPos2 m (x,y-1) == '?' || checkPos2 m (x,y+1) == '?' 


-- | Função que comanda o bot para fugir das bombas
evitaBomba :: [Bombas] -> Mapa -> Coordenadas -> Maybe Char
evitaBomba [] _ _ = Nothing
evitaBomba (((i,j),_,f,t):bs) m (x,y) | i /= x && j /= y = evitaBomba bs m (x,y)
                                      | i == x && j == y = evitaBombaAux1 m (round x,round y)
                                      | round (abs (y-j)) <= f && x==i = evitaBombaAux2 m (round i,round j) f (round x,round y)
                                      | round (abs (x-i)) <= f && y==j = evitaBombaAux3 m (round i,round j) f (round x,round y)
                                      | otherwise = evitaBomba bs m (x,y)


-- | Função que decide o que fazer quando o bot está nas mesmas coordenadas que uma bomba
evitaBombaAux1 :: Mapa -> (Int,Int) -> Maybe Char
evitaBombaAux1 mapa (x,y) | podeMover (x,y) mapa 'D' && (podeMover (x,y+1) mapa 'R' || podeMover (x,y+1) mapa 'L') = Just 'D'
                          | podeMover (x,y) mapa 'U' && (podeMover (x,y-1) mapa 'R' || podeMover (x,y-1) mapa 'L') = Just 'U'
                          | podeMover (x,y) mapa 'R' && (podeMover (x+1,y) mapa 'U' || podeMover (x+1,y) mapa 'D') = Just 'R'
                          | podeMover (x,y) mapa 'L' && (podeMover (x-1,y) mapa 'U' || podeMover (x-1,y) mapa 'D') = Just 'L'
                          | podeMover (x,y) mapa 'D' && podeMover (x,y+1) mapa 'D' = Just 'D'
                          | podeMover (x,y) mapa 'U' && podeMover (x,y-1) mapa 'U' = Just 'U'
                          | podeMover (x,y) mapa 'R' && podeMover (x+1,y) mapa 'R' = Just 'R'
                          | podeMover (x,y) mapa 'L' && podeMover (x-1,y) mapa 'L' = Just 'L'
                          | podeMover (x,y) mapa 'D' = Just 'D'
                          | podeMover (x,y) mapa 'U' = Just 'U'
                          | podeMover (x,y) mapa 'R' = Just 'R'
                          | otherwise = Just 'L'

-- | Função que decide o que fazer quando o bot está na mesma coluna que uma bomba
evitaBombaAux2 :: Mapa -> (Int,Int) -> Int -> (Int,Int) -> Maybe Char
evitaBombaAux2 m (i,j) f (x,y) | podeMover (x,y) m 'R' = Just 'R'
                               | podeMover (x,y) m 'L' = Just 'L'
                               | podeMover (x,y) m 'D' && abs (y+1-j) > f = Just 'D'
                               | podeMover (x,y) m 'U' && abs (y-1-j) > f = Just 'U'
                               | podeMover (x,y) m 'D' && (x,y+1) /= (i,j) && (podeMover (x,y+1) m 'R' || podeMover (x,y+1) m 'L') = Just 'D'
                               | podeMover (x,y) m 'U' && (x,y-1) /= (i,j) && (podeMover (x,y-1) m 'R' || podeMover (x,y-1) m 'L') = Just 'U'
                               | podeMover (x,y) m 'D' && podeMover (x,y+2) m 'D' && podeMover (x,y+1) m 'D' = Just 'D'
                               | podeMover (x,y) m 'U' && podeMover (x,y-2) m 'U' && podeMover (x,y-1) m 'U' = Just 'U'
                               | podeMover (x,y) m 'D' = Just 'D'
                               | otherwise = Just 'U'

-- | Função que decide o que fazer quando o bot está na mesma linha que uma bomba
evitaBombaAux3 :: Mapa -> (Int,Int) -> Int -> (Int,Int) -> Maybe Char
evitaBombaAux3 m (i,j) f (x,y) | podeMover (x,y) m 'D' = Just 'D'
                               | podeMover (x,y) m 'U' = Just 'U'
                               | podeMover (x,y) m 'R' && abs (x+1-i) > f = Just 'R'
                               | podeMover (x,y) m 'L' && abs (x-1-i) > f = Just 'L'
                               | podeMover (x,y) m 'R' && (x+1,y) /= (i,j) && (podeMover (x+1,y) m 'D' || podeMover (x+1,y) m 'U') = Just 'R'
                               | podeMover (x,y) m 'L' && (x-1,y) /= (i,j) && (podeMover (x-1,y) m 'D' || podeMover (x-1,y) m 'U') = Just 'L'
                               | podeMover (x,y) m 'R' && podeMover (x+2,y) m 'R' && podeMover (x+1,y) m 'R' = Just 'R'
                               | podeMover (x,y) m 'L' && podeMover (x-2,y) m 'L' && podeMover (x-1,y) m 'L' = Just 'L'
                               | podeMover (x,y) m 'R' = Just 'R'
                               | otherwise = Just 'L' 

-- | Define as bombas a que o bot deve prestar atenção
prestaAtencao :: [Bombas] -> (Coordenadas) -> [Bombas]
prestaAtencao [] _ = []
prestaAtencao (b@((i,j),_,f,t):bs) (x,y) | round t > f + 2 || round (abs (x-i)) > f+1 || round (abs (y-j)) > f+1 = prestaAtencao bs (x,y)
                                         | otherwise = b : prestaAtencao bs (x,y)

-- | Função que define quando o bot deve atacar
atacaJogador :: [Jogador] -> (Coordenadas) -> Bool
atacaJogador [] _ = False
atacaJogador ((_,(x,y),_):js) (i,j) = (x==i && (abs (y-j))<2) || (y==j && (abs (x-i))<2) || atacaJogador js (i,j)

-- | Função que verifica se as bombas que estão no mapa vão explodir
verificaTicksBombas :: [Bombas] -> Int -> Bool
verificaTicksBombas [] _ = True
verificaTicksBombas ((_,_,_,t):bs) tick = tick <= round t && verificaTicksBombas bs tick

-- | Função que controla o bot
bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks | ticks <=10 && verificaTicksBombas b ticks && ((mod ms 4 == 1 && (round x,round y) == ((div ms 2) -1,div ms 2)) || (mod ms 4 == 3 && (round x,round y) == (div ms 2,div ms 2))) = Just 'B'
                      | prestaAtencao b (x,y) /= [] = evitaBomba (prestaAtencao b (x,y)) m (x,y)
                      | poeBomba (round x,round y) m && (checkActiveBombs mapa player) < pb = Just 'B'
                      | ticks > 30 && atacaJogador (filter (\(iden,coord,powup) -> iden /= player) j) (x,y) && (checkActiveBombs mapa player) < pb = Just 'B'
                      | otherwise = moveToCenter (round x,round y) (ms`div`2) mapa
            where (m,p,b,j) = stringToEstado mapa
                  (_,(x,y),(pb,pf)) = encontraJogador player j 
                  ms = mapSize m
