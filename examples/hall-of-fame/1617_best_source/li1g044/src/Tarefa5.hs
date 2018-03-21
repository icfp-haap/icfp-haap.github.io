{-|
Module      : Main
Description : Módulo Haskell que implementa o jogo em Gloss
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Módulo Haskell que implementa o jogo em Gloss usando funções criadas para outras tarefas
-}
module Main where

import Graphics.Gloss.Data.Bitmap
import Bomberman
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Point
import System.Random


type Estado = (InformacaoMapa,[(Int,[Movimento])],Explosao,Coordenadas,Imagens,Int,Janela,Opcoes)
type Imagens = ([Picture],[Picture],([Picture],[Picture],[Picture],[Picture]),([Picture],Int),[Picture],[Picture]) -- (bombas,powerups,jogadores,imagens do menu principal,numeros,imagens do fim do jogo)
data Movimento = Esquerda | Direita | Cima | Baixo | Parado
    deriving (Show,Eq)
data Janela = MenuPrincipal | Jogo | Fim
type Opcoes = (Int,Int,Int,Int) --(nº de jogadores,tamanho do mapa,tipo de câmara,semente do jogo)
type Explosao = [((Int,Int),Float)] -- (Coordenadas das celulas afeadas,duração da explosao)


pedra :: Picture
pedra = rectangleSolid 1 1


-- | Função que desenha as pedras e tijolos de todo o mapa
desenhoMapa :: Mapa -> Float -> [Picture]
desenhoMapa [] _ = []
desenhoMapa (h:t) y = aux h (0,y) ++ desenhoMapa t (y+1)
     where aux [] _ = []
           aux (h:t) (x,y) | h == '#' = translate x (-y) pedra : aux t (x+1,y)
                           | h == '?' = translate x (-y) (color orange pedra) : aux t (x+1,y)
                           | otherwise = aux t (x+1,y)

-- | Função que desenha as pedras e tijolos do mapa para um certo jogador
desenhoMapaAlt :: Mapa -> Float -> Coordenadas -> [Picture]
desenhoMapaAlt [] _ _ = []
desenhoMapaAlt (h:t) y (i,j) | floor y == ceiling y = aux h (0,y) (i,-j) ++ desenhoMapaAlt (h:t) (y+0.5) (i,j)
                             | otherwise = aux h (0,y) (i,-j) ++ desenhoMapaAlt t (y+0.5) (i,j)
     where aux [] _ _ = []
           aux (h:t) (x,y) (i,j)| abs (x-i) < 4.5 && abs (y-j) < 4.5 && h == '#' = translate (x-0.25) (-y+0.25) (scale 0.5 0.5 pedra) : if ceiling x == floor x then aux (h:t) (x+0.5,y) (i,j) else aux t (x+0.5,y) (i,j)
                                | abs (x-i) < 4.5 && abs (y-j) < 4.5 && h == '?' = translate (x-0.25) (-y+0.25) (color orange$scale 0.5 0.5 pedra) : if ceiling x == floor x then aux (h:t) (x+0.5,y) (i,j) else aux t (x+0.5,y) (i,j)
                                | otherwise = if ceiling x == floor x then aux (h:t) (x+0.5,y) (i,j) else aux t (x+0.5,y) (i,j)



-- | Função que desenha os jogadores 1, 2 e 4
jogador :: Movimento -> [Picture] -> Int -> Picture
jogador Parado bmp tick = pictures [bmp !! 4,translate 0 13$bmp !! 0]
jogador Baixo bmp tick = pictures [bmp !! (4+mod (div (-tick) (div fr 15)) 10),translate 0 13$bmp !! 0]
jogador Cima bmp tick = pictures [bmp !! (4+mod (-(div (-tick) (div fr 15))) 10),translate 0 13$bmp !! 2]
jogador Direita bmp tick = pictures [bmp !! (14+mod (div (-tick) (div fr 15)) 10),translate 0 13$bmp !! 1]
jogador Esquerda bmp tick = pictures [bmp !! (24+mod (div (-tick) (div fr 15)) 10),translate 0 13$bmp !! 3]

-- | Função para desenhar o jogador 3
jogadorfunguy :: Movimento -> [Picture] -> Int -> Picture
jogadorfunguy Parado bmp tick = pictures [bmp !! 4,translate 0 17$bmp !! 0]
jogadorfunguy Baixo bmp tick = pictures [bmp !! (4+mod (div (-tick) (div fr 15)) 10),translate 0 17$bmp !! 0]
jogadorfunguy Cima bmp tick = pictures [bmp !! (4+mod (-(div (-tick) (div fr 15))) 10),translate 0 17$bmp !! 2]
jogadorfunguy Direita bmp tick = pictures [bmp !! (14+mod (div (-tick) (div fr 15)) 10),translate 0 17$bmp !! 1]
jogadorfunguy Esquerda bmp tick = pictures [bmp !! (24+mod (div (-tick) (div fr 15)) 10),translate 0 17$bmp !! 3]




data Controlos = Cont Int Accao
data Accao = CUp | CDown | CLeft | CRight | CBomb


-- | Função onde foram definidos os controlos dos 4 jogadores
teclas :: Controlos -> Key
teclas (Cont 0 CDown) = SpecialKey KeyDown
teclas (Cont 0 CUp) = SpecialKey KeyUp
teclas (Cont 0 CLeft) = SpecialKey KeyLeft
teclas (Cont 0 CRight) = SpecialKey KeyRight
teclas (Cont 0 CBomb) = SpecialKey KeySpace
teclas (Cont 1 CDown) = Char 's'
teclas (Cont 1 CUp) = Char 'w'
teclas (Cont 1 CLeft) = Char 'a'
teclas (Cont 1 CRight) = Char 'd'
teclas (Cont 1 CBomb) = Char 'q'
teclas (Cont 2 CDown) = Char 'k'
teclas (Cont 2 CUp) = Char 'i'
teclas (Cont 2 CLeft) = Char 'j'
teclas (Cont 2 CRight) = Char 'l'
teclas (Cont 2 CBomb) = Char 'h'
teclas (Cont 3 CDown) = Char '5'
teclas (Cont 3 CUp) = Char '8'
teclas (Cont 3 CLeft) = Char '4'
teclas (Cont 3 CRight) = Char '6'
teclas (Cont 3 CBomb) = Char '0'


-- | Função que desenha todos o Power Ups
desenhaPUps :: [PowerUps] -> [Picture] -> [Picture]
desenhaPUps [] _ = []
desenhaPUps ((k,(x,y)):t) [b,f]| k=='+' = translate x (-y+0.1) (scale 0.01 0.01 b) : desenhaPUps t [b,f]
                               | otherwise = translate x (-y) (scale 0.01 0.01 f) : desenhaPUps t [b,f]

-- | Função que desenha os Power Ups nas câmaras individuais
desenhaPUpsAlt :: [PowerUps] -> [Picture] -> Coordenadas -> [Picture]
desenhaPUpsAlt [] _ _ = []
desenhaPUpsAlt ((k,(x,y)):t) [b,f] (i,j) | abs (x-i) < 4 && abs ((-y)-j) < 4 = if k=='+' then translate x (-y+0.1) (scale 0.01 0.01 b) : desenhaPUpsAlt t [b,f] (i,j) else translate x (-y) (scale 0.01 0.01 f) : desenhaPUpsAlt t [b,f] (i,j)
                                         | otherwise = desenhaPUpsAlt t [b,f] (i,j)



-- | O estado inicial do jogo.
estadoInicial :: Imagens -> Int -> Estado
estadoInicial p s = (([],[],[],[]),[],[],tamanhoJanelaFloat,p,0,MenuPrincipal,(2,17,-1,s))

-- | Função que retorna o movimento de um dado jogador
buscaMovimento :: Int -> [(Int,[Movimento])] -> Movimento
buscaMovimento n mov = let [(a,b)] = (filter (\(iden,_) -> iden==n) mov) in head b

-- | Função que desenha todos os jogadores
desenhaJogadores :: Coordenadas -> [Jogador] -> [(Int,[Movimento])] -> ([Picture],[Picture],[Picture],[Picture]) -> Int -> Float -> [Picture]
desenhaJogadores  _ [] _ _ _ _ = []
desenhaJogadores (a,b) ((n,(x,y),p):t) mov bmp tick c | n==0 = scale (c/b) (c/b) (translate ((b/c)*x) ((b/c)*y) (scale ((b/c)*(17/600)) ((b/c)*(17/600)) (jogador (buscaMovimento n mov) cain tick))) : desenhaJogadores (a,b) t mov bmp tick c
                                                      | n==1 = scale (c/b) (c/b) (translate ((b/c)*x) ((b/c)*y) (scale ((b/c)*(17/600)) ((b/c)*(17/600)) (jogador (buscaMovimento n mov) bob tick))) : desenhaJogadores (a,b) t mov bmp tick c
                                                      | n==2 = scale (c/b) (c/b) (translate ((b/c)*x) ((b/c)*y) (scale ((b/c)*(17/600)) ((b/c)*(17/600)) (jogadorfunguy (buscaMovimento n mov) funguy tick))) : desenhaJogadores (a,b) t mov bmp tick c
                                                      | otherwise = scale (c/b) (c/b) (translate ((b/c)*x) ((b/c)*y) (scale ((b/c)*(17/600)) ((b/c)*(17/600)) (jogador (buscaMovimento n mov) bluebaby tick))) : desenhaJogadores (a,b) t mov bmp tick c
                where (cain,bob,funguy,bluebaby) = bmp

-- | Função que desenha os jogadores nas câmaras individuais
desenhaJogadoresAlt :: Coordenadas -> [Jogador] -> [(Int,[Movimento])] -> ([Picture],[Picture],[Picture],[Picture]) -> Int -> Coordenadas -> [Picture]
desenhaJogadoresAlt  _ [] _ _ _ _ = []
desenhaJogadoresAlt (a,b) ((n,(x,y),p):t) mov bmp tick (i,j) | n==0 = if abs (x-i) < 4 && abs (y-j) < 4 then scale (17/b) (17/b) (translate ((b/17)*x) ((b/17)*y)  (jogador (buscaMovimento n mov) cain tick)) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j) else desenhaMarcador (a,b) n (x,y) (i,j) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j)
                                                             | n==1 = if abs (x-i) < 4 && abs (y-j) < 4 then scale (17/b) (17/b) (translate ((b/17)*x) ((b/17)*y)  (jogador (buscaMovimento n mov) bob tick)) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j) else desenhaMarcador (a,b) n (x,y) (i,j) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j)
                                                             | n==2 = if abs (x-i) < 4 && abs (y-j) < 4 then scale (17/b) (17/b) (translate ((b/17)*x) ((b/17)*y)  (jogadorfunguy (buscaMovimento n mov) funguy tick)) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j) else desenhaMarcador (a,b) n (x,y) (i,j) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j)
                                                             | otherwise = if abs (x-i) < 4 && abs (y-j) < 4 then scale (17/b) (17/b) (translate ((b/17)*x) ((b/17)*y)  (jogador (buscaMovimento n mov) bluebaby tick)) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j) else desenhaMarcador (a,b) n (x,y) (i,j) : desenhaJogadoresAlt (a,b) t mov bmp tick (i,j)
            where (cain,bob,funguy,bluebaby) = bmp

-- | Função que desenha os marcadores que indicam a posição relativa de um jogador fora da câmara individual
desenhaMarcador :: Coordenadas -> Int -> Coordenadas -> Coordenadas -> Picture
desenhaMarcador (a,b) n (x,y) (i,j) = translate (if abs (x-i)<4 then x else -0.25+i+4*signum(x-i)) (if abs (y-j)<4 then y else 0.25+j+4*signum(y-j)) (marcador n)
                    where marcador 0 = color white$circleSolid 0.2
                          marcador 1 = color (dark green)$circleSolid 0.2
                          marcador 2 = pictures [color (dim yellow)$circleSolid 0.2,color red$circleSolid 0.1]
                          marcador 3 = color blue$circleSolid 0.2

-- | Função que desenha todas as bombas
desenhaBombas :: [Bombas] -> [Picture] -> Float ->[Picture]
desenhaBombas [] _ b = []
desenhaBombas (((x,y),_,_,t):l) [e1,e2] b | even (round ((fromIntegral fr)/t)) = translate x (-y) (scale 0.008 0.008$aux t e1) : desenhaBombas l [e1,e2] b
                                          | otherwise = translate x (-y) (scale 0.008 0.008$aux t e2) : desenhaBombas l [e1,e2] b
        where aux t | t>fromIntegral (fr*2) = scale 1 1
                    | even (round ((fromIntegral fr)/(2*t))) = scale 1.1 0.9
                    | otherwise = scale 0.9 1.1

-- | Função que desenha as bombas visiveis a cada jogador
desenhaBombasAlt :: [Bombas] -> [Picture] -> Float -> Coordenadas -> [Picture]
desenhaBombasAlt [] _ _ _ = []
desenhaBombasAlt (((x,y),_,_,t):l) [e1,e2] b (i,j) | even (round ((fromIntegral fr)/t)) && abs (x-i)<4 && abs ((-y)-j)<4 = translate x (-y) (scale 0.008 0.008$aux t e1) : desenhaBombasAlt l [e1,e2] b (i,j)
                                                   | abs (x-i)<4 && abs ((-y)-j)<4 = translate x (-y) (scale 0.008 0.008$aux t e2) : desenhaBombasAlt l [e1,e2] b (i,j)
                                                   | otherwise = desenhaBombasAlt l [e1,e2] b (i,j)
        where aux t | t>fromIntegral (fr*2) = scale 1 1
                    | even (round ((fromIntegral fr)/(2*t))) = scale 1.1 0.9
                    | otherwise = scale 0.9 1.1



-- | Função que desenha todos os elementos do menu principal
desenhaMenuPrincipal :: ([Picture],Int) -> [Picture] -> (Int,Int,Int) -> Picture
desenhaMenuPrincipal (m,x) n (a,b,c) = pictures [m !! 0,if x == 1 then m !! 2 else m !! 1,if x<2 then (pictures$map (color$dim$greyN 0.5) l) else pictures$map (color$dim$greyN 0.5) l1 ++ (color (bright (greyN 0.5)) (head l2)) : (map (color (dim (greyN 0.5))) (tail l2)),translate 0 (-100)$n!!a,tamanhoMapaMenu n b,if c == 1 then translate 185 (-255)$color white$rotate 45$pictures [rectangleSolid 4 35,rectangleSolid 35 4] else pictures []]
                    where l = [translate 0 (-100)$rectangleSolid 200 50,translate 0 (-200)$rectangleSolid 200 50,translate 185 (-255)$pictures [color white$rectangleSolid 30 30,rectangleSolid 24 24],if a==4 then pictures [] else pictures [translate 160 (-100)$rectangleSolid 80 50,translate 170 (-100)$n!!1,translate 150 (-100)$n!!10],if a==2 then pictures [] else pictures [translate (-160) (-100)$rectangleSolid 80 50,translate (-170) (-100)$n!!11,translate (-150) (-100)$n!!1],if b==99 then pictures [] else pictures [translate 160 (-200)$rectangleSolid 80 50,translate 170 (-200)$n!!2,translate 150 (-200)$n!!10],if b==5 then pictures [] else pictures [translate (-160) (-200)$rectangleSolid 80 50,translate (-170) (-200)$n!!11,translate (-150) (-200)$n!!2]]
                          (l1,l2) = splitAt (x-2) l
                          tamanhoMapaMenu n b = if b<10 then translate 0 (-200)$n !! b else pictures [translate (-10) (-200)$n !! div b 10,translate 10 (-200)$n !! mod b 10]

-- | Função que verifica se um dado jogador existe
existeJogador :: [Jogador] -> Int -> Bool
existeJogador [] _ = False
existeJogador ((j,_,_):l) k = j==k || existeJogador l k

-- | Função que devolve as coordenadas de um dado jogador
buscaCoordJogador :: [Jogador] -> Int -> Coordenadas
buscaCoordJogador ((j,(x,y),_):l) k = if j==k then (x,y) else buscaCoordJogador l k


-- | Função que ordena os jogadores por ordem decrescente da coordenada y para que os jogadores situados mais abaixo no mapa sejam desenhados por cima dos restantes
organizaJogadores :: [Jogador] -> [Jogador]
organizaJogadores [] = []
organizaJogadores (j@(_,(_,y),_):t) = aux t [j]
    where aux [] l = l
          aux j [] = j
          aux (j@(_,(_,y),_):t) (js@(a@(_,(_,y0),_):l)) = if y>=y0 then aux t (j:js) else aux t (a : aux [j] l)

-- | Função que desenha as explosões
desenhaExplosao :: Explosao -> [Picture]
desenhaExplosao [] = []
desenhaExplosao (((x,y),_):t) = (translate (fromIntegral x) (fromIntegral (-y))$color red$rectangleSolid 1 1) : desenhaExplosao t

-- | Função que desenha as explosões visíseis a cada jogador
desenhaExplosaoAlt :: Explosao -> Coordenadas -> [Picture]
desenhaExplosaoAlt [] _ = []
desenhaExplosaoAlt (((x,y),_):t) (i,j) = aux (xa,ya) (i,j) : aux (xa,ya+0.5) (i,j) : aux (xa+0.5,ya) (i,j) : aux (xa+0.5,ya+0.5) (i,j) :desenhaExplosaoAlt t (i,j)
                where aux (x1,y1) (i1,j1) | abs (x1-i1)<4.5 && abs ((-y1)-j1)<4.5 = (translate (x1-0.25) ((-y1+0.25))$color red$rectangleSolid 0.5 0.5)
                                          | otherwise = pictures []
                      aux1 (x1,y1) (i1,j1) = if abs (x1-i1+0.5)<4.5 && abs ((-y1)-j1)<4.5 then (translate (x1+0.25) ((-y1+0.25))$color red$rectangleSolid 0.5 0.5) else pictures []
                      aux2 (x1,y1) (i1,j1) = if abs (x1-i1+0.5)<4.5 && abs ((-y1)-j1-0.5)<4.5 then (translate (x1+0.25) ((-y1-0.25))$color red$rectangleSolid 0.5 0.5) else pictures []
                      aux3 (x1,y1) (i1,j1) = if abs (x1-i1)<4.5 && abs ((-y1)-j1)<4.5 then (translate (x1-0.25) ((-y1+0.25))$color red$rectangleSolid 0.5 0.5) else pictures []
                      aux4 (x1,y1) (i1,j1) = if abs (x1-i1)<4.5 && abs ((-y1)-j1-0.5)<4.5 then (translate (x1-0.25) ((-y1-0.25))$color red$rectangleSolid 0.5 0.5) else pictures []
                      (xa,ya) = (fromIntegral x,fromIntegral y)

-- | Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado ((_,_,_,j),_,_,_,(_,_,_,_,n,f),_,Fim,_) | j==[] = f !! 0
                                                      | otherwise = let [(k,_,_)] = j in pictures [f!!1,translate 0 (100)$scale 4 4$n!!(k+1)]
desenhaEstado (_,_,_,_,(_,_,_,m,n,_),_,MenuPrincipal,(a,b,c,seed)) = desenhaMenuPrincipal m n (a,b,c)
desenhaEstado ((s,p,z,j),mov,exp,(a,b),bmp,tick,Jogo,(_,c,camara,seed)) | camara == -1 = translate (-(a/2)+b/(2*(fromIntegral c))) ((b/2)-b/(2*(fromIntegral c)))$ajuste$pictures [pictures$desenhaPUps p spp,pictures (desenhoMapa s 0),pictures$desenhaBombas z spb b,pictures$desenhaExplosao exp,pictures$desenhaJogadores (a,b) (organizaJogadores j) mov spj tick (fromIntegral c)]
                                                                        | otherwise = pictures [desenhaEcraJ0,desenhaEcraJ1,desenhaEcraJ2,desenhaEcraJ3,pictures [translate (-100) 0$rectangleSolid 20 600,translate (-100) 0$rectangleSolid 600 20,color (greyN 0.5)$translate 300 0$rectangleSolid 200 600,translate 210 0$rectangleSolid 20 600]]
        where ajuste k = scale (b/(fromIntegral c)) (b/(fromIntegral c)) k
              (spb,spp,spj,spm,spn,_) = bmp
              (j0i,j0j)=buscaCoordJogador j 0
              (j1i,j1j)=buscaCoordJogador j 1
              (j2i,j2j)=buscaCoordJogador j 2
              (j3i,j3j)=buscaCoordJogador j 3
              desenhaEcraJ0 = if existeJogador j 0 then translate (-100+(-b)/4) (b/4)$scale 35 35$translate (-j0i) (-j0j)$pictures [pictures$desenhaPUpsAlt p spp (j0i,j0j),pictures (desenhoMapaAlt s 0 (j0i,j0j)),pictures$desenhaBombasAlt z spb b (j0i,j0j),pictures$desenhaExplosaoAlt exp (j0i,j0j),pictures$desenhaJogadoresAlt (a,b) (organizaJogadores j) mov spj tick (j0i,j0j)] else pictures []
              desenhaEcraJ1 = if existeJogador j 1 then translate (-85+(b)/4) (-15+(-b)/4)$scale 35 35$translate (-j1i) (-j1j)$pictures [pictures$desenhaPUpsAlt p spp (j1i,j1j),pictures (desenhoMapaAlt s 0 (j1i,j1j)),pictures$desenhaBombasAlt z spb b (j1i,j1j),pictures$desenhaExplosaoAlt exp (j1i,j1j),pictures$desenhaJogadoresAlt (a,b) (organizaJogadores j) mov spj tick (j1i,j1j)] else pictures []
              desenhaEcraJ2 = if existeJogador j 2 then translate (-85+(b)/4) (b/4)$scale 35 35$translate (-j2i) (-j2j)$pictures [pictures$desenhaPUpsAlt p spp (j2i,j2j),pictures (desenhoMapaAlt s 0 (j2i,j2j)),pictures$desenhaBombasAlt z spb b (j2i,j2j),pictures$desenhaExplosaoAlt exp (j2i,j2j),pictures$desenhaJogadoresAlt (a,b) (organizaJogadores j) mov spj tick (j2i,j2j)] else pictures []
              desenhaEcraJ3 = if existeJogador j 3 then translate (-100+(-b)/4) (-15+(-b)/4)$scale 35 35$translate (-j3i) (-j3j)$pictures [pictures$desenhaPUpsAlt p spp (j3i,j3j),pictures (desenhoMapaAlt s 0 (j3i,j3j)),pictures$desenhaBombasAlt z spb b (j3i,j3j),pictures$desenhaExplosaoAlt exp (j3i,j3j),pictures$desenhaJogadoresAlt (a,b) (organizaJogadores j) mov spj tick (j3i,j3j)] else pictures []

-- | Função que atribui um movimento a um jogador quando se pressiona a tecla que comanda esse movimento
adicionaMovimento :: Key -> [Jogador] -> [(Int,[Movimento])] -> [(Int,[Movimento])]
adicionaMovimento _ [] m = m
adicionaMovimento k ((j,c,p):t) m | k == teclas (Cont j CDown) = aux j m Baixo
                                  | k == teclas (Cont j CUp) = aux j m Cima
                                  | k == teclas (Cont j CRight) = aux j m Direita
                                  | k == teclas (Cont j CLeft) = aux j m Esquerda
                                  | otherwise = adicionaMovimento k t m
                                where aux j [] d = []
                                      aux j ((x,mov):t) d = if x==j then (x,d:mov):t else (x,mov) : aux j t d

-- | Função que retira o movimento a um jogador quando se larga a tecla que comanda esse movimento
retiraMovimento :: Key -> [Jogador] -> [(Int,[Movimento])] -> [(Int,[Movimento])]
retiraMovimento _ [] m = m
retiraMovimento k ((j,c,p):t) m | k == teclas (Cont j CDown) = aux j m Baixo
                                | k == teclas (Cont j CUp) = aux j m Cima
                                | k == teclas (Cont j CRight) = aux j m Direita
                                | k == teclas (Cont j CLeft) = aux j m Esquerda
                                | otherwise = retiraMovimento k t m
                             where aux j [] m = []
                                   aux j ((x,mov):t) m = if x==j then (x,delete m mov):t else (x,mov) : aux j t m




-- | Verifica se o comando efetuado tem a intenção de colocar uma bomba
porBomba :: Key -> [Jogador] -> Bool
porBomba k [] = False
porBomba k ((j,c,p):t) = k == teclas (Cont j CBomb) || porBomba k t

-- | Insere uma nova bomba à lista de bombas
insereBomba :: Key -> [Bombas] -> [Jogador] -> [Bombas]
insereBomba _ b [] = b
insereBomba k b ((j,c,p):t) = if k == teclas (Cont j CBomb) then aux j b c p else insereBomba k b t
                    where aux j b (x,y) (pb,pf) = if aux2 j b pb && aux3 (x,y) b then ((fromIntegral$round x,fromIntegral$round (-y)),j,pf,(fromIntegral fr)*5) : b else b
                          aux2 j b pb = (length$filter (\(_,x,_,_) -> x==j) b) < pb
                          aux3 (x,y) b = filter (\((i,j),_,_,_) -> (round i,round j) == (round x, round y)) b == []
                          inserePorOrdem nb@((xb,yb),_,_,_) [] = [nb]
                          inserePorOrdem nb@((xb,yb),_,_,_) bs@(bi@((xbi,ybi),_,_,_):l) | yb<ybi = nb : bs
                                                                                        | yb>ybi = bi : inserePorOrdem nb l
                                                                                        | xb<xbi = nb : bs
                                                                                        | otherwise = bi : inserePorOrdem nb l

-- | Função que cria um mapa aleatório dentro das opções definidas
criaMapa :: (Int,Int,Int) -> InformacaoMapa
criaMapa (x,y,seed) = (stringToMap a,stringToPU a,[],posicionaJogadores (x,y))
            where a = mapaaux y (seed+1)

-- | Função que posiciona os jogadores na posição inicial
posicionaJogadores :: (Int,Int) -> [Jogador]
posicionaJogadores (x,y) = take x [(0,(1,-1),(1,1)),(1,((fromIntegral y)-2,-((fromIntegral y)-2)),(1,1)),(2,((fromIntegral y)-2,-1),(1,1)),(3,(1,-((fromIntegral y)-2)),(1,1))]

-- | Função que define dá o aributo de Parado a todos os jogadores no início do jogo 
criaMovimento :: Int -> [(Int,[Movimento])]
criaMovimento n = aux n 0
            where aux n k = if n==k then [] else (k,[Parado]) : aux n (k+1)

-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (m,mov,exp,t,bmp,tick,Fim,o) = (([],[],[],[]),[],[],t,bmp,0,MenuPrincipal,o)
reageEvento (EventMotion p) (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,o) | pointInBox p (-200,25) (200,-25) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,1),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (-100,-75) (100,-125) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,2),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (-100,-175) (100,-225) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,3),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (170,-240) (200,-270) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,4),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (120,-75) (200,-125) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,5),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (-200,-75) (-120,-125) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,6),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (120,-175) (200,-225) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,7),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | pointInBox p (-200,-175) (-120,-225) = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,8),sp5,sp6),tick,MenuPrincipal,o)
                                                                                             | otherwise = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,0),sp5,sp6),tick,MenuPrincipal,o)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,o) | k `elem` [0,1,2,3] = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k+1),sp5,sp6),tick,MenuPrincipal,o)
                                                                                                                      | otherwise = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,1),sp5,sp6),tick,MenuPrincipal,o)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,o) | k `elem` [2,3,4] = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k-1),sp5,sp6),tick,MenuPrincipal,o)
                                                                                                                    | otherwise = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,4),sp5,sp6),tick,MenuPrincipal,o)
reageEvento (EventKey p Down _ _) (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,(x,y,z,seed)) | p == SpecialKey KeyLeft && k == 2 && x>2 = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,(x-1,y,z,seed))
                                                                                                              | p == SpecialKey KeyRight && k == 2 && x<4 = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,(x+1,y,z,seed))
                                                                                                              | p == SpecialKey KeyLeft && k == 3 && y>5 = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,(x,y-2,z,seed))
                                                                                                              | p == SpecialKey KeyRight && k == 3 && y<99 = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),tick,MenuPrincipal,(x,y+2,z,seed))
                                                                                                              | p == SpecialKey KeyEnter && k == 1 = (criaMapa (x,y,seed),criaMovimento x,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),fr*60*y^2,Jogo,(x,y,z,seed))
                                                                                                              | p == SpecialKey KeyEnter && k == 4 = (m,mov,exp,t,(sp1,sp2,sp3,(sp4,k),sp5,sp6),0,MenuPrincipal,(x,y,-z,seed))
reageEvento (EventKey (MouseButton LeftButton) Down _ p) (m,mov,exp,t,bmp,_,MenuPrincipal,(x,y,z,seed)) | pointInBox p (-200,25) (200,-25) = (criaMapa (x,y,seed),criaMovimento x,exp,t,bmp,fr*60*y^2,Jogo,(x,y,z,seed))
                                                                                                        | (pointInBox p (120,-75) (200,-125)) && x<4 = (m,mov,exp,t,bmp,0,MenuPrincipal,(x+1,y,z,seed))
                                                                                                        | (pointInBox p (-200,-75) (-120,-125)) && x>2 = (m,mov,exp,t,bmp,0,MenuPrincipal,(x-1,y,z,seed))
                                                                                                        | (pointInBox p (-200,-175) (-120,-225)) && y>5 = (m,mov,exp,t,bmp,0,MenuPrincipal,(x,y-2,z,seed))
                                                                                                        | (pointInBox p (120,-175) (200,-225)) && y<99 = (m,mov,exp,t,bmp,0,MenuPrincipal,(x,y+2,z,seed))
                                                                                                        | (pointInBox p (170,-240) (200,-270)) = (m,mov,exp,t,bmp,0,MenuPrincipal,(x,y,-z,seed))
reageEvento (EventKey k Down _ _) ((m,a,b,js),mov,exp,t,bmp,tick,Jogo,o) = if porBomba k js then ((m,a,insereBomba k b js,js),mov,exp,t,bmp,tick,Jogo,o) else ((m,a,b,js),adicionaMovimento k js mov,exp,t,bmp,tick,Jogo,o)
reageEvento (EventKey k Up _ _) ((m,a,b,js),mov,exp,t,bmp,tick,Jogo,o) = ((m,a,b,js),retiraMovimento k js mov,exp,t,bmp,tick,Jogo,o)
reageEvento (EventResize (x,y)) (m,mov,exp,t,bmp,tick,z,o) = (m,mov,exp,(fromIntegral x,fromIntegral y),bmp,tick,z,o)
reageEvento _ s = s

-- | Funçao que recebe e traduz o movimento para uma mudança de coordenadas em relação com o tempo
moveJogadores :: Mapa -> Float -> Movimento -> Coordenadas -> Coordenadas
moveJogadores m f h (x,y) | h == Baixo && podeMover m (x,y) h = (x,y-(4*f))
                          | h == Cima && podeMover m (x,y) h = (x,y+(4*f))
                          | h == Esquerda && podeMover m (x,y) h = (x-(4*f), y)
                          | h == Direita && podeMover m (x,y) h = (x+(4*f), y)
                          | otherwise = (x,y)

-- | Função que define se um jogador se pode mover para uma certa posição
podeMover :: Mapa -> Coordenadas -> Movimento -> Bool
podeMover m (x,y) Baixo = (concat m) !! ((round x) + ((floor (-y))*length m)+ (length m)) == ' ' || (fromIntegral (round y) < (y+0.25))
podeMover m (x,y) Cima = (concat m) !! ((round x) + ((ceiling (-y))*length m)- (length m)) == ' ' || (fromIntegral (round y) > (y-0.25))
podeMover m (x,y) Direita = (concat m) !! ((floor x) + ((round (-y))*length m)+1) == ' ' || (fromIntegral (round x) > (x-0.25))
podeMover m (x,y) Esquerda = (concat m) !! ((ceiling x) + ((round (-y))*length m)-1) == ' ' || (fromIntegral (round x) < (x+0.25))

-- | Função que atribui um power up a um jogador que o atravesse
recebePUp :: Jogador -> [PowerUps] -> (Int,Int)
recebePUp (_,_,(b,f)) [] = (b,f)
recebePUp (n,(x,y),(b,f)) ((k,(i,j)):l) | (round x,-(round y)) == (round i,round j) = if k=='+' then (b+1,f) else (b,f+1)
                                        | otherwise = recebePUp (n,(x,y),(b,f)) l

-- | Função que retira um power up do mapa quando alguem o apanha
retiraPUp :: [PowerUps] -> Coordenadas -> [PowerUps]
retiraPUp [] _ = []
retiraPUp ((k,(x,y)):l) (i,j) = if (round x,-(round y))==(round i,round j) then l else (k,(x,y)) : retiraPUp l (i,j)

-- | Função que vai alterando a lista dos power ups à medida que o tempo vai passando
pUpTempo :: [PowerUps] -> [Jogador] -> [PowerUps]
pUpTempo p [] = p
pUpTempo p ((_,c,_):l) = pUpTempo (retiraPUp p c) l

-- | Função que vai alterando as propriedades de cada jogador em função do tempo
jogadoresTempo :: Float -> InformacaoMapa -> [(Int,[Movimento])] -> [Jogador]
jogadoresTempo f (m,a,b,((j,c,p):t)) mov = (j,moveJogadores m f (buscaMovimento j mov) c,recebePUp (j,c,p) a) : jogadoresTempo f (m,a,b,t) mov
jogadoresTempo _ _ _ = []

-- | Função que define as bombas em função do tempo
bombasTempo :: Float -> Bombas -> Bombas
bombasTempo f (a,b,c,d) = (a,b,c,d-f)

-- | Função que verifica se os jogadores estão vivos
jSobrevive :: [Int] -> [Jogador] -> [Jogador]
jSobrevive [] _ = []
jSobrevive (h:t) (j@(n,_,_):js) | h==n = j : jSobrevive t js
                                | otherwise = jSobrevive (h:t) js

-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f ((m,a,b,j),mov,exp,t,bmp,tick,Jogo,o) | length j < 2 = ((m,a,b,j),mov,exp,t,bmp,tick,Fim,o)
                                                   | otherwise = ((ma,pUpTempo aa j,ba,jSobrevive (map (\(iden,_,_) -> iden) ja)$jogadoresTempo f (m,a,b,j) mov),mov,nexp++oexp,t,bmp,(tick-1),Jogo,o)
                            where (ma,aa,ba,ja) = stringToEstado$dataEstadoToString$avancaAux (stringToDataEstado (estadoToString (m,a,b,converteJs j)) 0) tick
                                  explosao = celulasAfetadasFinal (stringToDataEstado (estadoToString (m,a,b,j)) 0) (bombExplosion (stringToDataEstado (estadoToString (m,a,b,converteJs j)) 0))
                                  nexp = map (\(xexp,yexp) -> ((xexp,yexp),0.1)) explosao
                                  oexp = filter (\((_,_),texp) -> texp >= 0) (map (\((xexp,yexp),texp) -> ((xexp,yexp),texp -f)) exp)
                                  converteJs = map (\(iden,(xpos,ypos),pj) -> (iden,(xpos,-ypos),pj))
reageTempo f s = s

-- | Frame rate
fr :: Int
fr = 50


-- | Tamanho da Janela em Float
tamanhoJanelaFloat :: (Float,Float)
tamanhoJanelaFloat = let (a,b) = tamanhoJanela in (fromIntegral a,fromIntegral b)

-- | Tamanho da janela
tamanhoJanela :: (Int,Int)
tamanhoJanela = (800,600)

-- | Display mode
dm :: Display
dm = InWindow "Bomberman" tamanhoJanela (0,0)

-- | Função que vai buscar imagens .bmp na diretoria /imagens
buscaImagens :: String -> Int -> FilePath
buscaImagens x y = "../imagens/"++ x ++show y ++ ".bmp"

-- | Função principal que invoca o jogo.
main :: IO ()
main = do p <- mapM loadBMP$map (buscaImagens "Bomba/bomba") [0,1]
          b <- mapM loadBMP$map (buscaImagens "Jogadores/bob") [0..33]
          c <- mapM loadBMP$map (buscaImagens "Jogadores/cain") [0..33]
          bb <- sequence$map loadBMP$map (buscaImagens "Jogadores/???") [0..33]
          fg <- mapM loadBMP$map (buscaImagens "Jogadores/funguy") [0..33]
          m <- mapM loadBMP$map (buscaImagens "Menus/MenuPrincipal") [0..2]
          n <- mapM loadBMP$map (buscaImagens "Menus/num") [0..11]
          mf <- mapM loadBMP$map (buscaImagens "Menus/fim") [0..1]
          pup <- mapM loadBMP$map (buscaImagens "PowerUps/pup") [0..1]
          s <- randomIO
          play dm              -- display mode
               (greyN 0.5)     -- côr do fundo da janela
               fr              -- frame rate
               (estadoInicial (p,pup,(c,b,fg,bb),(m,0),n,mf) s)  -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo
