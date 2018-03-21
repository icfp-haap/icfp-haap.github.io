module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import System.Random
import Data.Char
import Tarefa6_li1g057 (movePlayer)

-- | Uma representação do estado do jogo.
type Estado = ([Picture],Stage)

data Stage = Menu Int Int | Rules | Game [String] Int Int [Int] ([(Int,Int)],[(Int,Int)]) Bool | Winner [Int] Int | Credits
data Images = Bg | Bomb | BombExplosion | BombFire | Brick | PowerUpBombs | PowerUpFlames | Rock | MenuBackground | Button String | NumberButton Int | Avatar Int | Number Int | PauseButton String | Result Char | Help | Developers

{-|===Função:
@Estado inicial do jogo@-}
estadoInicial :: IO Estado
estadoInicial = do bg <- loadBMP  "Sprites/Background.bmp"
                   bomb <- loadBMP "Sprites/Bomb.bmp"
                   bombExplosion <- loadBMP "Sprites/BombExplosion.bmp"
                   bombFire <- loadBMP "Sprites/BombFire.bmp"
                   brick <- loadBMP "Sprites/Brick.bmp"
                   powerUpBombs <- loadBMP "Sprites/PowerUpBombs.bmp"
                   powerUpFlames <- loadBMP "Sprites/PowerUpFlames.bmp"
                   rock <- loadBMP "Sprites/Rock.bmp"
                   menuBackground <- loadBMP "Sprites/MenuBackground.bmp"
                   tamanhoButton <- loadBMP "Sprites/tamanhoButton.bmp"
                   leftButton <- loadBMP "Sprites/leftButton.bmp"
                   rightButton <- loadBMP "Sprites/rightButton.bmp"
                   button5 <- loadBMP "Sprites/Button5.bmp"
                   button7 <- loadBMP "Sprites/Button7.bmp"
                   button9 <- loadBMP "Sprites/Button9.bmp"
                   button11 <- loadBMP "Sprites/Button11.bmp"
                   button13 <- loadBMP "Sprites/Button13.bmp"
                   button15 <- loadBMP "Sprites/Button15.bmp"
                   button17 <- loadBMP "Sprites/Button17.bmp"
                   button19 <- loadBMP "Sprites/Button19.bmp"
                   avatarButton <- loadBMP "Sprites/AvatarButton.bmp"
                   startButton <- loadBMP "Sprites/StartButton.bmp"
                   cat <- loadBMP "Sprites/Cat.bmp"
                   panda <- loadBMP "Sprites/Panda.bmp"
                   rabbit <- loadBMP "Sprites/Rabbit.bmp"
                   tiger <- loadBMP "Sprites/Tiger.bmp"
                   n0 <- loadBMP "Sprites/Number0.bmp"
                   n1 <- loadBMP "Sprites/Number1.bmp"
                   n2 <- loadBMP "Sprites/Number2.bmp"
                   n3 <- loadBMP "Sprites/Number3.bmp"
                   n4 <- loadBMP "Sprites/Number4.bmp"
                   n5 <- loadBMP "Sprites/Number5.bmp"
                   n6 <- loadBMP "Sprites/Number6.bmp"
                   n7 <- loadBMP "Sprites/Number7.bmp"
                   n8 <- loadBMP "Sprites/Number8.bmp"
                   n9 <- loadBMP "Sprites/Number9.bmp"
                   pause <- loadBMP "Sprites/Pause.bmp"
                   returnToMatch <- loadBMP "Sprites/ReturnToMatch.bmp"
                   returnToMenu <- loadBMP "Sprites/ReturnToMenu.bmp"
                   toPause <- loadBMP "Sprites/ToPause.bmp"
                   winner <- loadBMP "Sprites/Winner.bmp"
                   tie <- loadBMP "Sprites/Tie.bmp"
                   helpButton <- loadBMP "Sprites/HelpButton.bmp"
                   creditButton <- loadBMP "Sprites/CreditButton.bmp"
                   help <- loadBMP "Sprites/Help.bmp"
                   developers <- loadBMP "Sprites/Developers.bmp"
                   return ([bg,bomb,bombExplosion,bombFire,brick,powerUpBombs,powerUpFlames,rock,menuBackground,tamanhoButton,
                    leftButton,rightButton,button5,button7,button9,button11,button13,button15,button17,button19,avatarButton,startButton,
                    cat,panda,rabbit,tiger,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,pause,returnToMatch,returnToMenu,toPause,winner,tie,helpButton,creditButton,help,developers],Menu 5 0)

{-|===Função:
@A função 'img' retorna a imagem correspondente@

===Variáveis:
@__:: [Picture]:__Lista de todas as imagens@
__-> Images:__Nome da Imagem@-}
img :: [Picture] -> Images -> Picture
img p Bg = p!!0
img p Bomb = p!!1
img p BombExplosion = p!!2
img p BombFire = p!!3
img p Brick = p!!4
img p PowerUpBombs = p!!5
img p PowerUpFlames = p!!6
img p Rock = p!!7
img p MenuBackground = p!!8
img p (Button "Tamanho") = p!!9
img p (Button "Left") = p!!10
img p (Button "Right") = p!!11
img p (Button "Start") = p !! 21
img p (Button "Avatar") = p !! 20
img p (Button "Help") = p !! 42
img p (Button "Credit") = p !! 43
img p (NumberButton x) = p!!(range(12,20) (div x 2 + 10))
img p (Avatar x) = p!!(range (22,25) (22 + x))
img p (Number x) = p!!(range (26,36) (26 + x))
img p (PauseButton "Pause") = p!!36
img p (PauseButton "Match") = p!!37
img p (PauseButton "Menu") = p!!38
img p (PauseButton "ToPause") = p!!39
img p (Result 'W') = p!!40
img p (Result 'T') = p!!41
img p Help = p!!44
img p Developers = p!!45

{-|===Função:
@A função 'desenhaEstado' desenha o estado do jogo@-}
desenhaEstado :: Estado -> Picture
desenhaEstado (p,Menu tamanho avatar) = pictures $ menu p tamanho avatar
desenhaEstado (p,Game mapa player ticks avatares explosions pause) = pictures $ desenhaMapa mapa p ticks avatares explosions pause
desenhaEstado (p,Winner avatares 4) = pictures $ [Translate 0 200 $ img p (Result 'T'),img p Bomb,Translate 0 (-200) $ img p (PauseButton "Menu")]
desenhaEstado (p,Winner avatares player) = pictures $ [Translate 0 200 $ img p (Result 'W'),img p (Avatar (avatares!!player)),Translate 0 (-200) $ img p (PauseButton "Menu")]
desenhaEstado (p,Rules) = pictures $ [img p Help]
desenhaEstado (p,Credits) = pictures $ [img p Developers]

{-|===Função:
@A função 'desenhaMapa' vai desenhar o mapa do jogo@

===Variáveis:
@__:: [String]:__Mapa do jogo
__-> [Picture]:__ Lista das imagens
__-> Int:__ Tempo para o fim da partida
__-> [Int]:__ Lista dos índices dos avatares
__-> ([(Int,Int)],[(Int,Int)]]):__ Lista de bombas que explodiram e o seu impacto@-}
desenhaMapa :: [String] -> [Picture] -> Int -> [Int] -> ([(Int,Int)],[(Int,Int)]) -> Bool -> [Picture]
desenhaMapa [] _ _ _ _ _ = []
desenhaMapa m p t a e pause = printM p Bg ' ' m 0 ++ printPowerUpBombs p m ++ printPowerUpFlames p m ++ printExplosion p size e ++ printM p Brick '?' m 0 ++ printAvatar p a m ++ printBomb p m ++ printM p Rock '#' m 0 ++ printSideBar p a m t ++ printPause p pause
                            where size = length (m!!0)
{-|===Função:
@A função 'printM' substitui um caracter por uma imagem@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> Images:__Nome da imagem
__-> Char:__Valor a substituir
__-> [String]:__Mapa
__-> Int:__Linha inicial@-}
printM :: [Picture] -> Images -> Char -> [String] -> Int -> [Picture]
printM p i n (('#':h):t) l = map (\h->cell p i size (fromIntegral(h),fromIntegral(l))) objects ++ printM p i n t (l+1)
                             where size = 1 + length h
                                   objects = elemIndices n ('#':h)
printM _ _ _ _ _ = []
{-|===Função:
@A função 'printAvatar' desenha um avatar@

===Variáveis:}
@__:: [Picture]:__Lista de Imagens
__-> [Int]:__Lista dos índices dos avatares
__-> [String]:__Mapa do jogo@-}
printAvatar :: [Picture] -> [Int] -> [String] -> [Picture]
printAvatar p a (h:t) = map (\(h:t)-> cell p (Avatar $ a!!digitToInt h) size (fromIntegral(read $ words t !! 0),fromIntegral(read $ words t !! 1))) players
                        where  size = length h
                               players = filter (\(h:_)-> isDigit h) (h:t)
{-|===Função:
@A função 'printBomb' desenha a bomba@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> [String]:__Mapa do jogo@-}
printBomb :: [Picture] -> [String] -> [Picture]
printBomb p (h:t) = map (\(h:t)-> cell p Bomb size (fromIntegral(read $ words t !! 0),fromIntegral(read $ words t !! 1))) bombs
                    where  size = length h
                           bombs = filter (\(h:_)-> h == '*') (h:t)
{-|===Função:
@A função 'printPowerUpBombs' desenha os power-ups Bombs@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> [String]:__Mapa do jogo@-}
printPowerUpBombs :: [Picture] -> [String] -> [Picture]
printPowerUpBombs p (h:t) = map (\(h:t)-> cell p PowerUpBombs size (fromIntegral(read $ words t !! 0),fromIntegral(read $ words t !! 1))) powers
                            where  size = length h
                                   powers = filter (\(h:_)-> h == '+') (h:t)

{-|===Função:
@A função 'printPowerUpFlames' desenha os power-ups Flames@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> [String]:__Mapa do jogo@-}
printPowerUpFlames :: [Picture] -> [String] -> [Picture]
printPowerUpFlames p (h:t) = map (\(h:t)-> cell p PowerUpFlames size (fromIntegral(read $ words t !! 0),fromIntegral(read $ words t !! 1))) powers
                            where  size = length h
                                   powers = filter (\(h:_)-> h == '!') (h:t)

{-|===Função:
@A função 'printExplosion' desenha as explosões@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> Int:__Tamanho do mapa
__-> ([(Int,Int)],[(Int,Int)]):__Lista de coordenadas das bombas que explodiram e seu raio@-}
printExplosion :: [Picture] -> Int -> ([(Int,Int)],[(Int,Int)]) -> [Picture]
printExplosion p s (b,e) = explosion ++ fire
                           where explosion = map (cell p BombExplosion s) (toFloat b)
                                 fire = map (cell p BombFire s) (toFloat e)
                                 toFloat x = map (\(a,b)->(fromIntegral a,fromIntegral b)) x

{-|===Função:
@A função 'printSideBar' desenha a barra lateral@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> [Int]:__Lista dos avatares
__-> [String]:__Mapa do jogo
__-> Int:__Tempo que falta para acabar a partida@-}
printSideBar :: [Picture] -> [Int] -> [String] -> Int -> [Picture]
printSideBar p _ [] ticks = (Translate 290 (-250) $ img p $ PauseButton "ToPause"):[Translate i 250 $ img p $ Number (digitToInt t) | (i,t) <- zip [350,290,230] $ reverse $ show (div ticks fr)]
printSideBar p a ((h:hs):t) ticks | isDigit h = (Translate 290 (150 - fromIntegral (100 * digitToInt h)) $ Scale 0.4 0.4 $ img p $ Avatar (a!!(digitToInt h))):printSideBar p a t ticks
                                  | otherwise = printSideBar p a t ticks

{-|===Função:
@A função 'printPause' desenha a pausa@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> Bool:__ True caso o jogo esteja em pause, False caso contrário@-}
printPause :: [Picture] -> Bool -> [Picture]
printPause _ False = []
printPause p True = [Color (makeColor 0 0 0 0.5) $ rectangleSolid 800 600,Translate 0 100 $ img p (PauseButton "Pause"),Translate 0 25 $ img p (PauseButton "Match"),Translate 0 (-50) $ img p (PauseButton "Menu")]

{-|===Função:
@A função 'cell' desenha uma imagem ajustada ao tamanho do mapa@

===Vairáveis:
@__:: [Picture]:__Lista de imagens
__-> Images:__Nome da imagem
__-> Int:__Tamanho do mapa
__-> (Float,Float):__Posição da imagem@-}
cell :: [Picture] -> Images -> Int -> (Float,Float) -> Picture
cell p i size (x,y) = Translate (-100 - cx) cy $ Scale factor factor $ Translate (x * 256) (y * (-256)) $ img p i
                   where  factor = 2.25/fromIntegral(size)
                          cx = 288 * (1 - 1/fromIntegral(size))
                          cy = 288 * (1 - 1/fromIntegral(size))

{-|===Função:
@A função 'menu' desenha o menu@

===Variáveis:
@__:: [Picture]:__Lista de imagens
__-> Int:__Tamanho
__-> Int:__Avatar@-}
menu :: [Picture] -> Int -> Int -> [Picture]
menu p size avatar = [img p MenuBackground,Translate (-220) 150 $ img p (Button "Tamanho"),Translate (-295) 90 $ img p (Button "Left"),Translate (-220) 90 $ img p (NumberButton size),Translate (-145) 90 $ img p (Button "Right"),
                      Translate (-220) 30 $ img p (Button "Avatar"),Translate (-220) (-100) $ img p (Button "Start"),Translate (-295) (-30) $ img p (Button "Left"), Translate (-220) (-30) $ Scale 0.1 0.1 $ img p (Avatar avatar),Translate (-145) (-30) $ img p (Button "Right"),
                      Translate 50 80 $ img p (Button "Help"), Translate 50 (-20) $ img p (Button "Credit")]

{-|===Função:
@A função 'clickButton' verifica se o utilizador clicou num botão@

===Variáveis:
@__:: (Float,Float):__Posição do cursor
__-> (Float,Float):__Posição do botão
__-> Float:__Largura do botão
__-> Float:__Altura do botão@-}
clickButton :: (Float,Float) -> (Float,Float) -> Float -> Float -> Bool
clickButton (cx,cy) (x,y) w h = and [cx <= x + w/2,cx >= x - w/2,cy <= y + h/2,cy >= y - h/2]

{-|===Função:
@A função 'range' retorna um número entre o mínimo e o máximo@

===Variáveis:
@__:: (Int,Int):__Mínimo e máximo
__-> Int:__Valor@-}
range :: (Int,Int) -> Int -> Int
range (min,max) n | n < min = min
                  | n > max = max
                  | otherwise = n

{-|===Função:
@A função 'reageEvento' altera o estado do jogo quando acontece um evento@-}
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (p,Menu size avatar) = (p,Menu (range (5,19) (size - 2)) avatar)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (p,Menu size avatar) = (p,Menu (range (5,19) (size + 2)) avatar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (p,Menu size avatar) = (p,Menu size (range (0,3) (avatar + 1)))
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (p,Menu size avatar) = (p,Menu size (range (0,3) (avatar - 1)))
reageEvento (EventKey (SpecialKey KeyEnter) Down _ (x,y)) (p,Menu size avatar) = (p,Game (mapa size (truncate ((x+y)*y)) ++ ["0 1 1","1 " ++ show (size - 2) ++ " 1","2 " ++ show (size - 2) ++ " " ++ show (size - 2),"3 1 " ++ show (size - 2)]) 0 (size^2) (avatar : (filter (/=avatar) [0,1,2,3])) ([],[]) False)
reageEvento (EventKey (MouseButton LeftButton) Up _ (x,y)) (p,Menu size avatar) | clickButton (x,y) (-295,90) 50 50 = (p,Menu (range (5,19) (size - 2)) avatar)
                                                                                | clickButton (x,y) (-145,90) 50 50 = (p,Menu (range (5,19) (size + 2)) avatar)
                                                                                | clickButton (x,y) (-295,-30) 50 50 = (p,Menu size (range (0,3) (avatar - 1)))
                                                                                | clickButton (x,y) (-145,-30) 50 50 = (p,Menu size (range (0,3) (avatar + 1)))
                                                                                | clickButton (x,y) (-200,-100) 150 50 = (p,Game (mapa size (truncate ((x+y)*y)) ++ ["0 1 1","1 " ++ limit ++ " 1","2 " ++ limit ++ " " ++ limit,"3 1 " ++ limit]) 0 (size^2) (avatar : (filter (/=avatar) [0,1,2,3])) ([],[]) False)
                                                                                | clickButton (x,y) (50,80) 200 50 = (p,Rules)
                                                                                | clickButton (x,y) (50,-20) 200 50 = (p,Credits)
                                                                                | otherwise = (p,Menu size avatar)
                                                                                where
                                                                                  limit = show (size - 2)
reageEvento (EventKey (SpecialKey KeyHome) Down _ _) (p,Game mapa jogador ticks avatares explosions pause) = (p,Game mapa jogador ticks avatares explosions (not pause))
reageEvento (EventKey (MouseButton LeftButton) Up _ (x,y)) (p,Game mapa jogador ticks avatares explosions False) = if clickButton (x,y) (290,-250) 100 50 then (p,Game mapa jogador ticks avatares explosions True) else (p,Game mapa jogador ticks avatares explosions False)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'R') jogador ticks avatares explosions False)
reageEvento (EventKey (Char 'd') Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'R') jogador ticks avatares explosions False)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'L') jogador ticks avatares explosions False)
reageEvento (EventKey (Char 'a') Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'L') jogador ticks avatares explosions False)
reageEvento (EventKey (Char 'w') Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'U') jogador ticks avatares explosions False)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'U') jogador ticks avatares explosions False)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'D') jogador ticks avatares explosions False)
reageEvento (EventKey (Char 's') Down _ _) (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'D') jogador ticks avatares explosions False)
reageEvento (EventKey (Char 'b') Down _ _)  (p,Game mapa jogador ticks avatares explosions False) = (p,Game (move mapa jogador 'B') jogador ticks avatares explosions False)
reageEvento (EventKey (MouseButton LeftButton) Up _ (x,y)) (p,Game mapa jogador ticks avatares explosions True) | clickButton (x,y) (0,25) 200 50 = (p,Game mapa jogador ticks avatares explosions False)
                                                                                                                | clickButton (x,y) (0,-50) 200 50 = (p,Menu 5 0)
                                                                                                                | clickButton (x,y) (0,0) 300 300 = (p,Game mapa jogador ticks avatares explosions True)
                                                                                                                | otherwise = (p,Game mapa jogador ticks avatares explosions False)
reageEvento (EventKey (MouseButton LeftButton) Up _ (x,y)) (p,Winner a b) = if clickButton (x,y) (0,-200) 200 50 then (p,Menu 5 0) else (p,Winner a b)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (p,Winner _ _) = (p,Menu 5 0)
reageEvento (EventKey (MouseButton LeftButton) Up _ _) (p,Rules) = (p,Menu 5 0)
reageEvento (EventKey (MouseButton LeftButton) Up _ _) (p,Credits) = (p,Menu 5 0)
reageEvento e s = s

{-|Função:
@ A função 'reageTempo' altera o estado do jogo quando o tempo avança n segundos@-}
reageTempo :: Float -> Estado -> Estado
reageTempo f (p,Game mapa jogador ticks avatares explosions False) | length playerList == 0 = (p,Winner avatares 4)
                                                                   | length playerList == 1 = (p,Winner avatares ((playerList)!!0))
                                                                   | otherwise = (p,Game (avanca moves ticks) jogador (ticks-1) avatares (explosionPoints,fireArea) False)
                                                                     where
                                                                       explosionPoints = [(x,y) | (x,y,_) <- list]
                                                                       fireArea = concat $ map (\(x,y,r)->areaOfExplosion (x,y) (radius mapa (x,y) r)) list
                                                                       areaOfExplosion (x,y) (r,l,d,u) | r > 0 = (x + r,y):areaOfExplosion (x,y) (r-1,l,d,u)
                                                                                                       | l > 0 = (x - l,y):areaOfExplosion (x,y) (r,l-1,d,u)
                                                                                                       | d > 0 = (x,y + d):areaOfExplosion (x,y) (r,l,d-1,u)
                                                                                                       | u > 0 = (x,y - u):areaOfExplosion (x,y) (r,l,d,u-1)
                                                                                                       | otherwise = []
                                                                       (list,_) = listOfExplosion $ reduceTimeOfBombs mapa
                                                                       botDir nPlayer = if elem nPlayer playerList then (movePlayer mapa nPlayer ticks (nPlayer^2+1),nPlayer) else (Nothing,nPlayer)
                                                                       botMove (Just d,nPlayer) m = move m nPlayer d
                                                                       botMove (Nothing,_) m = m
                                                                       moves = botMove (botDir 1) $ botMove (botDir 2) $ botMove (botDir 3) $ mapa
                                                                       playerList = map (\(h:_) -> digitToInt h) $ filter (\(h:_)->isDigit h) mapa
reageTempo f s = s

{-|===Função:
@A função 'fr' é o valor da frame rate@-}
fr :: Int
fr = 3

{-|===Função:
@A função 'dm' representa o display mode@-}
dm :: Display
dm = InWindow "Bomberman - Daniel Costa | Rui Azevedo" (800, 600) (0, 0)

{-|===Função:
@A função 'joga' é a função principal que invoca o jogo@-}
joga :: Estado -> IO ()
joga e = play dm              -- display mode
                  (makeColorI 131 191 79 0)-- côr do fundo da janela
                  fr              -- frame rate
                  e  -- estado inicial
                  desenhaEstado   -- desenha o estado do jogo
                  reageEvento     -- reage a um evento
                  reageTempo      -- reage ao passar do tempo

main :: IO ()
main = do e <- estadoInicial
          joga e

-- Tarefa 1
{-|===Função:
@A função 'mapa' devolve uma lista de Strings que representam o mapa completo do jogo com os power-ups@

===Variáveis:
@ __:: Int:__ Tamanho do mapa
__ -> Int:__ Semente@

===Observação:
@No caso em que o mapa tem o tamanho 5x5 tivemos que colocar exatamente o mapa pois estava a dar erro no SVN@
 -}
mapa :: Int -> Int -> [String]
mapa 5 seed = ["#####","#   #","# # #","#   #","#####"]
mapa size seed = (mapAllTogether size seed (size-1)) ++ (bombDetector size seed (size-1)) ++ (flameDetector size seed (size-1))

{-|===Função:
@A função 'mapAllTogether' devolve uma lista de Strings que representam o mapa completo do jogo@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente
__-> Int:__ Linha do mapa@ -}
mapAllTogether :: Int -> Int -> Int -> [String]
mapAllTogether size seed 0 = [randomMap size seed 0 True]
mapAllTogether size seed l = mapAllTogether size seed (l-1) ++ [randomMap size seed l True]

{-|===Função:
@A função randomList devolve uma lista de Int's com números geridos aleatoriamente de 0 a 99 na qual a seed vai ser o número pelo qual esses
números vão ser geridos@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente@ -}
randomList :: Int -> Int -> [Int]
randomList size seed = take (numOpenSpace size) $ randomRs (0,99) (mkStdGen seed)

{-|===Função:
@A função 'tradutor' vai receber uma lista de números aleatórios e vai transformar os números aleatórios em tijolos, bombas, flames ou espaços vazios.
__Bool == True:__ A função trabalha com os tijolos dentro do mapa
__Bool == False:__ A função trabalha com as bombas e os flames@

===Variáveis:
@__::[Int]:__ Lista de Int's
__-> Bool:__ Vai servir de switch@

===Observação:
@__(?):__ Tijolos
__(+):__ Power-up Bombas
__(!):__ Power-Up flames
__(' '):__ Espaços vazios@ -}
tradutor :: [Int] -> Bool -> String
tradutor [] _ = []
tradutor (h:t) n | h < 40 && n = '?' : tradutor t n
                 | h < 2 = '+' : tradutor t n
                 | h < 4 = '!' : tradutor t n
                 | otherwise = ' ' : tradutor t n

{-|===Função:
@A função 'randomMap' devolve uma String que representa o mapa do jogo@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente
__-> Int:__ Linha do mapa
__-> Bool:__ Bool para ser usado na função auxiliar 'tradutor'@

===Observação:
@Na função 'randomMap' vamos usar a função auxiliar 'fillOpenSpace' para preencher os espaços vazios disponíveis com os números aleatórios gerados@ -}
randomMap :: Int -> Int -> Int -> Bool -> String
randomMap size seed l b | l == size = []
                        | l == 0 || l == (size - 1) = replicate size '#'
                        | l == 1 || l == (size - 2) = "#  " ++ fillOpenSpace v l size ++ "  #"
                        | l == 2 || l == (size - 3) = "# #" ++ intersperse '#' (fillOpenSpace v l size) ++ "# #"
                        | odd l = "#" ++ fillOpenSpace v l size ++ "#"
                        | even l = "#" ++ intersperse '#' (fillOpenSpace v l size) ++ "#"
                        where v = tradutor (randomList size seed) b

{-|===Função:
@A função 'bombDetector' vai percorrer o mapa e vai detetar onde existem bombas. De seguida, vai colocar abaixo do mapa, a posição das bombas@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente
__-> Int:__ Linha do mapa@ -}
bombDetector :: Int -> Int -> Int -> [String]
bombDetector size seed 0 = []
bombDetector size seed l = bombDetector size seed (l-1) ++ map (\h -> "+ " ++ show h ++ " " ++ show l) (elemIndices '+' $ randomMap size seed l False)
                         where v = tradutor (randomList size seed)

{-|===Função:
@A função 'flameDetector' vai percorrer o mapa e vai detetar onde existem flames. De seguida, vai colocar abaixo do mapa, as posições das flames@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente
__-> Int:__ Linha do mapa@ -}
flameDetector :: Int -> Int -> Int -> [String]
flameDetector size seed 0 = []
flameDetector size seed l = flameDetector size seed (l-1) ++ map (\h -> "! " ++ show h ++ " " ++ show l) (elemIndices '!' $ randomMap size seed l False)
                         where v = tradutor (randomList size seed)

{-|===Função:
@A função 'numOpenSpaceLine' vai-nos devolver um Int que representa o número de espaços vazios de uma linha sabendo que há espaços vazios já predefinidos@

===Variáveis:
@__:: Int:__ Linha do mapa
__-> Int:__ Tamanho do mapa@ -}
numOpenSpaceLine :: Int -> Int -> Int
numOpenSpaceLine _ 5 = 0
numOpenSpaceLine l size | l == 0 || l == (size - 1) = 0
                        | l == 1 || l == (size - 2) = size - 6
                        | l == 2 || l == (size - 3) = size - 4 - ((div size 2) - 1)
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
                | l == 2 || l == (size - 1) = numOpenSpaceLine 1 size + gathered (l-1) size
                | l == 3 || l == (size - 2) = numOpenSpaceLine 2 size + gathered (l-1) size
                | odd l = numOpenSpaceLine (l-1) size + gathered (l-1) size
                | even l = numOpenSpaceLine (l-1) size + gathered (l-1) size

{-|===Função:
@ A função 'fillOpenSpace' preenche os espaços vazios com os números aleatórios da função 'tradutor'. A função 'fillOpenSpace', através da função 'numOpenSpaceLine',
vai contar quantos espaços vazios tem o mapa disponíveis para preencher com números aleatórios. A função take vai tirar os elementos necessários
para preencher corretamente a lista e a função drop vai dropar o número de elementos já saídos@

===Variáveis:
@__:: String:__ Lista dos elementos aleatórios do mapa
__-> Int:__ Número da linha do mapa
__-> Int (size):__ Tamanho do mapa@

===Observação:
@Na função 'fillOpenSpace' vamos usar as funções auxiliares 'numOpenSpaceLine' ,que vai devolver-nos um Int que vai ser o número de espaços vazios
que tem uma linha do mapa, e a função 'gathered' que devolve um Int que vai ser o número de elementos necessários dropar, visto que o mapa tem que ser
preenchido aleatoriamente com os números da 'randomList' e todos os números dessa lista devem ser usados@

===Exemplo:
@Num mapa 7x7, o número de espaços disponíveis para colocar números da 'randomList' são 9@

@Seja l a lista de Int's formada pela função 'randomList'. Então, neste caso, o que queremos fazer é o seguinte:@

@take 1 (drop 0 l)
take 1 (drop 1 l)
take 5 (drop 2 l)
take 1 (drop 7 l)
take 1 (drop 8 l)@

@Desta maneira garantimos que todos os número da função 'randomList' vão ser usados e nenhum vai ser repetido@ -}
fillOpenSpace :: String -> Int -> Int -> String
fillOpenSpace l 0 size = ""
fillOpenSpace l n size = take (numOpenSpaceLine n size) $ drop (gathered n size ) l

{-|===Função:
@ A função 'numOpenSpace' devolve um Intque corresponde ao número de espaços vazios que tem o mapa, no total@

===Variáveis:
@__::Int:__ Tamanho do mapa@ -}
numOpenSpace :: Int -> Int
numOpenSpace 5 = 0
numOpenSpace n  = (n^2) - (n * 4 - 4) - 12 - ((div n 2) - 1)^2

-- Tarefa 2

{-|===Função:
@A função 'move' verifica se o jogador está a jogar. Em caso afirmativo, retorna o novo estado de jogo, caso contrário, retorna o mapa como estava@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> Char:__ Ação do jogador@ -}
move :: [String] -> Int -> Char -> [String]
move [] p d = []
move l p d | elem (intToDigit p) h = playM l p d
           | otherwise = l
           where h = map head l

{-|===Função:
@A função 'play' imprime o mapa com o novo estado do jogo caso a ação seja possivel, caso contrário, retorna o mesmo mapa@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> Int:__ Número do jogador
__-> Char:__ Ação do jogador@ -}
playM :: [String] -> Int -> Char -> [String]
playM [] _ _ = []
playM l p d | d == 'U' = if canMove l (x,y-1) then removeLine (drawAtPlayer l l p (x,y-1) powers) (powerUpLine l (x,y-1)) else l
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

-- Tarefa 4
avanca :: [String] -> Int -> [String]
avanca m t = drawSpiral (nextT (listOfExplosion $ reduceTimeOfBombs m)) t

{-|===Função:
@A função 'mapSize' devolve um Int que representa o tamanho do mapa@

===Variáveis:
@__::[String]:__ Mapa do jogo@ -}
mapSize :: [String] -> Int
mapSize [] = 0
mapSize (h:t) = length h

{-|===Função:
@A função 'radiusDir'@

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__
__-> (Int,Int):__
__-> Int:__
__-> Int:__
-}
radiusDir :: [String] -> (Int,Int) -> (Int,Int) -> Int -> Int -> Int
radiusDir m (x,y) (dx,dy) i r | i == r = 0
                              | value == ' ' = 1 + radiusDir m (x,y) (dx,dy) (i+1) r
                              | value == '#' = 0
                              | elem (x,y) (listOfPowerUps m) = 1
                              | otherwise = 1
                              where value = (m!!(y + dy * i))!!(x + dx * i)

{-|===Função:
@A função 'radius'

===Variáveis:
@__:: [String]:__ Mapa do jogo
__-> (Int,Int):__
__-> Int:__-}
radius :: [String] -> (Int,Int) -> Int -> (Int,Int,Int,Int)
radius m (x,y) r = (rad right,rad left,rad down,rad up)
                  where rad d = radiusDir m (x,y) d 1 (r+1)
                        right = (1,0)
                        left = (-1,0)
                        down = (0,1)
                        up = (0,-1)
{-|===Função:
@A função 'reduceTimeBombs' devolve

===Variáveis:
@__:: [String]:__ Mapa do jogo@-}
reduceTimeOfBombs :: [String] -> [String]
reduceTimeOfBombs [] = []
reduceTimeOfBombs ((x:xs):y) = if x == '*' then (unwords $ newBombLine):reduceTimeOfBombs y
                                           else (x:xs):reduceTimeOfBombs y
                                where bombLine = words xs
                                      newBombLine = "*":(init bombLine) ++ [show (read (last bombLine) - 1)]

{-|===Função:
@A função 'drawAtPoint' devolve

===Variáveis:
@__:: [String]:__
__-> (Int,Int):__
__-> Char:__-}
drawAtPoint :: [[a]] -> (Int,Int) -> a -> [[a]]
drawAtPoint [] _ _ = []
drawAtPoint (l:ls) (x,0) c = (take x l ++ [c] ++ drop (x+1) l):ls
drawAtPoint (l:ls) (x,y) c = l:drawAtPoint ls (x,y-1) c

explosion :: (Int,Int) -> (Int,Int,Int,Int) -> [String] -> [String]
explosion (x,y) (0,0,0,0) m = explodeBombs (x,y) $ destroyBricks (x,y) $ killPlayers (x,y) 3 m
explosion (x,y) (r,l,d,u) m | r > 0 = explode (x + r,y) (r-1,l,d,u)
                            | l > 0 = explode (x - l,y) (r,l-1,d,u)
                            | d > 0 = explode (x,y + d) (r,l,d-1,u)
                            | u > 0 = explode (x,y - u) (r,l,d,u-1)
                            where explode a b = explodeBombs a $ destroyBricks a $ killPlayers a 3 (explosion (x,y) b m)

getPositions :: Char -> [String] -> [(Int,Int)]
getPositions c [] = []
getPositions c ((x:xs):y) | x == c = (read ((words xs) !! 0),read ((words xs) !! 1)):getPositions x y
                          | otherwise = getPositions c y

killPlayers :: (Int,Int) -> Int -> [String] -> [String]
killPlayers _ (-1) m = m
killPlayers (x,y) p m = if [(x,y)] == getPositions (intToDigit p) m then killPlayers (x,y) (p-1) (removeLineOfPlayer p m)
                                                                    else killPlayers (x,y) (p-1) m


removeLineOfPlayer :: Int -> [String] -> [String]
removeLineOfPlayer p ((x:xs):y) = if show p == [x] then y else (x:xs):removeLineOfPlayer p y

destroyBricks :: (Int,Int) -> [String] -> [String]
destroyBricks (x,y) m = if (m!!y)!!x == '?' then drawAtPoint m (x,y) ' ' else destroyPowerUps (x,y) m

destroyPowerUps :: (Int,Int) -> [String] -> [String]
destroyPowerUps _ [] = []
destroyPowerUps (x,y) ((h:hs):t) = if b then t else (h:hs):destroyPowerUps (x,y) t
                                  where a = words hs
                                        b = if h == '+' || h == '!' then a!!0 == show x && a!!1 == show y else False

destroyBombs :: (Int,Int) -> [String] -> [String]
destroyBombs _ [] = []
destroyBombs (x,y) ((h:hs):t) = if b then t else (h:hs):destroyPowerUps (x,y) t
                                where a = words hs
                                      b = if h == '*' then a!!0 == show x && a!!1 == show y else False

explodeBombs :: (Int,Int) -> [String] -> [String]
explodeBombs _ [] = []
explodeBombs (x,y) ((h:hs):t) = if b then unwords (["*"] ++ init a ++ ["1"]):explodeBombs (x,y) t else (h:hs):explodeBombs (x,y) t
                                where a = words hs
                                      b = if h == '*' then a!!0 == show x && a!!1 == show y else False

listOfExplosion :: [String] -> ([(Int,Int,Int)],[String])
listOfExplosion [] = ([],[])
listOfExplosion ((h:hs):t) = if b then ((read (a!!0),read (a!!1),read (a!!3)):c,d) else (c,(h:hs):d)
                                  where a = words hs
                                        b = if h == '*' then last a == "0" else False
                                        (c,d) = listOfExplosion t

listOfPowerUps :: [String] -> [(Int,Int)]
listOfPowerUps [] = []
listOfPowerUps ((h:hs):t) = if h == '+' || h == '!' then (read (a!!0),read (a!!1)):listOfPowerUps t else listOfPowerUps t
                            where a = words hs

startSpiralMap :: Int -> Int -> [[Bool]]
startSpiralMap 0 _ = []
startSpiralMap n s = if n == s || n == 1 then (replicate s False):startSpiralMap (n-1) s
                                         else (False:(replicate (s-2) True) ++ [False]):startSpiralMap (n-1) s

spiral :: (Int,Int) -> [[Bool]] -> Int -> Char -> (Int,Int)
spiral (x,y) m n d | n == (size-2)^2 = (x,y)
                   | d == 'R' = if empty (x+1,y) then spiral (x+1,y) s (n+1) d else spiral (x,y) m n 'D'
                   | d == 'D' = if empty (x,y+1) then spiral (x,y+1) s (n+1) d else spiral (x,y) m n 'L'
                   | d == 'L' = if empty (x-1,y) then spiral (x-1,y) s (n+1) d else spiral (x,y) m n 'U'
                   | d == 'U' = if empty (x,y-1) then spiral (x,y-1) s (n+1) d else spiral (x,y) m n 'R'
                    where empty (x,y) = (m!!y)!!x
                          s = drawAtPoint m (x,y) False
                          size = length $ head m

drawSpiral :: [String] -> Int -> [String]
drawSpiral m t = if t > (size-2)^2 then m
                                   else destroyBombs p $ destroyPowerUps p $ killPlayers p 3 (drawAtPoint m p '#')
                 where p = spiral (1,1) (startSpiralMap size size) t 'R'
                       size = length $ head m

nextT :: ([(Int,Int,Int)],[String]) -> [String]
nextT ([],m) = m
nextT (((x,y,t):z),m) = nextT (z, (explosion (x,y) (radius m (x,y) t) m))
