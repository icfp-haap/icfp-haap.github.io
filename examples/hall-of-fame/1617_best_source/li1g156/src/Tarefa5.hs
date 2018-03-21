{-|
Module : Main
Description : Implementação do jogo em Gloss
Copyright : Pedro Pinto <a80741@alunos.uminho.pt>;
            Pedro Lima <a80328@alunos.uminho.pt>
O objetivo desta tarefa é a implementação propriamente funcional do jogo.
Embora nas outras tarefas já tenham sido desenvolvidas soluções par aa reação a comandos e passagem do tempo, continua a ser necessária a utilização de uma plataforma de desenvolvimento de uma interface gráfica, reproduzida a partir de cada estado do jogo.
Compilada esta tarefa, deve resultar o produto final do projeto.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import Tarefa1
import Tarefa2
import TarefaSqrt16
import Tarefa6_li1g156
import System.Environment
import System.Random
import Data.Maybe



type Brick   = (Int,Int)
-- ^ (abcissa, ordenada)
type Player  = (Int,Int,Int,Int,Int)
-- ^ (numero,abcissa,ordenada,bombs,flames)
type Powerup = (Char,Int,Int)
-- ^ (powerup, abcissa, ordenada)
type Bomb    = (Int,Int,Int,Int,Int)
-- ^ (abcissa, ordenada, player, range, timer)
type Estado  = (Mapa,[Brick],[Brick],[Powerup],[Bomb],[Player],Float, Screen)
-- ^ (lista de strings, pedras, tijolos, powerups, bombas, players, tempo, informação sobre fase do programa)
type Seed    = Int
-- ^ Seed geradora do mapa.
type Tempo   = Int
-- ^ Instantes que faltam para o jogo acabar
type Jogador = Int
-- ^ Número representativo de um jogador.
type Screen  = String
-- ^ String representativa do ecrã em que o programa do jogo se encontra.


--type Dim     = Int                        <- já definido na Tarefa 6 -> Dimensão do mapa
--type Abcissa  = Int                       <- já definido na Tarefa 4 -> Abcissa de um espaço no mapa
--type Ordenada = Int                       <- já definido na Tarefa 4 -> Ordenada de um espaço no mapa.
--type Mapa     = [String]                  <- já definido na Tarefa 6 -> Estado do jogo representado em lista de strings
--type Linha    = String                    <- já definido na Tarefa 6 -> Uma linha da representação doe stado do jogo no tipo @Mapa@




-- | O estado inicial do jogo.
estadoInicial :: Seed -> Estado
estadoInicial s = ([],[],[],[],[],[],0,"d11"++(show s))




-- | O estado inicial do jogo, depois de escolhidos a dimensão e o numero de players.
estadoInicial2 ::  Dim -> Seed -> Int -> Estado
estadoInicial2 d s n     = (             map,
                          retiraRocks  0 map,
                         retiraBricks  0 map,
                              retiraPups map,
                             retiraBombs map,
                             geraPlayers map,
                        fromIntegral (2*d^2),
                                      show s    )
           where map = inserePlayers (mapa d s) n d


-- | Frame rate
fr :: Int
fr = 50

-- | Display mode
dm :: Display
dm = InWindow "Bomberman" (800, 600) (0, 0)


-- | Função principal que invoca o jogo.
main :: IO ()
main = do  s   <- randomRIO (0,99)
           p0  <- loadBMP "mario_img/rocks.bmp"
           p1  <- loadBMP "mario_img/brick.bmp"
           p2  <- loadBMP "mario_img/powerup_bombs.bmp"
           p3  <- loadBMP "mario_img/powerup_flames.bmp"
           p4  <- loadBMP "mario_img/bomb1.bmp"
           p5  <- loadBMP "mario_img/pl/0.bmp"
           p6  <- loadBMP "mario_img/pl/1.bmp"
           p7  <- loadBMP "mario_img/pl/2.bmp"
           p8  <- loadBMP "mario_img/pl/3.bmp"
           sb4 <- loadBMP "mario_img/sb/sb.bmp"
           sb0 <- loadBMP "mario_img/sb/0_sb.bmp"
           sb1 <- loadBMP "mario_img/sb/1_sb.bmp"
           sb2 <- loadBMP "mario_img/sb/2_sb.bmp"
           sb3 <- loadBMP "mario_img/sb/3_sb.bmp"
           fl0 <- loadBMP "mario_img/explosion/center1.bmp"
           fl1 <- loadBMP "mario_img/explosion/center2.bmp"
           fl2 <- loadBMP "mario_img/explosion/center3.bmp"
           fl3 <- loadBMP "mario_img/explosion/mid1.bmp"
           fl4 <- loadBMP "mario_img/explosion/mid2.bmp"
           fl5 <- loadBMP "mario_img/explosion/mid3.bmp"
           fl6 <- loadBMP "mario_img/explosion/tip1.bmp"
           fl7 <- loadBMP "mario_img/explosion/tip2.bmp"
           fl8 <- loadBMP "mario_img/explosion/tip3.bmp"
           m0  <- loadBMP "mario_img/menu/gameover.bmp"
           m1  <- loadBMP "mario_img/menu/dim.bmp"
           m2  <- loadBMP "mario_img/menu/players.bmp"
           let l  = [p0,p1,p2,p3,p4,p5,p6,p7,p8]
           let l2 = [sb0,sb1,sb2,sb3,sb4]
           let l3 = [fl0,fl1,fl2,fl3,fl4,fl5,fl6,fl7,fl8]
           let m  = [m0,m1,m2]
           play dm                               -- display mode
                (makeColorI 119 145 255 255)     -- côr do fundo da janela
                fr                               -- frame rate
                (estadoInicial s)                -- estado inicial
                (desenhaEstado l l2 l3 m)        -- desenha o estado do jogo
                reageEvento                      -- reage a um evento
                reageTempo                       -- reage ao passar do tempo





















--------------------------------------------------------REAÇÃO A COMANDOS----------------------------------------------------------------------------------------------------------------------------------
                                                                                                                                                                                                        ---
                                                                                                                                                                                                        ---
                                                                                                                                                                                                        ---
-- | Função que altera o estado do jogo quando acontece um evento.                                                                                                                                      ---
reageEvento :: Event -> Estado -> Estado
reageEvento x (a,b,c,d,e,f,g,h) = if head h == 'd' then reageDim     x (a,b,c,d,e,f,g,h)
                          else    if head h == 'p' then reagePls     x (a,b,c,d,e,f,g,h)
                          else    if      a == []  then reageOver    x (a,b,c,d,e,f,g,h)
                          else                          reageEvento1 x (a,b,c,d,e,f,g,h)

-----------------------------------------------------NO MENU DE SELEÇÃO DE DIMENSÃO-------------------------------------

reageDim :: Event -> Estado -> Estado
-- ^Função que faz mover o círculo mediante o input do jogador no men de seleção da dimensão.
reageDim (EventKey (SpecialKey KeyUp                             ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if z == '2' then (a,b,c,d,e,f,g,(x:y:'1':w))      else (a,b,c,d,e,f,g,(x:y:z:w))
reageDim (EventKey (SpecialKey KeyDown                           ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if z == '1' then (a,b,c,d,e,f,g,(x:y:'2':w))      else (a,b,c,d,e,f,g,(x:y:z:w))
reageDim (EventKey (SpecialKey KeyLeft                           ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if y >  '1' then (a,b,c,d,e,f,g,(x:(pred y):z:w)) else (a,b,c,d,e,f,g,(x:y:z:w))
reageDim (EventKey (SpecialKey KeyRight                          ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if y <  '4' then (a,b,c,d,e,f,g,(x:(succ y):z:w)) else (a,b,c,d,e,f,g,(x:y:z:w))
reageDim (EventKey (SpecialKey KeyEnter                          ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = enterDim (a,b,c,d,e,f,g,(x:y:z:w))
reageDim                             _                                                                s =                               s



enterDim :: Estado -> Estado
-- ^ Função que seleciona a dimensão pretendida pelo jogador.
enterDim (a,b,c,d,e,f,g,(x:y:z:w)) | y == '1'                    = if z == '1' then (a,b,c,d,e,f,g,"p21/9/" ++w)         else (a,b,c,d,e,f,g,"p21/23/"++w)
                                   | y == '2'                    = if z == '1' then (a,b,c,d,e,f,g,"p21/13/"++w)         else (a,b,c,d,e,f,g,"p21/25/"++w)
                                   | y == '3'                    = if z == '1' then (a,b,c,d,e,f,g,"p21/15/"++w)         else (a,b,c,d,e,f,g,"p21/27/"++w)
                                   | y == '4'                    = if z == '1' then (a,b,c,d,e,f,g,"p21/19/"++w)         else (a,b,c,d,e,f,g,"p21/29/"++w)



-----------------------------------------------------NO MENU DE SELEÇÃO DO NUMERO DE PLAYERS-------------------------------------




reagePls :: Event -> Estado -> Estado
-- ^Função que faz mover o círculo mediante o input do jogador no menu de seleção do numero de players.
reagePls (EventKey (SpecialKey KeyLeft                           ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if y >  '2' then (a,b,c,d,e,f,g,(x:(pred y):z:w)) else (a,b,c,d,e,f,g,(x:y:z:w))
reagePls (EventKey (SpecialKey KeyRight                          ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = if y <  '4' then (a,b,c,d,e,f,g,(x:(succ y):z:w)) else (a,b,c,d,e,f,g,(x:y:z:w))
reagePls (EventKey (SpecialKey KeyEnter                          ) Down _ _) (a,b,c,d,e,f,g,(x:y:z:w))  = enterPls (a,b,c,d,e,f,g,(x:y:z:w))
reagePls                             _                                                                s =                               s




enterPls :: Estado -> Estado
-- ^ Função que seleciona o numero de jogadores pretendido pelo jogador.
enterPls (a,b,c,d,e,f,g,(x:y:z:w)) | y == '1'                  = estadoInicial2 d s 1
                                   | y == '2'                  = estadoInicial2 d s 2
                                   | y == '3'                  = estadoInicial2 d s 3
                                   | y == '4'                  = estadoInicial2 d s 4
                                where d = read           $ takeWhile (isDigit) (drop 1  w)
                                      s = read $ reverse $ takeWhile (isDigit) (reverse w)

-------------------------------------------------------NO JOGO PROPRIAMENTE DITO-------------------------------------------------



reageEvento1 :: Event -> Estado -> Estado
-- ^ Função que controla os movimentos dos jogadores.
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'w') Down _ _) s = geraEstado (move (takeLString s) 0 'U') (takeTime s) (takeMen s)
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'a') Down _ _) s = geraEstado (move (takeLString s) 0 'L') (takeTime s) (takeMen s)
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 's') Down _ _) s = geraEstado (move (takeLString s) 0 'D') (takeTime s) (takeMen s)
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'd') Down _ _) s = geraEstado (move (takeLString s) 0 'R') (takeTime s) (takeMen s)
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'c') Down _ _) s = geraEstado (move (takeLString s) 0 'B') (takeTime s) (takeMen s)
reageEvento1 (EventKey (Graphics.Gloss.Interface.Pure.Game.Char 'm') Down _ _) s = geraEstado (move (takeLString s) 1 'B') (takeTime s) (takeMen s)
reageEvento1 (EventKey (SpecialKey KeyUp                           ) Down _ _) s = geraEstado (move (takeLString s) 1 'U') (takeTime s) (takeMen s)
reageEvento1 (EventKey (SpecialKey KeyDown                         ) Down _ _) s = geraEstado (move (takeLString s) 1 'D') (takeTime s) (takeMen s)
reageEvento1 (EventKey (SpecialKey KeyLeft                         ) Down _ _) s = geraEstado (move (takeLString s) 1 'L') (takeTime s) (takeMen s)
reageEvento1 (EventKey (SpecialKey KeyRight                        ) Down _ _) s = geraEstado (move (takeLString s) 1 'R') (takeTime s) (takeMen s)
reageEvento1  _                                                                s =                               s






-----------------------------------------------------------NO FINAL DO JOGO------------------------------------------------------


reageOver :: Event -> Estado -> Estado
-- ^ Função que faz com que, premida alguma tecla após o jogo acabar, o jogo volte ao início.
reageOver (EventKey _ Down _ _)                             ([],b,c,d,e,f,_,h)  =      estadoInicial (newseed (read h))
reageOver _                                                                 s   = s



newseed :: Seed -> Seed
-- ^ Função que gera uma nova seed a partir da anterior.
newseed n = head (take 2 (randomRs (0,99) (mkStdGen n)))
                                                                                                                                                                                                            ---
                                                                                                                                                                                                            ---
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


































---------------------------------------------------------PASSAGEM DO TEMPO------------------------------------------------------------------------------------------------
                                                                                                                                                                       ---
                                                                                                                                                                       ---                                                                                                                                                          


-- | Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo t (a,b,c,d,e,f,g,h) | elem (head h) "dp"           = (a,b,c,d,e,f,g,h)
                               | a == []                      = ([],b,c,d,e,f,0,h)
                               |  deci t2 <= 0.08     = geraEstado (fst (avanca (botTempo a (truncate (g - 0.08))) (truncate (g - 0.08)))) (g - 0.08) h
                               | f == [] || length f == 1     = ([],b,c,d,e,f,g,h)
                               | g <= 0.16                    = ([],b,c,d,e,f,g,h)
                               |otherwise                     =  (a,b,c,d,e,f,g - 0.08,h)
                              where t2 = 2*x^2 - g
                                    x = if a /= [] then  fromIntegral (length (a!!0)) else 1
                                    n = truncate g




                                                                                                                                                                       ---
                                                                                                                                                                       ---
botTempo :: Mapa -> Tempo -> Mapa
-- ^ Função que recebe o estado e devolve-o com as alterações de acordo com o input dos bots.
botTempo mapa n | b1 /= Nothing && b2 /= Nothing            = move (move mapa 2 (fromJust b1)) 3 (fromJust b2)
                | b1 /= Nothing                                 = move mapa 2 (fromJust b1)
                | b2 /= Nothing                                 = move mapa 3 (fromJust b2)
                | otherwise                                     = mapa
               where b1 = bot mapa 2 n
                     b2 = bot mapa 3 n



                                                                                                                                                                       ---
                                                                                                                                                                       ---
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------








--------------------------------------------------------------------COMPONENTE GRÁFICA -----------------------------------------------------------------------------------
                                                                                                                                                                       ---
                                                                                                                                                                       ---
                                                                                                                                                                       ---
-- | Função que desenha o jogo, levando como argumento as listas de imagens relativas a diferentes componentes do jogo.
desenhaEstado :: [Picture] ->  [Picture] -> [Picture] -> [Picture] -> Estado -> Picture
desenhaEstado l l2 l3 m s  = if head (takeMen s) == 'd'                 then drawDim  m s
             else            if head (takeMen s) == 'p'                 then drawPls  m s
             else            if   takeLString s  == []                  then gameover s m l2
             else
                           Pictures  [                                    pedras (l!!0) d s ,
                                                                         tijolos (l!!1) d s ,
                                       Pictures (powersImg (l!!2) (l!!3) d (takePowers s) s),
                                                                       bombasImg (l!!4) d s ,
                                               playersImg [(l!!5),(l!!6),(l!!7),(l!!8)] d s ,
                                                                                scoreB l2 s ,
                                                                      Pictures   (burn l3 s),
                                                                                    tempo s                        ]
              where d = if  takeLString s /= [] then  length (head (takeLString s)) else 1





                                                                                                                                                                       ---
                                                                                                                                                                       ---
----------------------------------------------------------------- PEDRAS --------------------------------------------------------------------------




pedras :: Picture -> Dim -> Estado -> Picture
-- ^ Esta função gera uma imagem com todas as pedras contidas no mapa.
pedras p0 d s = Pictures [placeMe d x y (resized p0 d)  | (x,y) <- takeRocks s]



                                                                                                                                                                       ---
                                                                                                                                                                       ---
----------------------------------------------------------------- TIJOLOS --------------------------------------------------------------------------




tijolos :: Picture -> Dim -> Estado -> Picture
-- ^ Esta função gera uma imagem com todos os tijolos contidos no mapa.
tijolos p1 d s = Pictures [placeMe d x y (resized p1 d)  | (x,y) <- takeBricks s]


                                                                                                                                                                       ---
                                                                                                                                                                       ---
-----------------------------------------------------------------POWERUPS----------------------------------------------------------------------------



powersImg :: Picture -> Picture -> Dim -> [Powerup] -> Estado -> [Picture]
-- ^ Esta função gera uma imagem com todos os powerups contidos no mapa.
powersImg _ _ _ [] _ = []
powersImg p2 p3 d (h:t) s
          | ((takeLString s) !! (takeY h)) !! takeX h == ' '    = if takeChar h == '+' then (placeMe d (takeX h) (takeY h) (resized p2 d)) : (powersImg p2 p3 d t s)
                                                                                       else (placeMe d (takeX h) (takeY h) (resized p3 d)) : (powersImg p2 p3 d t s)
          | otherwise                                           =                                                                            (powersImg p2 p3 d t s)




                                                                                                                                                                       ---
                                                                                                                                                                       ---
-------------------------------------------------------------------BOMBAS---------------------------------------------------------------------------



bombasImg :: Picture -> Dim -> Estado -> Picture
-- ^ Esta função gera uma imagem com todas as bombas contidas no mapa.
bombasImg p4 d s = Pictures [placeMe d x y (resized p4 d)   | (x,y,_,_,_) <- takeBombs1 s]


                                                                                                                                                                       ---
                                                                                                                                                                       ---

-----------------------------------------------------------------JOGADORES--------------------------------------------------------------------------



playersImg :: [Picture] -> Dim -> Estado -> Picture
-- ^ Esta função gera uma imagem com todos os jogadores contidos no mapa.
playersImg  [p5,p6,p7,p8] d s =   Pictures          [if c == 0   then placeMe d x y (resized p5  d)
                                                else if c == 1   then placeMe d x y (resized p6  d)
                                                else if c == 2   then placeMe d x y (resized p7  d)
                                                else                  placeMe d x y (resized p8  d)             | (c,x,y,_,_) <- takePlayers1 s]


                                                                                                                                                                       ---
                                                                                                                                                                       ---
----------------------------------------------------------SCOREBOARD---------------------------------------------------------------------------------


scoreB :: [Picture] -> Estado -> Picture
scoreB l2 s = Pictures [Translate 300 0 (l2!!4), Pictures (sbPlayers l2 (takePlayers1 s))]

sbPlayers :: [Picture] -> [Player] -> [Picture]
sbPlayers _ [] = []
sbPlayers l2 ((a,_,_,_,_):t)  | a == 0           =  [Translate 300 120    (l2!!0)] ++ (sbPlayers l2 t)
                              | a == 1           =  [Translate 300 20     (l2!!1)] ++ (sbPlayers l2 t)
                              | a == 2           =  [Translate 300 (-80)  (l2!!2)] ++ (sbPlayers l2 t)
                              | a == 3           =  [Translate 300 (-180) (l2!!3)] ++ (sbPlayers l2 t)
                              |otherwise         =                                    (sbPlayers l2 t)


                                                                                                                                                                       ---
                                                                                                                                                                       ---
-----------------------------------------------------------EXPLOSÕES---------------------------------------------------------------------------------



burn :: [Picture] -> Estado -> [Picture]
-- ^  Função que coloca as imagens das chamas.
burn l3 (a,b,c,d,e,f,g,h) | deci g > 0.66    = burn2 [l3!!0,l3!!3,l3!!6] d (rmDoubles ( snd (avanca a (truncate (g - 1)))))
                          | deci g > 0.33    = burn2 [l3!!1,l3!!4,l3!!7] d (rmDoubles ( snd (avanca a (truncate (g - 1)))))
                          |otherwise         = burn2 [l3!!2,l3!!5,l3!!8] d (rmDoubles ( snd (avanca a (truncate (g - 1)))))
                        where d = length (head a)


burn2 :: [Picture] -> Int -> [Flames] -> [Picture]
-- ^ Função que coloca a imagem de cada espaço a arder.
burn2 _ _ [] = []
burn2 [a,b,c] d ((x,y,k):t) |k == 'U'         = (placeMe d x y (Rotate 270 (resized c d)) ): burn2 [a,b,c] d t
                            |k == 'u'         = (placeMe d x y (Rotate 270 (resized b d)) ): burn2 [a,b,c] d t
                            |k == 'D'         = (placeMe d x y (Rotate 90  (resized c d)) ): burn2 [a,b,c] d t
                            |k == 'd'         = (placeMe d x y (Rotate 90  (resized b d)) ): burn2 [a,b,c] d t
                            |k == 'L'         = (placeMe d x y (Rotate 180 (resized c d)) ): burn2 [a,b,c] d t
                            |k == 'l'         = (placeMe d x y (Rotate 180 (resized b d)) ): burn2 [a,b,c] d t
                            |k == 'R'         = (placeMe d x y             (resized c d)  ): burn2 [a,b,c] d t
                            |k == 'r'         = (placeMe d x y             (resized b d)  ): burn2 [a,b,c] d t
                            |k == 'c'         = (placeMe d x y             (resized a d)  ): burn2 [a,b,c] d t




                                                                                                                                                                       ---
                                                                                                                                                                       ---
-------------------------------------------------MENU DE SELEÇÃO DA DIMENSÃO-------------------------------------------------

drawDim :: [Picture] -> Estado -> Picture
-- ^ Função que desenha o menu de seleção da dimensão do mapa.
drawDim m (_,_,_,_,_,_,_,h) = Pictures [m!!1, redCircleD h]





redCircleD :: Screen -> Picture
-- ^ Função que desenha o círculo que ajuda a selecionar uma opção.
redCircleD (a:b:c:t) | b == '1'                 = if c == '1' then Translate 0   0 redCircle1 else Translate 0   (-40) redCircle1
                     | b == '2'                 = if c == '1' then Translate 70  0 redCircle1 else Translate 70  (-40) redCircle1
                     | b == '3'                 = if c == '1' then Translate 140 0 redCircle1 else Translate 140 (-40) redCircle1
                     | b == '4'                 = if c == '1' then Translate 210 0 redCircle1 else Translate 210 (-40) redCircle1


                                                                                                                                                                       ---
                                                                                                                                                                       ---
-------------------------------------------------MENU DE SELEÇÃO DO NUMERO DE PLAYERS-----------------------------------------



drawPls :: [Picture] -> Estado -> Picture
-- ^ Função que desenha o menu de seleção do nú mero de jogadores.
drawPls m (_,_,_,_,_,_,_,h) = Pictures [m!!2, redCircleP h]


redCircleP :: Screen -> Picture
-- ^ Função que desenha o círculo que ajuda a selecionar uma opção.
redCircleP (a:b:c:t) | b == '2'                 = Translate 30  (-40)  redCircle1
                     | b == '3'                 = Translate 100 (-40)  redCircle1
                     | b == '4'                 = Translate 170 (-40)  redCircle1


                                                                                                                                                                       ---
                                                                                                                                                                       ---

------------------------------------------------- FIM DO JOGO-------------------------------------------------------------------


gameover :: Estado -> [Picture] -> [Picture] ->  Picture
-- ^ Função que desenha na janela o vencedor no fim do jogo.
gameover (a,b,c,d,e,f,g,h) m pl = Pictures ([m!!0] ++  winnerPlayer f pl)




winnerPlayer :: [Player] -> [Picture] -> [Picture]
-- ^ Função que deteta o jogador que ganhou o jogo e imprime a sua imagem.
winnerPlayer [] _ = []
winnerPlayer ((a,b,c,d,e):t) pl  | a == 0                            = [Translate 70 0 (pl!!0)]
                                 | a == 1                            = [Translate 70 0 (pl!!1)]
                                 | a == 2                            = [Translate 70 0 (pl!!2)]
                                 | a == 3                            = [Translate 70 0 (pl!!3)]







                                                                                                                                                                       ---
                                                                                                                                                                       ---

----------------------------------------------------FUNCOES AUXILIARES À COMPONENTE GRÁFICA ----------------------------------------------

resized :: Picture -> Dim -> Picture
-- ^ Função que redimensiona uma imagem importada de acordo om uma dimensão do mapa.
resized p d = Scale (r d) (r d) p



r :: Dim -> Float
-- ^ Função que, a partir da dimensão do mapa, indica o rácio necessário para redimensionar as imagens importadas (que têm sempre 85px, por escolha nossa)
r d = (squareSide d) / 85



squareSide :: Dim -> Float
-- ^ Dimensão do lado de cada quadrado do mapa, em função da dimensão do mapa de jogo.
squareSide d = (600 / ((fromIntegral d) ))


placeMe :: Dim -> Abcissa -> Ordenada -> Picture -> Picture
-- ^ Função que a partir de umas coordenadas e de uma dimensão demapa coloca corretamente imagens na janela.
placeMe d x y p = Translate (-400 + (squareSide d)/2 + (fromIntegral x)*dim) (300 - (squareSide d)/2 - (fromIntegral y)*dim) p
               where dim = squareSide d



redCircle1 :: Picture
-- ^ Cículo utilizado nos menus de seleção.
redCircle1 = Translate (-85) (-41) (Color red (thickCircle 2 15))




tempo :: Estado -> Picture
-- ^ Função que mostra o visor como numero de instantes até o jogo terminar.
tempo (_,_,_,_,_,_,t,_) = Translate 280 215 (Scale 0.2 0.2 (Color white (Pictures [Text (show (truncate t))])))




                                                                                                                                                                       ---
                                                                                                                                                                       ---
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------



















---------------------------------------------------------------CONSTRUÇÃO DO ESTADO---------------------------------------------------------------------------------------
                                                                                                                                                                       ---
                                                                                                                                                                       ---
inserePlayers :: Mapa -> Int -> Dim -> [String]
-- ^ Função que insere num mapa gerado pela tarefa1 um número especificado de jogadores.
inserePlayers mapa n d | n == 1          = mapa ++ ["0 1 1"                                                                    ]
                       | n == 2          = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x                                         ]
                       | n == 3          = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x ,    "2 " ++ x ++ " 1"                  ]
                       | otherwise       = mapa ++ ["0 1 1",     "1 " ++ x ++ " " ++ x ,    "2 " ++ x ++ " 1" ,    "3 1 " ++ x ]
                       where x = show (d-2)


geraEstado :: Mapa -> Float -> Screen -> Estado
-- ^ Função que, a partir de uma lista de strings representativa de um mapa e do tempo, gera uma estrutura do tipo Estado, anteriormente definido.
geraEstado map t st = (                 map,
                         retiraRocks  0 map,
                        retiraBricks  0 map,
                             retiraPups map,
                            retiraBombs map,
                            geraPlayers map,
                                          t,
                                         st )



                                                                                                                                                                       ---
                                                                                                                                                                       ---
retiraRocks :: Ordenada -> Mapa -> [Brick]
-- ^ Função que retira do mapa as pedras em forma de túpulo.
retiraRocks _ []    = []
retiraRocks l (h:t) = (retiraRocksRow l 0 h) ++ retiraRocks (l+1) t



retiraRocksRow :: Ordenada -> Abcissa -> Linha -> [Brick]
-- ^ Função que retira de cada linha do mapa as pedras, em forma de túpulo.
retiraRocksRow _ _ []    = []
retiraRocksRow l c (h:t) = if    h == '#'         then (c,l):retiraRocksRow l (c+1) t
                                                  else       retiraRocksRow l (c+1) t






                                                                                                                                                                       ---
                                                                                                                                                                       ---

retiraBricks :: Ordenada -> Mapa -> [Brick]
-- ^ Função que retira do mapa os tijolos em forma de túpulo.
retiraBricks _ []    = []
retiraBricks l (h:t) = (retiraBricksRow l 0 h) ++ retiraBricks (l+1) t



retiraBricksRow :: Ordenada -> Abcissa -> Linha -> [Brick]
-- ^ Função que retira de cada linha do mapa os tijolos, em forma de túpulo.
retiraBricksRow _ _ []    = []
retiraBricksRow l c (h:t) = if    h == '?'        then (c,l):retiraBricksRow l (c+1) t
                                                  else       retiraBricksRow l (c+1) t



retiraBombs :: Mapa -> [Bomb]
-- ^ Função que retira do mapa as bombas colocadas.
retiraBombs [] = []
retiraBombs (h:t) = if h!!0 == '*' then (                                         pAbcis h,
                                                                                  pOrden h,
                    read (take 1 (drop (length (pAbcisSt h) + length (pOrdenSt h) + 4) h)),
                    read (take 1 (drop (length (pAbcisSt h) + length (pOrdenSt h) + 6) h)),
                            read (drop (length (pAbcisSt h) + length (pOrdenSt h) + 8) h)               ) : retiraBombs t
                                   else                                                                     retiraBombs t





                                                                                                                                                                       ---
                                                                                                                                                                       ---
retiraPups :: Mapa -> [Powerup]
-- ^ Função que a partir do estado do jogo obtém informação dos powerups.
retiraPups [] = []
retiraPups (h:t) = if elem (h!!0) "+!" then (tuplePower h):retiraPups t else retiraPups t

tuplePower :: Linha -> Powerup
-- ^ Função que converte num túpulo uma string com informação de dum dado powerup.
tuplePower string = (   head string,
                      pAbcis string,
                      pOrden string  )








                                                                                                                                                                       ---
                                                                                                                                                                       ---
geraPlayers :: Mapa -> [Player]
-- ^ Função que gera a partir de um estado do jogo (no formato da tarefa 2) uma lista de túpulos @Player@.
geraPlayers []    = []
geraPlayers (h:t) = if isDigit (h!!0)      then (tuplePlayer h) : geraPlayers t
                                           else                   geraPlayers t


tuplePlayer :: Linha -> Player
-- ^ Função que, a partir de uma string de um jogador, gera o túpulo relativo ao mesmo.
tuplePlayer string = (   read (take 1 string) :: Int,
                               pAbcis string    ,
                               pOrden string    ,
                          howMany1 '+' string    ,
                          howMany1 '!' string     )


                                                                                                                                                                       ---
                                                                                                                                                                       ---
-------TÚPULOS DE ESTADO-----------


takeLString :: Estado -> Mapa
-- ^ Função que retira de um estado a lista de strings associada à representação do mapa.
takeLString (a,_,_,_,_,_,_,_) = a


takeRocks :: Estado -> [Brick]
-- ^ Função que retira de um estado a lista de pedras no mapa.
takeRocks  (_,a,_,_,_,_,_,_) = a



takeBricks :: Estado -> [Brick]
-- ^ Função que retira de um estado a lista de tijolos no mapa.
takeBricks  (_,_,a,_,_,_,_,_) = a


takePowers  :: Estado -> [Powerup]
-- ^ Função que retira de um estado a lista de powerups no mapa, em forma de túpulos.
takePowers  (_,_,_,a,_,_,_,_) = a


takeBombs1   :: Estado -> [Bomb]
-- ^ Função que retira de um estado a lista de bombas no mapa, em forma de túpulos.
takeBombs1   (_,_,_,_,a,_,_,_) = a


takePlayers1 :: Estado -> [Player]
-- ^ Função que retira de um estado a lista de jogadores no mapa, em forma de túpulos.
takePlayers1 (_,_,_,_,_,a,_,_) = a


takeTime    :: Estado -> Float
-- ^ Função que retira de um estado o tempo do jogo.
takeTime    (_,_,_,_,_,_,a,_) = a

takeMen :: Estado -> String
-- ^ Função que retira de um estado o elemento que especifica o tipo de interface a representar.
takeMen     (_,_,_,_,_,_,_,a) = a

                                                                                                                                                                       ---
                                                                                                                                                                       ---
-------TÚPULOS DE POWERUPS---------

takeChar    :: Powerup -> Char
-- ^Função que descobre o tipo de powerup representado pelo túpulo (obtém o seu @Char@ representativo)
takeChar (a,_,_)          = a

takeX       :: Powerup -> Abcissa
-- ^ Função que obtém a abcissa de um determinado powerup.
takeX    (_,x,_)          = x

takeY       :: Powerup -> Ordenada
-- ^ Função que obtém a ordenada de um determinado powerup.
takeY    (_,_,y)          = y


                                                                                                                                                                       ---
                                                                                                                                                                       ---
howMany1 :: Char -> Linha -> Int
-- ^ Função que conta o número de vezes que um certo caracter ocorre numa string, com utilidade, por exemplo, no caso de saber quantos powerups de determinado tipo tem um jogador.
howMany1 _ [] = 0
howMany1 c (h:t) = if  h == c      then 1 + howMany1 c t
                                  else     howMany1 c t


isInt :: Float -> Bool
-- ^ Função que verifica se um float é inteiro.
isInt x = if x == fromIntegral (truncate x) then True else False


deci :: Float -> Float
-- ^Função que devolve a parte decimal de um float
deci t = t - fromIntegral (truncate t)
                                                                                                                                                                       ---
                                                                                                                                                                       ---
