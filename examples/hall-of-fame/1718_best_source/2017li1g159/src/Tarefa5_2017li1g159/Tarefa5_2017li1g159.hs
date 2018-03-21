{-|
Module : Tarefa 5
Description : Tarefa 5 Trabalho prático.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

Este módolo contêm a solução da Tarefa 5 do trabalho prático de Laboratórios de Informática 1.
-}

module Main where

import Graphics.Gloss
import LI11718
import Load
import Define
import Graphics.Gloss.Interface.Pure.Game
import Tarefa3Opt
import Tarefa4Opt
import Tarefa6_2017li1g159
import Direction

{-|
Esta é função que executará o jogo.
-}
main :: IO ()
main = do
        st <- __init__ 
               
        play dm black fr st load act_to treact
{-|
plano de fundo.
-}
allblack = rectangleSolid 3000 3000
{-|
Esta função desenhará o estado do jogo no ecra.
-}
load :: Ket -> Picture
load x | rCode x < 90   = blank
       | rCode x < 160  = color (greyN $ ( fromIntegral (rCode x) - 90) /120 ) allblack    
       | rCode x < 210  = color white allblack
       | rCode x < 310  = Pictures $ color white allblack : [symbol (ico x)]
       | rCode x == 340 = Pictures [ color white allblack ,translate (-60) 0 $ playb (ico x) ]  
       | rCode x == 350 = Pictures $ color white allblack : [ translate 0 (-20) $ scale 1.1 1.1 $ map1 ( ico x)] 
       | rCode x == 351 = Pictures $ color white allblack : [ translate 0 (-20) $ scale 1.1 1.1 $ map2 ( ico x)] 
       | rCode x == 352 = Pictures $ color white allblack : [ translate 0 (-20) $ scale 1.1 1.1 $ voltar( ico x)] 
       | rCode x >= 400 && rCode x <413 = Pictures $ dat k0 k1 : translate 0 200 (scale 0.8 0.8 (do3  (ico x))) : [] 
       | rCode x >= 413 && rCode x <426 = Pictures $ dat k0 k1 : translate 0 200 (scale 0.8 0.8 (do2  (ico x))) : [] 
       | rCode x >= 426 && rCode x <440 = Pictures $ dat k0 k1 : translate 0 200 (scale 0.8 0.8 (do1  (ico x))) : []
       | rCode x >= 440 && rCode x <450 = Pictures $ dat k0 k1 : translate 0 200 (scale 0.8 0.8 (dogo (ico x))) : []
       | rCode x == 450 =  Pictures $ dat k2 k3 : [translate 400 100 (get_Helm (nitros (jogo x)) (ico x) (timeout x))]
       | rCode x >= 500 && verifywin (mapa (jogo x) ) (tipo x) (historico (jogo x) ) && rCode x <570 = Pictures [dat k2 k3 , scale 0.7 0.7 (win (ico x)) ]
       | rCode x >= 500 && rCode x < 570 = Pictures [ dat k2 k3 , scale 0.7 0.7 (lose (ico x) )]
       | rCode x < 2000 = color white allblack
                        where dat cx cy = translate cx cy (scale alp bet usual) 
                              (a,b) = t realToFrac $ posicao $ carros (jogo x) !! 3 --- Perspectiva bot.
                              (c,d) = t realToFrac $ posicao $ carros (jogo x) !! 0 --- Perspectiva player
                              alp = 2 * 1000/ (175*n)
                              bet = 2 * 1000/ (175*m)
                              (k0,k1) = sett (a,b) $ (8,10) == (n,m)
                              (k2,k3) = sett (c,d) $ (8,10) == (n,m)
                              (Mapa (un,_) _) = mapa (jogo x)
                              (n,m) = mapDim (mapa (jogo x))
                              usual = Pictures $  translate (-n*175/2) (m*175/2) (back x) : fg x ( map (translate (-n*175/2) (m*175/2) . scale 0.09 0.125) (getCar x) ) 
{-|
Esta função permite aceder aos diferentes perfis de visualização para cada mapa.
-} 
sett :: (Float,Float )-> Bool ->(Float, Float)
sett (a,b) True = (700 - a*170 , b* 120 -700)
sett (a,b) _  = (  700 - a*80 ,b * 90 -700 )

{-|
Esta função verifica se quem ganhou foi um player ou um bot.
-}
verifywin :: Mapa -> [Turing] -> [[Posicao]]->Bool
verifywin  _ [] [] = False
verifywin  (Mapa (v,g) u ) (AI:t) (x:xs) = False || verifywin (Mapa (v,g) u ) t xs
verifywin  (Mapa (v,g) u ) (HUMAN:t) (x:xs) = ( ( length x + 2 > length (vecFile (Mapa (v,g) u )) ) && (head x == v) ) || verifywin (Mapa (v,g) u ) t xs

{-|
Esta função coloca as imagens dos carros na posição onde os carros estão.
-}
fg :: Ket -> [Picture]->[Picture] -- zipWith (curry aux) (map posicao (carros (jogo x ) ))
fg x = zipWith (curry aux) (map posicao (carros (jogo x ) ))
        where aux :: (Ponto,Picture) ->Picture
              aux ((x,y),p) = let (a,b) = soma origem (t (175*) (t realToFrac (x,y)) ) in translate a (-b) p 
 
{-|
Esta função pões no ecra os capacetes que representam os diferentes carros.
-}
get_Helm ::[Tempo] -> Icon -> [Float] -> Picture
get_Helm nt ic l = Pictures $ map (aux ic) $ zip (zip [0..3] l) nt
        where aux :: Icon -> ((Float , Float),Double) -> Picture
              aux ic (( n , fl),ndt) | fl >0  = translate 0 (n*70) $ scale 0.1 0.1 $ Pictures [ helmCinz ic, scale 1.5 1.5 $translate (-450) 0 (nCinz ic)]

              aux ic ((0 ,_),ndt) | ndt > 0   = translate 0 (0*70) $ scale 0.1 0.1 $ Pictures [helmVermelho ic, scale 1.5 1.5 $translate (-450) 0 (nVermelho ic)]
                                  | otherwise = translate 0 (0*70) $ scale 0.1 0.1 $ Pictures [helmVermelho ic, scale 1.5 1.5 $translate (-450) 0 (nCinz ic)]

              aux ic ((1 ,_),ndt) | ndt > 0   = translate 0 (1*70) $ scale 0.1 0.1 $ Pictures [helmAzul ic, scale 1.5 1.5 $ translate (-450) 0 (nAzul ic)]
                                  | otherwise = translate 0 (1*70) $ scale 0.1 0.1 $ Pictures [helmAzul ic, scale 1.5 1.5 $ translate (-450) 0 (nCinz ic)]

              aux ic ((2 ,_),ndt) | ndt >0    = translate 0 (2*70) $ scale 0.1 0.1 $ Pictures [helmVerde ic,scale 1.5 1.5  $ translate (-450) 0 (nVerde ic)]
                                  | otherwise = translate 0 (2*70) $ scale 0.1 0.1 $ Pictures [helmVerde ic,scale 1.5 1.5  $ translate (-450) 0 (nCinz ic)]

              aux ic ((3 ,_),ndt) | ndt >0    = translate 0 (3*70) $ scale 0.1 0.1 $ Pictures [helmAmarelo ic, scale 1.5 1.5 $ translate (-450) 0 (nAmarelo ic)]
                                  | otherwise = translate 0 (3*70) $ scale 0.1 0.1 $ Pictures [helmAmarelo ic, scale 1.5 1.5 $ translate (-450) 0 (nCinz ic)] 
{-|
Esta função recebe um mapa e devolve as respetivas dimensões.

-}
mapDim :: Mapa -> (Float,Float)
mapDim (Mapa _ tab) = (fromIntegral $ length $ head tab , fromIntegral $ length tab  )

{-|
Esta função devolve uma lista com as diferentes imagen dos carros.
-}
getCar :: Ket ->[Picture]
getCar x =  map (aux (car x)) $ zip (zip [0..3] ( carros (jogo x))) (edges x)  ---- map (translate (-n*175/2) (m*175/2)) $

                where aux :: Vehicle-> ((Int,Carro),Acao) -> Picture -- Falta incluir nitro ou não
                      aux r ((n,c), k ) --- 
                                  | n==0 && nitro k /= Nothing = rotate (realToFrac ( (-1) *direcao c)) $ translate (-310) 0 (vermelhon r)     
                                  | n==0  = rotate (realToFrac ( (-1) *direcao c)) $ vermelho r

                                  | n==1 && nitro k /= Nothing    = rotate (realToFrac ( (-1) *direcao c)) $ translate (-310) 0 (verden r)
                                  | n==1  = rotate (realToFrac ( (-1) *direcao c)) $ verde r

                                  | n==2 && nitro k /= Nothing    = rotate (realToFrac ( (-1) *direcao c)) $ translate (-310) 0 (azuln r)
                                  | n==2  = rotate (realToFrac ( (-1) *direcao c)) $ azul r

                                  | n==3 && nitro k /= Nothing    = rotate (realToFrac ( (-1) *direcao c)) $ translate (-310) 0 (amarelon r)
                                  | n==3  = rotate (realToFrac ( (-1) *direcao c)) $ amarelo r
                                        where (a,b)= soma (t realToFrac ( t (175*) (posicao c))) origem
{-|
Esta função irá processar as alterações de estado do jogo. 
-}
act_to :: Event -> Ket -> Ket                      
act_to (EventKey (SpecialKey KeyDown) Down _ _) x   | rCode x == 350 = x { rCode = 352 }
                                                    | rCode x == 351 = x { rCode = 352 }
act_to (EventKey (SpecialKey KeyUp) Down _ _) x     | rCode x == 352 = x { rCode = 350 }
                                                    | rCode x == 351 = x { rCode = 350 }
act_to (EventKey (SpecialKey KeyRight ) Down _ _) x | rCode x == 350 = x { rCode = 351 }
act_to (EventKey (SpecialKey KeyLeft ) Down _ _)  x | rCode x == 351 = x { rCode = 350 }
act_to (EventKey (SpecialKey KeyEnter) Down _ _)  x | rCode x == 340 = x { rCode = 350 , tipo = replicate 1 HUMAN ++ replicate 3 AI } -- 1 player
                                                    | rCode x == 352 = x { rCode = 340 } -- back
                                                    | rCode x == 350  = x { rCode = 400 ,back = Pictures $ readMap (board x) m2 , jogo = init_jg m2 (game_settings !!1 ) }
                                                    | rCode x == 351  = x { rCode = 400 ,back = Pictures $ readMap (board x) m1 , jogo = init_jg m1 (head game_settings ) }
act_to (EventKey (SpecialKey c ) Down _ _) x        | c == KeyUp    && rCode x == 450 = x { edges = replaceAtIndex 0 (k { acelerar = True }) (edges x) }
                                                    | c == KeyDown  && rCode x == 450 = x { edges = replaceAtIndex 0 (k { travar   = True }) (edges x) }
                                                    | c == KeyRight && rCode x == 450 = x { edges = replaceAtIndex 0 (k { direita  = True }) (edges x) }
                                                    | c == KeyLeft  && rCode x == 450 = x { edges = replaceAtIndex 0 (k { esquerda = True }) (edges x) }
                                                    | c == KeyShiftR && rCode x == 450 = x { edges = replaceAtIndex 0 (k { nitro = Just 0 }) (edges x)  }

                                                                where k = head (edges x )

act_to (EventKey (SpecialKey c ) Up   _ _) x        | c == KeyUp    && rCode x == 450 = x { edges = replaceAtIndex 0 (k { acelerar = False }) (edges x) }
                                                    | c == KeyDown  && rCode x == 450 = x { edges = replaceAtIndex 0 (k { travar   = False }) (edges x) }
                                                    | c == KeyRight && rCode x == 450 = x { edges = replaceAtIndex 0 (k { direita  = False }) (edges x) }
                                                    | c == KeyLeft  && rCode x == 450 = x { edges = replaceAtIndex 0 (k { esquerda = False }) (edges x) }
                                                    | c == KeyShiftR && rCode x == 450 = x { edges = replaceAtIndex 0 (k { nitro = Nothing }) (edges x)  }
                                                    | otherwise = x
                                                                where k = head (edges x )

act_to _ x = x { edges = replaceAtIndex 0  Acao { acelerar = False , travar = False , esquerda = False, direita = False, nitro = Nothing }  (edges x) }

{-| 
Esta função vai processar as consequências da passagem do tempo ao estado do jogo.
-}
treact :: Float -> Ket -> Ket
treact _ x | rCode x < 340  = x { rCode = rCode x + 1 }
           | rCode x >= 400 && rCode x < 450  = x { rCode = rCode x + 1 }
           | (rCode x == 450 || rCode x == 460 ) && endhi (jogo x)  = x { rCode = 500 }
           | rCode x == 450 || rCode x == 460 = nullspc ( x { jogo  = njg , edges = sv })
           | rCode x >= 500 && rCode x < 570  = x { rCode = rCode x + 1 }
           | rCode x == 570 = x { rCode = 340 }
           | otherwise   = x
                where njg =  episode (jogo x) all_mighty 
                      (_ , sv) = unzip all_mighty
                      all_mighty = map ( aux (jogo x) ) $ zip (zip [0..3] (tipo x) )(edges x)
                                where aux :: Jogo -> ((Int ,Turing), Acao) -> (Int , Acao )
                                      aux jg ((n,AI),_) = (n,bot (1/30) jg n)
                                      aux jg ((n,_),ac) = (n, ac )
{-|
Esta função executa a atualização da velocidade dos carros no jogo, tempos de nitro e historico de posição dos carros.
-}                                 
episode :: Jogo -> [(Int , Acao )] -> Jogo
episode = foldl aux
        where aux :: Jogo -> (Int, Acao) -> Jogo 
              aux jg (n,ac) =  atualiza (1/30) jg n ac      
{-|
Esta posição verifica se algum dos carros terminou a corrida.
-}
endhi :: Jogo -> Bool
endhi jg  = foldr 
                  ((||) .
                     (\ h -> length h + 3 > length (vecFile (mapa jg)) &&
                        that == head h && soma that (-1, 0) == h!! 1 ))
                  False 
                  (historico jg)
                  
                  
-- foldr (||) False (map  (\ h -> length h + 3 > length (vecFile (mapa jg)) && that == head h && soma that (-1, 0) == h!! 1 ) (historico jg) )
                where ( Mapa (that,_) _ ) =  mapa jg

{-| 
Esta função executa o movimento dos carros e também caso este se tenha destruido aplica uma penalziação. 
-}
nullspc :: Ket -> Ket
nullspc x = flag x $ zip [0..3] ( map (movimenta tab (1/30) ) (carros (jogo x) ))
        where (Mapa _ tab) = mapa (jogo x)
{-|
Esta função aplica uma penalização caso o carro tenha sido destruido e atualiza o tempo de espera caso um carro se encontre penalizado. 
-}
flag :: Ket -> [(Int ,Maybe Carro)] -> Ket
flag x []    = x
flag x ((n,Nothing):t) = flag (x { timeout = replaceAtIndex n 0.5 (timeout x) }) t
flag x ((n,Just a ):t) = if timeout x !!n <= 0 then flag (x {  jogo = (jogo x) { carros  = replaceAtIndex n a (carros (jogo x))}    }) t
                                               else flag (x { timeout = replaceAtIndex n ( timeout x !!n - (1/30)) (timeout x) }) t
