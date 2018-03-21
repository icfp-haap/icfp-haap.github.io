{-|
Module : Define
Description : Este módulo serve para auxiliar a Tarefa5.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

O objetivo deste módulo é o de defenir todos os tipos que serão usado na Tarefa5 
assim como criar funções que os iniciam. 
Neste código podem-se encontrar também constantes que são usados no ficheiro principal da Tarefa5
-}
module Define where

import Load
import LI11718
import Graphics.Gloss

{-| 
Tipo de dados que será usado para identificar se um dado carro é um Jogador ou Bot.
-}
data Turing = AI | HUMAN

{-| 
Tipo de dados que descreverá o todo o estado da aplicação.
-}
data Ket = Ket { board    :: Board    ,
                 car      :: Vehicle  ,
                 ico      :: Icon     ,
                 jogo     :: Jogo     ,
                 back     :: Picture  ,
                 tipo     :: [Turing] ,
                 timeout  :: [Float ] ,
                 edges    :: [Acao ]  ,
                 rCode    :: Int  


               }

--- Mapas ---------------------------------------------------------------------------------------------------------
{-| 
Um dos mapas que será usado no jogo.  
-}
m1 = Mapa ((6,4),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-2),Peca (Rampa Oeste) (-3),Peca (Rampa Oeste) (-4),Peca Recta (-4),Peca Recta (-4),Peca Recta (-4),Peca (Rampa Este) (-4),Peca Recta (-3),Peca (Curva Este) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-3),Peca Recta (-3),Peca Recta (-3),Peca (Curva Sul) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca (Curva Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Oeste) (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca Lava 0]
                       ,[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0]
                       ,[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-2),Peca (Curva Sul) (-2),Peca Lava 0]
                       ,[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-3),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-4),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca (Curva Norte) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta (-2),Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Recta (-2),Peca (Rampa Norte) (-3),Peca Lava 0,Peca (Curva Norte) (-4),Peca Recta (-4),Peca (Curva Sul) (-4),Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca (Rampa Sul) (-2),Peca (Rampa Norte) (-4),Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Recta (-1),Peca Recta (-4),Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Sul) (-1),Peca (Curva Oeste) (-4),Peca Recta (-4),Peca (Curva Sul) (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

----------------------------------------------------------------------------------------------------
{-| 
Outros dos mapas que serão usados no jogo.  
-}
m2 = Mapa ((4,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-3),Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Recta (-3),Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Norte) (-3),Peca (Curva Sul) (-3),Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Oeste) (-3),Peca Recta (-3),Peca (Curva Este) (-3),Peca (Rampa Norte) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-3),Peca (Curva Sul) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

------------------------------------------------------------------------------------------------------------
{-|
Numero de frames por segundo.
-}
fr :: Int
fr = 30

{-|
Dimensões da janela onde decorerá o jogo.
-}
dm :: Display
dm = InWindow "Micro Machine Space Edition" (1000,1000) (50,50)

{-|
Ponto onde se inicia o tabuleiro.
-}
origem :: (Float , Float)
origem = t negate ( 87.5 , 87.5)

{-|
Configurações para o jogo.
-}
game_settings :: [ ( Propriedades  , Tempo )]
-- k_atrito, k_pneus, k_acel, k_peso, k_nitro, k_roda     
game_settings  = [(Propriedades 2 3 4 2 15 180,4),(Propriedades 1 20 4 2 15 180,0)]

------------------------------------------------------------------------------------------------------------
{-|
Esta função receberá um tipo que contêm várias pictures e com isso construir a imagem que representará o tipo Mapa.
-}
readMap :: Board -> Mapa -> [Picture]
readMap im (Mapa (w,_) tab) = get_Position $ defStart (start im) w $ map (map (aux im)) tab 
                 where aux :: Board -> Peca -> Picture    
                       aux im (Peca Lava x)          =  space im
                       aux im (Peca Recta x)         =  recta im !! negate x  
                       aux im (Peca (Curva Norte) x) =  curvN im !! negate x  
                       aux im (Peca (Curva Sul) x)   =  curvS im !! negate x 
                       aux im (Peca (Curva Este) x)  =  curvE im !! negate x 
                       aux im (Peca (Curva Oeste) x) =  curvO im !! negate x 
                       aux im (Peca (Rampa Norte) x) =  rampN im !! negate x 
                       aux im (Peca (Rampa Sul)  x)  =  rampS im !! negate x 
                       aux im (Peca (Rampa Este) x)  =  rampE im !! negate x 
                       aux im (Peca (Rampa Oeste) x) =  rampO im !! negate x 
{-|
Esta função recebe uma lista de lista de imagens de peças e vai defenir os limites para a função 'auxPosition'.
-}
get_Position :: [[Picture]] -> [Picture]
get_Position l = auxPosition (-n/2) (m/2) l
                        where n = fromIntegral $ length $ head l
                              m = fromIntegral $ length l
{-|
Esta função recebendo uma dada origem e uma lista de lista com pictures vai mudar a localizaão relativa destas e colocar numa lista.
-}
auxPosition :: Float -> Float -> [[Picture]] -> [Picture]
auxPosition x y [] = []
auxPosition x y ([]:ys) = auxPosition (-n/2 ) (y-175) ys
                        where n = fromIntegral $ length $ head ys 
auxPosition x y ((h:t):ys) =  Translate x y h : auxPosition (x+175) y (t:ys)

{-| 
Esta função vai substituir a picture que se encontra na inicio do mapa.
-}
defStart :: Picture -> (Int , Int) -> [[Picture]] -> [[Picture]]
defStart g (x,y) l = replaceAtIndex' y (replaceAtIndex' x newversion (l!!y) ) l
            where newversion = Pictures $ (l!!y) !!x : [ scale 1 1.1 $ translate (-10) 0 g ]
            
------------------------------------------------------------------------------------------------------------
{-|   
      Esta função inicia o estado da aplicação.
-}
__init__ :: IO Ket
__init__    = do
             x <-  loadBoard
             y <-  loadVehicle
             z <-  loadIcon
             let k = Ket {board =x,
                          car=y,
                          ico=z,
                          jogo= init_jg m1 ( head game_settings  ),
                          back = Pictures $ readMap x m2 ,
                          tipo = [] , 
                          timeout = replicate 4 0,
                          edges = replicate 4  Acao { acelerar = False , travar = False , esquerda = False, direita = False, nitro = Nothing } ,
                          rCode = 0 }

             return k
{-|
Esta função recebendo uma mapa devolve as confirações inicias do jogo que permitem fazer uma corrida com 4 elementos.  
-}
init_jg :: Mapa ->(Propriedades, Tempo)->Jogo
init_jg (Mapa u v ) (a,b) = jg
            where  jg = Jogo { mapa = Mapa u v 
                             , pista = a 
                             , carros = foldr slide [] $ initCar 4 u
                             , nitros = replicate 4 b
                             , historico = [[],[],[],[]] }
{-| 
Esta função serve para posicionar os carros na linha de partida.
-}
slide :: Carro -> [Carro] -> [Carro]
slide h r = h { posicao = (a,b + 0.2 * n + 0.1) } : r 
                where (a,b) = posicao h
                      n     = fromIntegral $ length r
{-|
Esta função inicia um grupo de carros na linha de partida.
-}
initCar :: Int -> (Posicao, Orientacao ) -> [Carro]
initCar n ( s, o ) = replicate n Carro {posicao = t fromIntegral s , direcao = aux o, velocidade = (0,0) } 
        where aux :: Orientacao -> Angulo
              aux Norte = 90
              aux Este  = 0
              aux Oeste = 180
              aux Sul   = 270 

------------------------------------------------------------------------------------------------------------
{-|
Aplica uma função a cada elemento de um dado tuplo.
-}
t :: (a -> b) -> (a,a) -> (b,b)
t  f (x,y) = (f x ,f y)

{-|
Esta função substitui um elemento de uma por outro no indice indicado.

      ==Exemplos de utilização:

      >>> replaceAtIndex 2 10 [0,0,0,0,0]
      [0,0,10,0,0]

      >>> replaceAtIndex 4 11 [3,1,2,6,3]
      [3,1,2,6,11]

-}
replaceAtIndex' :: Int -> a -> [a] -> [a]
replaceAtIndex' n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls