module Main(main) where

import GHC.Float
import Graphics.Gloss                       -- interface gloss
import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import Graphics.Gloss.Interface.Pure.Game   -- importar o tipo Event
import Tarefa4_2017li1g12
import LI11718
import Tarefa3_2017li1g12
import Tarefa6_2017li1g12
import Tarefa2_2017li1g12
import Data.List

--data Jogo = Jogo  { mapa:: Mapa, pista:: Propriedades, carros:: [Carro], nitros:: [Tempo], historico :: [[Posicao]]}

{-| Função que define os elementos que pertencem e que são necessários para o estado do jogo.
-}
type Estado = (Float, Int, [Picture], [Picture],([Picture],Int,[Picture]), Jogo, [Acao], Win, Float, [Int])


type Win = (Int,Int)
{-| Esta função é do tipo 'Jogo' e é apenas usada no estado inicial. -}

jogo :: Jogo
jogo = Jogo mapa1 asfalto [Carro (0,0) 0 (0,0),Carro (0,0) 0 (0,0),Carro (0,0) 0 (0,0),Carro (0,0) 0 (0,0)] [5,5,5,5] [[],[],[],[]] 
    
timeframe = 0.01

{-
mapaTesteVelMaximaCurva :: Mapa
mapaTesteVelMaximaCurva = (Mapa ((2,6),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])
-}

{-| Lista de propriedades para no jogo, sendo estas Gelo, terra e asfalto-}
propriedades :: [Propriedades]
propriedades = [terra,gelo,asfalto]

terra, gelo, asfalto :: Propriedades
terra   = Propriedades 2 3 4 2 15 180
gelo    = Propriedades 0.3 0.4 2 1.5 15 270
asfalto = Propriedades 4 5 8 4 10 120

{-| A função menu1 é uma função que dado um tuplo, com duas listas e um inteiro, pertencentes ao estado do jogo, define qual será o proximo menu a ser apresentado, ou seja quais vão ser as proximas imagens a ser apresentadas no ecrã.-}

menu1 :: ([Picture],Int, [Picture]) ->  ([Picture],Int, [Picture])
menu1 (l,i,m) | i==9  = menu1 (l,6,m)
              | i==5  = menu1 (l,8,m)
              | i==4  = menu1 (l,1,m)
              | i==0  = menu1 (l,3,m)
              | i==1 = ([(!!) m 0],1,m)
              | i==2 = ([(!!) m 1],2,m)
              | i==3 = ([(!!) m 16],3,m)
              | i==6 = ([(!!) m 17],6,m) 
              | i==7 = ([(!!) m 18],7,m)
              | i==8 = ([(!!) m 19],8,m) 
              | i==10 || i==19 = menu1 (l,(i+7),m)
              | i==18 || i==27 = menu1 (l,(i-7),m)
              |i == 11 || i==20 = ([(!!) m 6], i, m )
              |i == 12 || i==21 = ([(!!) m 7], i, m )
              |i == 13 || i==22 = ([(!!) m 8], i, m )
              |i == 14 || i==23 = ([(!!) m 9], i, m )
              |i == 15 || i==24 = ([(!!) m 10], i, m )
              |i == 16 || i==25 = ([(!!) m 11], i, m )
              |i == 17 || i==26 = ([(!!) m 12], i, m )
              |i == 30 =  ([(!!) m 13], i, m )
              |i == 31 =  ([(!!) m 14], i, m )
              |i == 32 =  ([(!!) m 15], i, m )
 {-| Lista de ações, que vai ser usada na função 'jogo'.
 -}
acao :: [Acao]
acao = [Acao False False False False Nothing,Acao False False False False Nothing,Acao False False False False Nothing,Acao False False False False Nothing]


{- O estadoInicial é, como o nome indica o estado em que o jogo vai começar, e neste estado têm de ser dados todos os elementos necessários ao jogo.
-}
estadoInicial :: IO Estado
estadoInicial =  do
    pl    <- loadBMP "PecaLava0.bmp"
    pr    <- loadBMP "recta0.bmp"
    prn   <- loadBMP "rectan.bmp"
    cnn   <- loadBMP "curvaNorten.bmp"
    csn   <- loadBMP "curvaSuln.bmp"
    cen   <- loadBMP "curvaEsten.bmp"
    con   <- loadBMP "curvaOesten.bmp"  
    cn    <- loadBMP "curvaNorte.bmp"
    cs    <- loadBMP "curvaSul.bmp"
    ce    <- loadBMP "curvaEste.bmp"
    co    <- loadBMP "curvaOeste.bmp"
    rs    <- loadBMP "rampasu.bmp"
    re    <- loadBMP "rampaes.bmp"
    ro    <- loadBMP "rampaoe.bmp"
    rn    <- loadBMP "rampano.bmp"
    rnn    <- loadBMP "rampa.bmp"
    rsn    <- loadBMP "rampas.bmp"
    ren    <- loadBMP "rampa2.bmp"
    ron    <- loadBMP "rampao.bmp"
    c1    <- loadBMP "c.bmp"
    c2    <- loadBMP "c2.bmp"
    c3    <- loadBMP "c3.bmp"
    m1    <- loadBMP "menu1.1.bmp"
    m2    <- loadBMP "menu1.2.bmp"
    m3    <- loadBMP "menu1.3.bmp"
    asf   <- loadBMP "asf.bmp"
    ter   <- loadBMP "ter.bmp" 
    gel   <- loadBMP "gel.bmp"    
    mp1   <- loadBMP "mp1.bmp"
    mp2   <- loadBMP "mp2.bmp"
    mp3   <- loadBMP "mp3.bmp" 
    mp4   <- loadBMP "mp4.bmp" 
    mp5   <- loadBMP "mp5.bmp"
    mp6   <- loadBMP "mp6.bmp"
    mp7   <- loadBMP "mp7.bmp"
    s3    <- loadBMP "3.bmp"
    s2    <- loadBMP "2.bmp"
    s1    <- loadBMP "1.bmp"
    go    <- loadBMP "go.bmp"
    v1    <- loadBMP "vence1.bmp"
    v2    <- loadBMP "vence2.bmp"
    v3    <- loadBMP "vence3.bmp"
    return (0.0,0, [], [pl, pr, cn,cs,ce,co,rn,rs,re,ro,c2,c3,prn,cnn,csn,cen,con,rnn,rsn,ren,ron, c1 ],([m1],1,[m1,m2,s3,s2,s1,go,mp1,mp2,mp3,mp4,mp5,mp6,mp7,v1,v2,v3,m3,asf,ter,gel]), jogo, acao, (0, 0), 0,[0,0,0,0])
 
--(Int,Int) representa a posição da Picture
--Int representa o numero da oeça a ser retirado a apresentar

{-| A função 'aplicavoltas' vai ser a função que vai atualizar o número de voltas de cada carro recursivamente.-}

aplicavoltas :: Posicao -> Orientacao -> Tabuleiro -> [[Posicao]] -> [Carro] -> [Int]
aplicavoltas pos oi tab [] posi = []
aplicavoltas pos oi tab (h:hs) (car:cars) = ((volta pos oi tab h ( posx,  posy) ): aplicavoltas  pos oi tab hs cars)
                         where Carro (posx,posy) ang vel = car 

{-| A função 'volta' vai ser a função que vai atualizar o número de voltas de um carro de cada vez.-}
volta ::  Posicao -> Orientacao -> Tabuleiro -> [Posicao] -> Ponto -> Int
volta (xpi,ypi) oi tab  l (xa,ya) | length l == length posicoes && xa > (fromIntegral xpi) +0.5 && xa < (fromIntegral xpi) +0.535 = 1
                                  | otherwise = 0
                            where (pecas,orientacoes,posicoes) = separatePecaOrientacaoPosicao (listaposicoes (xpi,ypi) oi tab 0 oi (xpi,ypi))

{-| Função que altera o estado do jogo quando o tempo avança @n@ segundos. Esta função apenas guarda o tempo que passou no estado.
-}
reageTempo :: Float -> Estado -> Estado
reageTempo (ts) (t, e,p,l,m,j,a,w,time,[v1,v2,v3,v4])|otherwise =  atualizaEstado (t+ts, e,p,l,m, atualizaMovimenta time timeframe j a,a,w,time,nvoltas )
                                                                              where [nv1,nv2,nv3,nv4]                           = aplicavoltas pos ori tab lp lc
                                                                                    nvoltas                                     = [nv1 +v1,nv2 +v2,nv3+v3,nv4+v4]
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa (pos,ori) tab                                  = map

{-| Função que altera o estado do jogo quando acontece um evento. Esta função permite que joguem dois jogadores em simultâneo, e também que se usem as setas do teclado e a tecla 'space' para intercalar entre os menus do jogo 
Esta função dá também para usar a letra j para voltar ao estado inicial.
-}
reageEvento :: Event -> Estado -> Estado
reageEvento (EventResize size) (t, i, l,p,(m,n,pi),j, [a,b,c,d], k , time,nvoltas) = (t, i, l,p,(m,n,pi),j,[a,b,c,d],size,time,nvoltas) 
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | i==0 && (n ==30 || n==31 || n==32) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),j,[a,b,c,d],w,time,nvoltas)
                                                                                          | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar True direita nitro,b,c,d], [Acao acelerar travar True direita nitro, b,c,d],w,time,nvoltas)
                                                                                          | i==0 = atualizaEstado (t, i, l,p,menu1 (m,n-1,pi),j,[a,b,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map

reageEvento (EventKey (SpecialKey KeyLeft)  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar False direita nitro,b,c,d], [Acao acelerar travar False direita nitro,b,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (SpecialKey KeyRight)  Down _ _)   (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)| i==0 && (n ==30 || n==31 || n==32) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),j,[a,b,c,d],w,time,nvoltas)
                                                                                         | (i==2||i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda True nitro,b,c,d], [Acao acelerar travar esquerda True nitro ,b,c,d],w,time,nvoltas)
                                                                                         | i==0 = atualizaEstado (t, i, l,p,menu1 (m,n+1,pi),j,[a,b,c,d],w,time,nvoltas)
                                                                                  where Acao acelerar travar esquerda direita nitro = a                                                         
reageEvento (EventKey (SpecialKey KeyRight)  Up _ _)   (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)  |  (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda False nitro,b,c,d], [Acao acelerar travar esquerda False nitro,b,c,d],w,time,nvoltas)                                                                            
                                                         where Acao acelerar travar esquerda direita nitro = a
reageEvento (EventKey (SpecialKey KeyUp)  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao True travar esquerda direita nitro,b,c,d], [Acao True travar esquerda direita nitro,b,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map

reageEvento (EventKey (SpecialKey KeyUp)  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao False travar esquerda direita nitro,b,c,d],[Acao False travar esquerda direita nitro,b,c,d],w,time,nvoltas)
                                                         where Acao acelerar travar esquerda direita nitro = a
                                                               Jogo map pr lc ln lp                        = j
                                                               Mapa x tab                                  = map
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar True esquerda direita nitro,b,c,d], [Acao acelerar True esquerda direita nitro,b,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (SpecialKey KeyDown)  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar False esquerda direita nitro,b,c,d], [Acao acelerar False esquerda direita nitro,b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (SpecialKey KeyEnter)  Down _ _) (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)  |  i==0 && n>=30 = (t, 0, [],p,menu1 (m,1,pi),j,[a,b,c,d],w,0,[0,0,0,0]) 
                                                                                               
                                                                                                                                                                  
reageEvento (EventKey (SpecialKey KeySpace)  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | i==0 && n==1 = atualizaEstado (t, 0, l,p,menu1 (m,11,pi), (carrosjogo j),[a,b,c,d],w,time,nvoltas)
                                                                                                | i==0 && n==2 = atualizaEstado (t, 0, l,p,menu1 (m,20,pi), (carrosjogo j),[a,b,c,d],w,time,nvoltas)
                                                                                                | i==0 && n==3 = atualizaEstado (t, 0, l,p,menu1 (m,6,pi), (carrosjogo j),[a,b,c,d],w,time,nvoltas)
                                                                                                | i==0 && n==6 = (t, 0, [],p,menu1 (m,1,pi),Jogo map asfalto lc ln lp,acao,w,0,[0,0,0,0]) 
                                                                                                | i==0 && n==7 = (t, 0, [],p,menu1 (m,1,pi),Jogo map terra lc ln lp,acao,w,0,[0,0,0,0]) 
                                                                                                | i==0 && n==8 = (t, 0, [],p,menu1 (m,1,pi),Jogo map gelo lc ln lp,acao,w,0,[0,0,0,0]) 
                                                                                                | i==0 && (n>=10 && n<20) = atualizaEstado (t, 1, l,p,(m,n,pi), (carrosjogo escmapa),[a,b,c,d],w,time,nvoltas)
                                                                                                | i==0 && n>=20 && n<30 = atualizaEstado (t, 3, l,p,(m,n,pi), (carrosjogo escmapa),[a,b,c,d],w,time,nvoltas)
                                                                                                | i==0 && n>=30 = (t, 0, [],p,menu1 (m,1,pi),jogo,acao,w,0,[0,0,0,0]) 
                                                                                                | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita (Just 0),b,c,d], [Acao acelerar travar esquerda direita (Just 0),b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
                                                                                    escmapa = escolhemapa j n
reageEvento (EventKey (SpecialKey KeySpace)  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita Nothing,b,c,d], [Acao acelerar travar esquerda direita Nothing,b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'm')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | otherwise = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita (Just 1),b,c,d], [Acao acelerar travar esquerda direita (Just 1),b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'n')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | otherwise = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita (Just 2),b,c,d], [Acao acelerar travar esquerda direita (Just 2),b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'b')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | otherwise = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita (Just 3),b,c,d], [Acao acelerar travar esquerda direita (Just 3),b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'b')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita Nothing,b,c,d], [Acao acelerar travar esquerda direita Nothing,b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map 
reageEvento (EventKey (Char 'm')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita Nothing,b,c,d], [Acao acelerar travar esquerda direita Nothing,b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'n')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [Acao acelerar travar esquerda direita Nothing,b,c,d], [Acao acelerar travar esquerda direita Nothing,b,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = a
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
--------------
--------------
-------------- jogador 2 
reageEvento (EventKey (Char 'a')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar True direita nitro,c,d], [a, Acao acelerar travar True direita nitro,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map

reageEvento (EventKey (Char 'a')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar False direita nitro,c,d], [a,Acao acelerar travar False direita nitro,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'd')  Down _ _)   (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda True nitro,c,d], [a ,Acao acelerar travar esquerda True nitro,c,d],w,time,nvoltas)
                                                                                  where Acao acelerar travar esquerda direita nitro = b                                                         
reageEvento (EventKey (Char 'd')  Up _ _)   (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)  |  (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda False nitro,c,d], [a,Acao acelerar travar esquerda False nitro,c,d],w,time,nvoltas)                                                                            
                                                         where Acao acelerar travar esquerda direita nitro = b
reageEvento (EventKey (Char 'w')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao True travar esquerda direita nitro,c,d], [a,Acao True travar esquerda direita nitro,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map

reageEvento (EventKey (Char 'w')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao False travar esquerda direita nitro,c,d],[a,Acao False travar esquerda direita nitro,c,d],w,time,nvoltas)
                                                         where Acao acelerar travar esquerda direita nitro = b
                                                               Jogo map pr lc ln lp                        = j
                                                               Mapa x tab                                  = map
reageEvento (EventKey (Char 's')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,menu1 (m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar True esquerda direita nitro,c,d], [a,Acao acelerar True esquerda direita nitro,c,d],w,time,nvoltas)
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 's')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar False esquerda direita nitro,c,d], [a,Acao acelerar False esquerda direita nitro,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
                                                                                                                                                                   
reageEvento (EventKey (Char 'q')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)| (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita (Just 0),c,d], [a,Acao acelerar travar esquerda direita (Just 0),c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'q')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita Nothing,c,d], [a,Acao acelerar travar esquerda direita Nothing,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'e')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita (Just 1),c,d], [a,Acao acelerar travar esquerda direita (Just 1),c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'r')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas)  | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita (Just 2),c,d], [a,Acao acelerar travar esquerda direita (Just 2),c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 't')  Down _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita (Just 3),c,d], [a,Acao acelerar travar esquerda direita (Just 3),c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 't')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita Nothing,c,d], [a,Acao acelerar travar esquerda direita Nothing,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map 
reageEvento (EventKey (Char 'e')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita Nothing,c,d], [a,Acao acelerar travar esquerda direita Nothing,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map
reageEvento (EventKey (Char 'r')  Up _ _)  (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i==2 || i==4) = atualizaEstado (t, i, l,p,(m,n,pi),atualizaMovimenta time timeframe j  [a,Acao acelerar travar esquerda direita Nothing,c,d], [a,Acao acelerar travar esquerda direita Nothing,c,d],w,time,nvoltas) 
                                                                              where Acao acelerar travar esquerda direita nitro = b
                                                                                    Jogo map pr lc ln lp                        = j
                                                                                    Mapa x tab                                  = map 
reageEvento (EventKey (Char 'j') Down _ _ ) (t, i, l,p,(m,n,pi),j,[a,b,c,d],w,time,nvoltas) | (i == 2 || i==4) = (t, 0, [],p,menu1 (m,1,pi),Jogo map pr lcs lns lps ,acao,w,0,[0,0,0,0]) 
                         where  Jogo map pr lc ln lp                        = j
                                Jogo maps prp lcs lns lps                   = jogo
reageEvento _ (t, i, l,p,(m,n,pi),j,a,w,time,nvoltas) = (t, i, l,p,(m,n,pi),j,a,w,time,nvoltas)
                                       

{-| A função 'escolhemapa' vai servir para que, recebendo um Jogo e um inteiro, que vai ser o mesmo que o inteiro pertencente a um tuplo do estado do jogo, se possa escolher qual o mapa para ser jogado. -}

escolhemapa :: Jogo -> Int -> Jogo
escolhemapa jogo i | i ==11 || i==20 = Jogo mapa1 pr lc ln lp   
                   | i ==12 || i==21 = Jogo mapa2 pr lc ln lp 
                   | i ==13 || i==22 = Jogo mapa3 pr lc ln lp 
                   | i ==14 || i==23 = Jogo mapa4 pr lc ln lp 
                   | i ==15 || i==24 = Jogo mapa5 pr lc ln lp 
                   | i ==16 || i==25 = Jogo mapa6 pr lc ln lp 
                   | i ==17 || i==26 = Jogo mapa7 pr lc ln lp 
            where Jogo map pr lc ln lp = jogo




{-| A função 'movimentacao' aplica a função movimenta ao jogo, pois esta devolve um resultado 'Maybe Carro', e por isso é necessário que exista uma função para interpretar estes resultados-}

movimentacao :: Maybe Carro -> Posicao -> Angulo -> Carro
movimentacao (Just x) k ang  = x 
movimentacao Nothing (a,b) ang = (Carro ((fromIntegral (a)+0.5), (fromIntegral (b)+0.5)) ang  (0,0))
              
{-| 'atualizaEstado' é uma função  que é chamada por varias funçoes e que vai atualizar e alterar o estado consoante os elementos presentes no estado (elementos estes que podem ser alterados noutras funções) -}


atualizaEstado :: Estado -> Estado
atualizaEstado (t,i,l,k,(me,mi,mm),j,a,(w1,w2),time,nvoltas) | i==0 = est
                                                     | time>=63.1 = (t, 0, [],k,menu1 (me,menufinal,mm),j,a,(w1,w2),0,[0,0,0,0])
                                                     | ( i==3 && time > 3.1) = (t,2, (m (t,2,l,k,(me,mi,mm),j,a,(w1,w2), time,nvoltas) tab (-350+somx/2,350-somy/2) (somx,somy))++ ([(  translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $  rotate ( double2Float (- qq)) $ scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x somx 0)  (posparaimag y somy 1) $  rotate ( double2Float (- q)) $ scale (somx/50) (somy/50) $ (k !! 11))]) ++ ([(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))]) ++ ([(translate 10000 10000 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))]) ++ ([translate 10000 10000 $ scale 0.2 0.2 $ Text $ show $ "Voltas:  " ++ show voltamax])  ,k,(me,mi,mm),j,a, (w1,w2),time,nvoltas)
                                                     | ( i==1 && time > 3.1) = (t,4, (m (t,4,l,k,(me,mi,mm),j,a,(w1,w2), time,nvoltas) tab (-350+somx/2,350-somy/2) (somx,somy))++ ([(  translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $  rotate ( double2Float (- qq)) $ scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))]) ++ ([(translate 10000 10000 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))]) ++ ([translate 10000 10000 $ scale 0.2 0.2 $ Text $ show $ "Voltas:  " ++ show voltamax])  ,k,(me,mi,mm),j,a, (w1,w2),time,nvoltas)
                                                     |  i==3  = (t,3, (m (t,3,l,k,(me,mi,mm),j,a,(w1,w2), time,nvoltas) tab (-350+somx/2,350-somy/2) (somx,somy))++ ([(  translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $  rotate ( double2Float (- qq)) $ scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x somx 0)  (posparaimag y somy 1) $  rotate ( double2Float (- q)) $ scale (somx/50) (somy/50) $ (k !! 11))]) ++ ( [(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))] ++ [partidas est]) ++ ([(translate 10000 10000 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))]) ++ ([translate 10000 10000 $ scale 0.2 0.2 $ Text $ show $ "Voltas:  " ++ show voltamax]) ,k,(me,mi,mm),j,a, (w1,w2),(time+0.01),nvoltas)
                                                     |  i==1  = (t,1, (m (t,1,l,k,(me,mi,mm),j,a,(w1,w2), time,nvoltas) tab (-350+somx/2,350-somy/2) (somx,somy))++ ([(  translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $  rotate ( double2Float (- qq)) $ scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))] ++ [partidas est]) ++ ([(translate 10000 10000 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))]) ++ ([translate 10000 10000 $ scale 0.2 0.2 $ Text $ show $ "Voltas:  " ++ show voltamax])  ,k,(me,mi,mm),j,a, (w1,w2),(time + 0.01),nvoltas)
                                                     | i==2   = (t,i,  (init(init (init (init (init l))))) ++ ([(translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $ rotate (double2Float ( -qq)) $  scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x somx 0)  (posparaimag y somy 1) $  rotate ( double2Float (- q)) $ scale (somx/50) (somy/50) $ (k !! 11))]) ++ ([(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))]) ++ ([color white $ (translate 200 200 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))])  ++ ([color white $translate 200 250 $ scale 0.2 0.2 $ Text $ show $ "Voltas:  " ++ show voltamax]), k, (me,mi,mm), (Jogo maps prs lcs lns lps),a,(w1,w2),(time+0.01),nvoltas)
                                                     | i==4   = (t,i,  (init(init(init (init l)))) ++ ([(translate (posparaimag xx somx 0)  (posparaimag yy somy 1) $ rotate (double2Float ( -qq)) $  scale (somx/50) (somy/50) $last k)])  ++ ([(  translate (posparaimag x3 somx 0)  (posparaimag y3 somy 1) $  rotate ( double2Float (- q3)) $ scale (somx/50) (somy/50) $ (k !! 10))]) ++ ([(color white $ translate 200 200 $ scale 0.3 0.3 $ Text $ show $ show (truncatef (63.1 -time) 1 ))]) ++ ([color white $ translate 200 250 $ scale 0.2 0.2 $ Text  $ show $ "Voltas:  " ++ show voltamax]), k, (me,mi,mm), (Jogo maps prs lcs lns lps),a,(w1,w2),(time+0.01),nvoltas)
                        where m   = mapaparaimagens
                              somx= 700 / fromIntegral (length (head tab))
                              somy= 700 / fromIntegral (length tab)
                              Jogo map pr lc ln lp     = j
                              Mapa (pi,oi) tab               = map
                              Jogo maps prs lcs lns lps= atualizaMovimenta time timeframe j a 
                              Carro (xx,yy) qq ww      =  (lcs !! 0 )
                              Carro (x,y) q w      =   (lcs !! 1)
                              Carro (x3,y3) q3 w3      =  (lcs !! 2)
                              est             = (t,i,l,k,(me,mi,mm),j,a,(w1,w2),time,nvoltas)
                              (menufinal,voltamax) = maiorvolta nvoltas lps 

{-| Esta função vai calcular qual dos carros se encontra na frente do percurso, e vai devolver um tuplo de inteiros, um com as voltas do carro que se encontra na frente, e outro com um inteiro que será usado para o menu final das corridas.-}
maiorvolta :: [Int] -> [[Posicao]] -> (Int,Int)
maiorvolta li lp = (voltaaux li lp, maximum li ) 


{-| sta função vai calcular o número que deverá ser devolvido aos menus finais.-}
voltaaux :: [Int] -> [[Posicao]] -> Int
voltaaux li lp | length listamaximo == 1 =  30 + (head listamaximo)
               | length listamaximo == 2 && (length (lp !! (head listamaximo))) > (length (lp !! (listamaximo !! 1))) = 30 + (head listamaximo) 
               | length listamaximo == 2 && (length (lp !! (head listamaximo))) < (length (lp !! (listamaximo !! 1))) = 30 + (listamaximo !! 1)
               | length listamaximo == 3 && (length (lp !! (head listamaximo))) > (length (lp !! (listamaximo !! 1))) && (length (lp !! (head listamaximo))) > (length (lp !! (listamaximo !! 2))) = 30 + (head listamaximo)
               | length listamaximo == 3 && (length (lp !! (listamaximo !! 1))) > (length (lp !! (listamaximo !! 0))) && (length (lp !! (listamaximo !! 1))) > (length (lp !! (listamaximo !! 2))) = 31
               | length listamaximo == 3 && (length (lp !! (listamaximo !! 2))) > (length (lp !! (listamaximo !! 0))) && (length (lp !! (listamaximo !! 2))) > (length (lp !! (listamaximo !! 1))) = 32
               | otherwise = 30 
             where listamaximo   = elemIndices (maximum li) li 




{-| A Função 'temRepetidos' vai calcular se uma lista tem ou não elementos repetidos.. -}
temRepetidos :: Eq a => [a] -> Bool
temRepetidos []    = False
temRepetidos (h:t) = elem h t || temRepetidos t

{-| A Função 'pMaior' vai calcular qual o index do maior elemento de uma lista. -}
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) | h == aux (h:t) = 0
             | otherwise = 1 + pMaior t
            where
              aux [x] = x
              aux (x:y:xs) | x>=y      = aux (x:xs)
                           | otherwise = aux (y:xs) 

{-| A função 'partidas' vai receber um Estado, e quando for chamada vai devolver uma imagem que aparece no ecrã, e que vai servir como contagem para o inicio da corrida, dando como ultima imagem uma indicação de partida. -}
partidas ::Estado -> Picture
partidas (t,i,l,k,(me,mi,mm),j,a,(w1,w2),time,nvoltas) | (time >=0 && time <1) = mm !! 2
                                              | (time >=1 && time <2) = mm !! 3
                                              | (time >=2 && time < 2.9) = mm !! 4
                                              | otherwise =  mm !! 5


{-| A função 'posparaimag' vai transformar uma posição de um carro que se encontra num tabuleiro  e vai converter esse valor para o ecrã do jogo. -}

posparaimag :: Double -> Float -> Int-> Float
posparaimag a t 0= double2Float (-350+ a*(float2Double t))
posparaimag a t 1= double2Float (-a*(float2Double t)  + 350)
         

{-| 'mapaparaimagens' é uma função recursiva que vai pegar num tabuleiro e vai devolver uma lista de imagens, com uma determinada posição, de forma a que quando estas imagens forem desenhadas possam representar esse mesmo tabuleiro. -}          

mapaparaimagens :: Estado -> Tabuleiro -> (Float,Float) -> (Float,Float)-> [Picture]
mapaparaimagens (t,e,p,pe,m,j,a,w,time,nvoltas) [](posx,posy) (somx,somy) = []
mapaparaimagens (t,e,p,pe,m,j,a,w,time,nvoltas) ([]:hs) (posx,posy) (somx,somy) =  mapaparaimagens (t,e,p,pe,m,j,a,w,time,nvoltas) hs (-350+somx/2,posy - somy) (somx,somy)
mapaparaimagens (t,e,p,pe,m,j,a,w,time,nvoltas) (h:hs) (posx,posy) (somx,somy) = (pip  (t,e,p,pe,m,j,a,w,time,nvoltas) (head h) (posx,posy) (somx,somy): mapaparaimagens (t,e,p,pe,m,j,a,w,time,nvoltas) (tail (h):hs) (posx+somx,posy) (somx,somy))
                              where pip = pecaimagempos
                              

                                                     
{- A função 'pecaimagempos' vai ser uma função que vai ser chamama pela @mapaparaimagens@ e que vai, consoante a peça que lhe for dada pela mapaparaimagens, devolver a imagem correspondente. -}                 

pecaimagempos ::Estado -> Peca -> (Float,Float) -> (Float,Float) -> Picture
pecaimagempos (tem,e,p,pe,m,j,ac,w,time,nvoltas) peca (x,y) (somx,somy)| t == Lava =  translate (x)  (y) $ scale (somx/50) (somy/50) $  head pe
                                                                      | t == Recta && a>=0 =  translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 1
                                                                      | t == Curva Norte && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 2
                                                                      | t == Curva Sul  && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 3
                                                                      | t == Curva Este && a>=0  = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 4
                                                                      | t == Curva Oeste && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 5
                                                                      | t == Rampa Norte && a>=0= translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 6
                                                                      | t == Rampa Sul  && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 7
                                                                      | t == Rampa Este && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 8
                                                                      | t == Rampa Oeste && a>=0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 9
                                                                      | t == Recta && a<0 =  translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 12
                                                                      | t == Curva Norte && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 13
                                                                      | t == Curva Sul  && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 14
                                                                      | t == Curva Este && a<0  = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 15
                                                                      | t == Curva Oeste && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 16
                                                                      | t == Rampa Norte && a<0= translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 17
                                                                      | t == Rampa Sul  && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 17
                                                                      | t == Rampa Este && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 17
                                                                      | t == Rampa Oeste && a<0 = translate (x)  (y) $ scale (somx/50) (somy/50) $ (!!) pe 17

          where Peca t a = peca


{-|  A função 'drawing' vai pegar num estado, e vai devolver uma picture. Esta função vai devolver imagens diferentes par cada estado do jogo. -}

drawing :: Estado -> Picture
drawing (t, i, l,p,(m,n,pi), j,a,w,time,nvoltas)           |i==0      = scale ((fromIntegral (fst w))/700) ((fromIntegral (snd w)) /700) $ Pictures m
                                              |otherwise = scale ((fromIntegral (fst w))/700) ((fromIntegral (snd w)) /700) $ Pictures l
                                              where Jogo map pr lc ln lp     = j
                                                    Mapa (pi, ori) tab               = map
                                                    [lc1,lc2,lc3,lc4]        = lp 
                                                    Carro (xx,yy) qq (w1,w2) =  (head lc)
                                                    Carro (xb,yb) qb (w5,w6) =  (lc !! 2)
                                                     
                                                    
{-(l ++ ([translate 100 150 $ Scale 0.2 0.2 $ Text $ show  $  "bot " ++ (show (truncate' w5 1)) ++ "," ++ (show (truncate' w6 1)) ] ++([translate 100 250 $ Scale 0.2 0.2 $ Text $ show  $  "Jogador 1 " ++ (show (truncate' w1 1)) ++ "," ++ (show (truncate' w2 1)) ]) ++ ([translate 100 0 $ Scale 0.2 0.2 $ Text $ show  $  "nitros " ++ (show (truncate' (ln!!0) 1)) ++ "," ++ (show (truncate' (ln !! 3) 1)) ] ++ [translate 100 (-100) $ Scale 0.2 0.2 $ Text $ show  $  time]) ))-}

--(l ++ [translate 0 250 $ Scale 0.2 0.2 $ Text $ show  $  "Jogador 1 " ++ (show w1) ++ "," ++ (show w2) ])
------------------------------------------------


{-| 'atualizaMovimenta' vai ser a função que aplica a função @movimenta@ e a função @atualiza@ a todos os carros do jogo. Vai primeiro aplicar a atualia a todos os carros, e so depois a movimenta-}

atualizaMovimenta :: Float -> Tempo -> Jogo -> [Acao] -> Jogo
atualizaMovimenta time t j [a,b,c,d]  | time <3 = j 
                                      | otherwise= novoJogo
                     where jogoAct = auxat t (j) 3 [a,b,aplicabot j,d]
                           Jogo map pr lc ln lp     = jogoAct
                           Mapa (pi,oi) tab               = map
                           [(b1,l1), (b2,l2), (b3,l3), (b4,l4)] = aplicacaminhocorreto posicoes lp
                           (pecas,orientacoes,posicoes) = separatePecaOrientacaoPosicao (listaposicoes pi oi tab 0 oi pi)
                           carrosAct= auxCar t tab lc 0 [l1,l2,l3,l4]  [b1,b2,b3,b4]
                           novoJogo = Jogo map pr carrosAct ln [l1,l2,l3,l4]

{-| A função 'aplicabot' vai devolver uma Acao, resultante da aplicaçãoda função bot a um Jogo. -}

aplicabot :: Jogo-> Acao
aplicabot j = bot 0.01 j 2

{-| A função 'auxat' vai aplicar recursivamente a função atualiza a todos os carro pertencentes a estado. -}

auxat ::Tempo -> Jogo -> Int -> [Acao] -> Jogo 
auxat t j 0 a = atualiza t j 0 (a !! 0) 
auxat t j i a  = auxat t (atualiza t j i (a !! i)) (i-1) a
                                
{-| A função 'auxCar' vai aplicar recursivamente a função movimenta a todos os carro pertencentes a estado. -}


auxCar :: Tempo -> Tabuleiro -> [Carro] -> Int -> [[Posicao]] -> [Bool] -> [Carro]
auxCar t tab lc (4) k bo = []
auxCar t tab lc i k bo       | (length k)<1 && (bo !! i) = (movimentacao (movimenta tab t (Carro x ang vel)) (tirarult (k !! i) ) ang : auxCar t tab lc (i+1) k bo) 
                             | (bo !! i) = (movimentacao (movimenta tab t (Carro x ang vel)) (tirarult (k !! i) ) ang : auxCar t tab lc (i+1) k bo) 
                             | otherwise = (movimentacao Nothing  (head(k !! i))  ang  : auxCar t tab lc (i+1) k bo)
                        where Carro x ang vel = ((!!) lc i) 
                              

{-| A função 'tirarult' vai devolver a posição onde o carro passará a estar caso a função movimenta der Nothing, dependendo do tamanho da lista de posições.-}
tirarult :: [Posicao] ->   Posicao
tirarult h | length h == 1 = head h
           | length h == 2 = head h 
           | length h  > 2 = head h

{-| A função 'aplicacaminhocorreto' vai verificar recursivamente se a posição de cada um dos jogadores é ou não uma posição válida. -}
aplicacaminhocorreto :: [Posicao] -> [[Posicao]] -> [(Bool,[Posicao])]
aplicacaminhocorreto l [] = []
aplicacaminhocorreto l (h:hs) = (caminhocorreto l h : aplicacaminhocorreto l hs)
                         
{-|  A função 'caminhocorreto'  vai verificar se uma posição é ou não correta e legal -}

caminhocorreto :: [Posicao] -> [Posicao] -> (Bool, [Posicao])
caminhocorreto lista [] = (True,[])
caminhocorreto lista (h:[]) = (True,(h:[]))
caminhocorreto lista hist | length hist <2 = (True,hist)
                          | (length hist) > (length lista)  = caminhocorreto lista (drop (length lista) hist )
                          | (head hist) == (lista !! ((length hist) -1)) = (True,  hist)
                          | (head hist) == (lista !! ((length hist) -2)) = (True, (tail hist))
                          | (head hist) == (lista !! (length hist))      = (True, [(lista !! (length hist))] ++ [(lista !! ((length hist) -1))] ++ (tail hist))
                          | (head hist) == (lista !! ((length hist) +1)) = (True, [(lista !! ((length hist) +1))] ++[(lista !! (length hist))] ++ [(lista !! ((length hist) -1))] ++ (tail hist))
                          | (head hist) == (lista !! ((length hist) +2)) = (True, [(lista !! ((length hist) +2))] ++[ (lista !! ((length hist) +1))] ++[(lista !! (length hist))] ++ [(lista !! ((length hist) -1))] ++ (tail hist))
                          | elem (head hist) (tail hist) = (True, tail hist)
                          | otherwise = (False, ((tail hist))) 


{-| A função 'window' vai ser a função que vai definir a tamanho e a posição inicial do jogo no ecrã-}

window :: Display
window = InWindow "micro" (700, 700) (0, 0)


{-| 'truncate' ' vai arredondar os numeros para um numero exato de casas decimais. -}
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

truncatef :: Float -> Int -> Float
truncatef x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n
{-|  Função que vai criar o jogo e vai chamar todas a funções necessárias ao funcionamento deste. -}

joga :: Estado -> IO ()
joga inicio = play
    window  -- tamanho da janela do jogo
    (greyN 0.5)                               -- côr do fundo da janela
    70                                     -- refresh rate
    inicio                                    -- estado inicial
    drawing                             -- função que desenha o estado do jogo
    reageEvento                               -- reage a um evento
    reageTempo                                -- reage ao passar do tempo

{-| Função principal que invoca o jogo. -}

main :: IO ()
main = do
    inicio <- estadoInicial 
    joga inicio

{-| Mapa do jogo -}
mapa4 = Mapa ((2,7),Este)[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                              ,[Peca Lava 0,Peca (Curva Norte) (-4),Peca Recta (-4),Peca Recta (-4),Peca Recta (-4),Peca Recta (-4),Peca Recta (-4),Peca Recta (-4),Peca (Curva Este) (-4),Peca Lava 0]
                              ,[Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-4),Peca Lava 0]
                              ,[Peca Lava 0,Peca Recta (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-4),Peca Lava 0]
                              ,[Peca Lava 0,Peca (Rampa Sul) (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-3),Peca Lava 0]
                              ,[Peca Lava 0,Peca (Rampa Sul) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0]
                              ,[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0]
                              ,[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca (Curva Sul) (-2),Peca Lava 0]
                              ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Mapa do jogo -}                             
mapa5 = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca (Curva Este) 2,Peca Recta 2,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 6,Peca Recta 6,Peca Recta 6,Peca Recta 6,Peca (Curva Este) 6,Peca Recta 2,Peca (Rampa Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Recta 6,Peca (Curva Norte) 4,Peca Recta 4,Peca (Curva Este) 4,Peca (Rampa Norte) 5,Peca Recta 2,Peca Recta 3,Peca Lava 0],[Peca Lava 0,Peca Recta 6,Peca (Rampa Norte) 3,Peca (Curva Norte) 4,Peca (Curva Sul) 4,Peca Recta 5,Peca Recta 2,Peca (Rampa Sul) 3,Peca Lava 0],[Peca Lava 0,Peca Recta 6,Peca Recta 3,Peca (Curva Oeste) 4,Peca (Rampa Este) 4,Peca (Curva Sul) 5,Peca Recta 2,Peca Recta 4,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) 5,Peca (Curva Oeste) 3,Peca Recta 3,Peca (Rampa Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Recta 4,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 5,Peca Recta 5,Peca Recta 5,Peca Recta 5,Peca (Rampa Oeste) 4,Peca Recta 4,Peca (Curva Sul) 4,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Mapa do jogo -}
mapa6 = Mapa ((4,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Oeste) (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Mapa do jogo -}
mapa1 = Mapa ((4,3), Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] 

{-| Mapa do jogo -}  
mapa7 = Mapa ((2,1), Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca (Curva Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Este) (-1),Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Mapa do jogo -}
mapa3 = Mapa ((9,9),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Mapa do jogo -}
mapa2 =        Mapa ((6,5),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca (Rampa Este) 2,Peca Recta 3,Peca (Rampa Este) 3,Peca Recta 4,Peca (Rampa Este) 4,Peca Recta 5,Peca (Rampa Este) 5,Peca Recta 6,Peca (Rampa Este) 6,Peca (Curva Este) 7,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca (Rampa Este) 2,Peca (Rampa Este) 3,Peca (Rampa Este) 4,Peca (Rampa Este) 5,Peca (Rampa Este) 6,Peca (Curva Sul) 7,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-2),Peca (Curva Este) (-2),Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca (Rampa Oeste) (-2),Peca (Rampa Oeste) (-3),Peca (Rampa Oeste) (-4),Peca (Curva Este) (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Curva Sul) (-2),Peca (Curva Norte) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-4),Peca (Curva Este) (-4),Peca Recta (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca Recta (-3),Peca (Curva Este) (-3),Peca Lava 0,Peca (Curva Oeste) (-4),Peca Recta (-4),Peca (Curva Sul) (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Curva Sul) (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-3),Peca Recta (-3),Peca (Rampa Oeste) (-4),Peca Recta (-4),Peca (Curva Sul) (-4),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-3),Peca (Curva Sul) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                       ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

{-| Função 'carrosjogo' vai definir a posição inicial de todos os carros consoante o Mapa a ser jogado. -}
                       
carrosjogo :: Jogo -> Jogo
carrosjogo j = Jogo map pr [Carro (((fromIntegral x) + 0.5),((fromIntegral y) + 0.5)) (pi/4) (0,0), Carro (((fromIntegral x) + 0.5),((fromIntegral y) + 0.5)) (pi/4) (0,0), Carro (((fromIntegral x) + 0.5),((fromIntegral y) + 0.5)) (pi/4) (0,0), Carro (((fromIntegral x) + 0.5),((fromIntegral y) + 0.5)) (pi/4) (0,0)] ln lp
                 where Jogo map pr lc ln lp     = j
                       Mapa ((x,y),o) tab       = map