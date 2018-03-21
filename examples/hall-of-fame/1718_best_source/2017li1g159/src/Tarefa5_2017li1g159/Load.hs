{-|
Module : Load
Description : Este módulo serve para auxiliar a Tarefa5.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

O objetivo deste módulo é o de inicializar os tipos que conterão todas as imagens que
vamos usar no jog.
-}
module Load where

import Graphics.Gloss

data Board  = Board   { space :: Picture,
                        recta :: [Picture],
                        start :: Picture,
                        curvN :: [Picture],
                        curvS :: [Picture],
                        curvE :: [Picture],
                        curvO :: [Picture],
                        rampN :: [Picture],
                        rampS :: [Picture],
                        rampE :: [Picture],
                        rampO :: [Picture] 
                        }

data Vehicle = Car    { vermelho   :: Picture,
                        verde      :: Picture,
                        azul       :: Picture,
                        amarelo    :: Picture,

                        vermelhon  :: Picture,
                        verden     :: Picture,
                        azuln      :: Picture,
                        amarelon   :: Picture

                        }

data Icon = Icon      { helmVermelho :: Picture,
                        helmVerde    :: Picture,
                        helmAzul     :: Picture,
                        helmAmarelo  :: Picture,
                        helmCinz     :: Picture,

                        nVermelho    :: Picture,
                        nVerde       :: Picture,
                        nAzul        :: Picture,
                        nAmarelo     :: Picture,
                        nCinz        :: Picture,

                        map1         :: Picture,
                        map2         :: Picture,
                        voltar       :: Picture,
                        symbol       :: Picture,
                        playb        :: Picture,
                        exitb        :: Picture,

                        win          :: Picture,
                        lose         :: Picture,

                        do1          :: Picture,
                        do2          :: Picture,
                        do3          :: Picture,
                        dogo         :: Picture
                        }

-- Load Images 

-------------------------------------
-- Icons
playbutton = loadBMP "playbutton.bmp"
exitbutton = loadBMP "exitbutton.bmp"
micro = loadBMP "micro.bmp"
menu2map1 = loadBMP "EscolherMapa1.bmp"
menu2map2 = loadBMP "EscolherMapa2.bmp"
menu2volt = loadBMP "EscolherVoltar.bmp"

win_p  = loadBMP "YOU_WIN.bmp"
lose_p = loadBMP "YOU_LOSE.bmp"
--count-----------------------------
count1 = loadBMP "1.bmp"
count2 = loadBMP "2.bmp"
count3 = loadBMP "3.bmp"
countgo = loadBMP "GO.bmp"
-------------------------------------
-- Cars
car1 = loadBMP "NAVE1RED.bmp"
car2 = loadBMP "NAVE1BLUE.bmp"
car3 = loadBMP "NAVE1GREEN.bmp"
car4 = loadBMP "NAVE1YELLOW.bmp"

car1n = loadBMP "NAVE1REDN.bmp"
car2n = loadBMP "NAVE1BLUEN.bmp"
car3n = loadBMP "NAVE1GREENN.bmp"
car4n = loadBMP "NAVE1YELLOWN.bmp"
-- Capacete
capaceteP1 = loadBMP "capaceteP1.bmp"
capaceteP2 = loadBMP "capaceteP2.bmp"
capaceteP3 = loadBMP "capaceteP3.bmp"
capaceteP4 = loadBMP "capaceteP4.bmp"
capaceteP0 = loadBMP "capaceteP0.bmp"

botijaP1 = loadBMP "BotijaVermelha.bmp"
botijaP2 = loadBMP "BotijaAzul.bmp"
botijaP3 = loadBMP "BotijaVerde.bmp"
botijaP4 = loadBMP "BotijaAmarela.bmp"
botijaP0 = loadBMP "BotijaPreta.bmp"
-------------------------------------
-- Lava
lava = loadBMP "lava.bmp"
-------------------------------------
-- Curva
curvaSul_0 = loadBMP "CurvaSul-0.bmp"
curvaSul_1 = loadBMP "CurvaSul-1.bmp"
curvaSul_2 = loadBMP "CurvaSul-2.bmp"
curvaSul_3 = loadBMP "CurvaSul-3.bmp"
curvaSul_4 = loadBMP "CurvaSul-4.bmp"


curvaNorte_0 = loadBMP "CurvaNorte-0.bmp"
curvaNorte_1 = loadBMP "CurvaNorte-1.bmp"
curvaNorte_2 = loadBMP "CurvaNorte-2.bmp"
curvaNorte_3 = loadBMP "CurvaNorte-3.bmp"
curvaNorte_4 = loadBMP "CurvaNorte-4.bmp"


curvaOeste_0 = loadBMP "CurvaOeste-0.bmp"
curvaOeste_1 = loadBMP "CurvaOeste-1.bmp"
curvaOeste_2 = loadBMP "CurvaOeste-2.bmp"
curvaOeste_3 = loadBMP "CurvaOeste-3.bmp"
curvaOeste_4 = loadBMP "CurvaOeste-4.bmp"

curvaEste_0 = loadBMP "CurvaEste-0.bmp"
curvaEste_1 = loadBMP "CurvaEste-1.bmp"
curvaEste_2 = loadBMP "CurvaEste-2.bmp"
curvaEste_3 = loadBMP "CurvaEste-3.bmp"
curvaEste_4 = loadBMP "CurvaEste-4.bmp"


-------------------------------------
-- Rampas
rampaNorte_4 = loadBMP "RampaNorte-3.bmp"
rampaNorte_0 = loadBMP "RampaNorte-0.bmp"
rampaNorte_1 = loadBMP "RampaNorte-0.bmp"
rampaNorte_2 = loadBMP "RampaNorte-1.bmp"
rampaNorte_3 = loadBMP "RampaNorte-2.bmp"

rampaSul_4 = loadBMP "RampaSul-3.bmp"
rampaSul_0 = loadBMP "RampaSul-0.bmp"
rampaSul_1 = loadBMP "RampaSul-0.bmp"
rampaSul_2 = loadBMP "RampaSul-1.bmp"
rampaSul_3 = loadBMP "RampaSul-2.bmp"

rampaEste_4 = loadBMP "RampaEste-3.bmp"
rampaEste_0 = loadBMP "RampaEste-0.bmp"
rampaEste_1 = loadBMP "RampaEste-0.bmp"
rampaEste_2 = loadBMP "RampaEste-1.bmp"
rampaEste_3 = loadBMP "RampaEste-2.bmp"

rampaOeste_4 = loadBMP "RampaOeste-3.bmp"
rampaOeste_0 = loadBMP "RampaOeste-0.bmp"
rampaOeste_1 = loadBMP "RampaOeste-0.bmp"
rampaOeste_2 = loadBMP "RampaOeste-1.bmp"
rampaOeste_3 = loadBMP "RampaOeste-2.bmp"

------------------------------------
-- Rectas
partidaPeca = loadBMP "Partida.bmp"

reta_0 = loadBMP "reta-0.bmp"
reta_1 = loadBMP "reta-1.bmp"
reta_2 = loadBMP "reta-2.bmp"
reta_3 = loadBMP "reta-3.bmp"
reta_4 = loadBMP "reta-4.bmp"

loadBoard :: IO Board
loadBoard = do 
            a <- lava

            b0 <- reta_0
            b1 <- reta_1
            b2 <- reta_2
            b3 <- reta_3
            b4 <- reta_4

            c <- partidaPeca

            d0 <- curvaNorte_0
            d1 <- curvaNorte_1
            d2 <- curvaNorte_2
            d3 <- curvaNorte_3
            d4 <- curvaNorte_4

            e0 <- curvaSul_0
            e1 <- curvaSul_1
            e2 <- curvaSul_2
            e3 <- curvaSul_3
            e4 <- curvaSul_4

            f0 <- curvaEste_0
            f1 <- curvaEste_1
            f2 <- curvaEste_2
            f3 <- curvaEste_3
            f4 <- curvaEste_4

            g0 <- curvaOeste_0
            g1 <- curvaOeste_1
            g2 <- curvaOeste_2
            g3 <- curvaOeste_3
            g4 <- curvaOeste_4
            ------------------------------------------
            h0 <- rampaNorte_0
            h1 <- rampaNorte_1
            h2 <- rampaNorte_2
            h3 <- rampaNorte_3
            h4 <- rampaNorte_4

            i0 <- rampaSul_0
            i1 <- rampaSul_1
            i2 <- rampaSul_2
            i3 <- rampaSul_3
            i4 <- rampaSul_4

            j0 <- rampaEste_0
            j1 <- rampaEste_1
            j2 <- rampaEste_2
            j3 <- rampaEste_3
            j4 <- rampaEste_4

            k0 <- rampaOeste_0
            k1 <- rampaOeste_1
            k2 <- rampaOeste_2
            k3 <- rampaOeste_3
            k4 <- rampaOeste_4

            --------------------------------------------
            let rampas = [[h0,h1,h2,h3,h4],[i0,i1,i2,i3,i4],[j0,j1,j2,j3,j4],[k0,k1,k2,k3,k4] ]
            let retas  = [b0,b1,b2,b3,b4]
            let curvas = [[d0,d1,d2,d3,d4],[e0,e1,e2,e3,e4],[f0,f1,f2,f3,f4],[g0,g1,g2,g3,g4] ]
            let x      = [a,c] 
            return (subFill x curvas retas rampas ) 

subFill ::[Picture] -> [[Picture]] -> [Picture] -> [[ Picture ]] ->Board
subFill [a,c] [d,e,f,g] b [h,i,j,k] =Board{ space = a,
                                        recta = b,
                                        start = c,
                                        curvN = d,
                                        curvS = e,
                                        curvE = f,
                                        curvO = g,
                                        rampN = h,
                                        rampS = i,
                                        rampE = j,
                                        rampO = k  
                                        }

loadVehicle :: IO Vehicle
loadVehicle = do
              a <- car1 
              b <- car2
              c <- car3
              d <- car4

              e <- car1n 
              f <- car2n
              g <- car3n
              h <- car4n
              let x = [a,c,b,d,e,f,g,h]
              return (subV x)

subV :: [Picture] -> Vehicle
subV [a,b,c,d,e,f,g,h] =Car{ vermelho   = a ,
                     verde     = b ,
                     azul      = c ,
                     amarelo   = d,

                     vermelhon = e ,
                     verden    = g ,
                     azuln     = f ,
                     amarelon  = h 
                     }

loadIcon :: IO Icon
loadIcon = do
           a <- capaceteP1 
           b <- capaceteP2 
           c <- capaceteP3 
           d <- capaceteP4 
           m <- capaceteP0

           r <- botijaP1
           s <- botijaP2
           t <- botijaP3
           u <- botijaP4
           v <- botijaP0

           e <- micro
           f <- playbutton
           g <- exitbutton

           w0 <- win_p
           l0 <- lose_p

           j <- menu2map1
           k <- menu2map2
           l <- menu2volt

           n <- count1
           o <- count2
           p <- count3
           q <- countgo
           
           let x = [a,b,c,d,e,f,g,j,k,l,m,n,o,p,q,r,s,t,u,v,w0,l0]
           return (subI x)

subI :: [Picture] -> Icon
subI [a,b,c,d,e,f,g,j,k,l,m,n,o,p,q,r,s,t,u,v,w0,l0] =Icon{ helmVermelho = a,
                            helmVerde    = b,
                            helmAzul     = c,
                            helmAmarelo  = d,
                            helmCinz     = m,
                            nVermelho    = r,
                            nVerde       = s,
                            nAzul        = t,
                            nAmarelo     = u,
                            nCinz        = v,
                            win          = w0,
                            lose         = l0,
                            symbol       = e,
                            map1         = j,
                            map2         = k,
                            voltar       = l,
                            do1          = n,
                            do2          = o,
                            do3          = p,
                            dogo         = q,
                            playb        = f,
                            exitb        = g 
                            }

{-
    
-}


