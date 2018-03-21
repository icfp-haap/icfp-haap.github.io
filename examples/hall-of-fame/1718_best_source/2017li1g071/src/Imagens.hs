module Imagens where
import LI11718
import Tarefa3_2017li1g71
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
-- | A função carregaNum carrega as Imagens de todos os números
carregaNum::IO ([Picture])
carregaNum = do
             {
              Just n0 <- loadJuicy "Pictures/Numeros/0.png";
              Just n1 <- loadJuicy "Pictures/Numeros/1.png";
              Just n2 <- loadJuicy "Pictures/Numeros/2.png";
              Just n3 <- loadJuicy "Pictures/Numeros/3.png";
              Just n4 <- loadJuicy "Pictures/Numeros/4.png";
              Just n5 <- loadJuicy "Pictures/Numeros/5.png";
              Just n6 <- loadJuicy "Pictures/Numeros/6.png";
              Just n7 <- loadJuicy "Pictures/Numeros/7.png";
              Just n8 <- loadJuicy "Pictures/Numeros/8.png";
              Just n9 <- loadJuicy "Pictures/Numeros/9.png";
              Just nD <- loadJuicy "Pictures/Numeros/Dot.png";
              return [n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,nD]
             }
-- | A função carregaCarros carrega as Imagens de todos os carros
carregaCarros::IO ([Picture])
carregaCarros = do
                {
                 Just cAl <- loadJuicy "Pictures/Carros/Alemanha.png";
                 Just cBr <- loadJuicy "Pictures/Carros/Brasil.png";
                 Just cEg <- loadJuicy "Pictures/Carros/Egipto.png";
                 Just cEs <- loadJuicy "Pictures/Carros/Espanha.png";
                 Just cFr <- loadJuicy "Pictures/Carros/França.png";
                 Just cIs <- loadJuicy "Pictures/Carros/Islandia.png";
                 Just cJp <- loadJuicy "Pictures/Carros/Japão.png";
                 Just cPt <- loadJuicy "Pictures/Carros/Portugal.png";
                 Just cRu <- loadJuicy "Pictures/Carros/Rússia.png";
                 Just cAs <- loadJuicy "Pictures/Carros/ArabiaSaudita.png";
                 Just cUr <- loadJuicy "Pictures/Carros/Uruguai.png";
                 Just cMa <- loadJuicy "Pictures/Carros/Marrocos.png";
                 Just cIr <- loadJuicy "Pictures/Carros/Irão.png";
                 Just cAu <- loadJuicy "Pictures/Carros/Australia.png";
                 Just cPr <- loadJuicy "Pictures/Carros/Peru.png";
                 Just cDn <- loadJuicy "Pictures/Carros/Dinamarca.png";
                 Just cAr <- loadJuicy "Pictures/Carros/Argentina.png";
                 Just cCr <- loadJuicy "Pictures/Carros/Croácia.png";
                 Just cNi <- loadJuicy "Pictures/Carros/Nigeria.png";
                 Just cSu <- loadJuicy "Pictures/Carros/Suiça.png";
                 Just cCo <- loadJuicy "Pictures/Carros/CostaRica.png";
                 Just cSe <- loadJuicy "Pictures/Carros/Servia.png";
                 Just cMe <- loadJuicy "Pictures/Carros/México.png";
                 Just cSc <- loadJuicy "Pictures/Carros/Suécia.png";
                 Just cCs <- loadJuicy "Pictures/Carros/CoreiaDoSul.png";
                 Just cBe <- loadJuicy "Pictures/Carros/Bélgica.png";
                 Just cPa <- loadJuicy "Pictures/Carros/Panama.png";
                 Just cTu <- loadJuicy "Pictures/Carros/Tunisia.png";
                 Just cIn <- loadJuicy "Pictures/Carros/Inglaterra.png";
                 Just cPo <- loadJuicy "Pictures/Carros/Polonia.png";
                 Just cSn <- loadJuicy "Pictures/Carros/Senegal.png";
                 Just cCl <- loadJuicy "Pictures/Carros/Colombia.png";
                 return [cAl,cBr,cEg,cEs,cFr,cIs,cJp,cPt,cRu,cAs,cUr,cMa,cIr,cAu,cPr,cDn,cAr,cCr,cNi,cSu,cCo,cSe,cMe,cSc,cCs,cBe,cPa,cTu,cIn,cPo,cSn,cCl]
                }
-- | A função carregaFloresta carrega as Imagens do tema Floresta
carregaFloresta::IO ([Picture])
carregaFloresta = do 
                  {
                   Just relva <- loadJuicy "Pictures/Floresta/Relva.png";
                   Just recta <- loadJuicy "Pictures/Floresta/Recta Madeira.jpg";
                   Just cN <- loadJuicy "Pictures/Floresta/CurvaNorteMadeira.png";
                   Just cS <- loadJuicy "Pictures/Floresta/CurvaSulMadeira.png";
                   Just cE <- loadJuicy "Pictures/Floresta/CurvaEsteMadeira.png";
                   Just cO <- loadJuicy "Pictures/Floresta/CurvaOesteMadeira.png";
                   return [relva,recta,cN,cS,cE,cO]
                  }
-- | A função carregaGelo carrega as Imagens do tema Gelo
carregaGelo::IO ([Picture])
carregaGelo = do
              {
               Just gelo <- loadJuicy "Pictures/Gelo/Gelo.jpg";
               Just neve <- loadJuicy "Pictures/Gelo/Neve.jpg";
               Just curv <- loadJuicy "Pictures/Gelo/CurvaNeve.png";
               return [gelo,neve,curv] 
              }
-- | A função carregaPraia carrega as Imagens do tema Praia
carregaPraia::IO ([Picture])
carregaPraia = do 
               {
                Just agua <- loadJuicy "Pictures/Praia/Agua.jpg";
                Just areia <- loadJuicy "Pictures/Praia/areia.jpg";
                Just curvaA <- loadJuicy "Pictures/Praia/CurvaAreia.png";
                return [agua,areia,curvaA]
               }
-- | A função carregaMenu carrega as Imagens escolhidas para o Menu
carregaMenu::IO ([Picture])
carregaMenu = do
              {
               Just menu <- loadJuicy "Pictures/Interações/Menu2.png";
               Just butão <- loadJuicy "Pictures/Interações/butão2.png";
               Just campanha <- loadJuicy "Pictures/Interações/Campanha2.png";
               Just seleção <- loadJuicy "Pictures/Interações/selecao.png";
               Just nitro <- loadJuicy "Pictures/Interações/nitro.png";
               Just venceu <- loadJuicy "Pictures/Interações/venceu.png";
               return [menu,butão,campanha,seleção,nitro,venceu]
              }
-- | A função carregaBandeiras carregas as Imagens das bandeiras
carregaBandeiras::IO ([Picture])
carregaBandeiras = do
                {
                 Just cAl <- loadJuicy "Pictures/Bandeiras/AlemanhaF.png";
                 Just cBr <- loadJuicy "Pictures/Bandeiras/BrasilF.png";
                 Just cEg <- loadJuicy "Pictures/Bandeiras/EgiptoF.png";
                 Just cEs <- loadJuicy "Pictures/Bandeiras/EspanhaF.png";
                 Just cFr <- loadJuicy "Pictures/Bandeiras/FrancaF.png";
                 Just cIs <- loadJuicy "Pictures/Bandeiras/IslandiaF.png";
                 Just cJp <- loadJuicy "Pictures/Bandeiras/JapaoF.png";
                 Just cPt <- loadJuicy "Pictures/Bandeiras/PortugalF.png";
                 Just cRu <- loadJuicy "Pictures/Bandeiras/RussiaF.png";
                 Just cAs <- loadJuicy "Pictures/Bandeiras/ArabiaSauditaF.png";
                 Just cUr <- loadJuicy "Pictures/Bandeiras/UruguaiF.png";
                 Just cMa <- loadJuicy "Pictures/Bandeiras/MarrocosF.png";
                 Just cIr <- loadJuicy "Pictures/Bandeiras/IraoF.png";
                 Just cAu <- loadJuicy "Pictures/Bandeiras/AustraliaF.png";
                 Just cPr <- loadJuicy "Pictures/Bandeiras/PeruF.png";
                 Just cDn <- loadJuicy "Pictures/Bandeiras/DinamarcaF.png";
                 Just cAr <- loadJuicy "Pictures/Bandeiras/ArgentinaF.png";
                 Just cCr <- loadJuicy "Pictures/Bandeiras/CroaciaF.png";
                 Just cNi <- loadJuicy "Pictures/Bandeiras/NigeriaF.png";
                 Just cSu <- loadJuicy "Pictures/Bandeiras/SuicaF.png";
                 Just cCo <- loadJuicy "Pictures/Bandeiras/CostaRicaF.png";
                 Just cSe <- loadJuicy "Pictures/Bandeiras/ServiaF.png";
                 Just cMe <- loadJuicy "Pictures/Bandeiras/MexicoF.png";
                 Just cSc <- loadJuicy "Pictures/Bandeiras/SueciaF.png";
                 Just cCs <- loadJuicy "Pictures/Bandeiras/CoreiaDoSulF.png";
                 Just cBe <- loadJuicy "Pictures/Bandeiras/BelgicaF.png";
                 Just cPa <- loadJuicy "Pictures/Bandeiras/PanamaF.png";
                 Just cTu <- loadJuicy "Pictures/Bandeiras/TunisiaF.png";
                 Just cIn <- loadJuicy "Pictures/Bandeiras/InglaterraF.png";
                 Just cPo <- loadJuicy "Pictures/Bandeiras/PoloniaF.png";
                 Just cSn <- loadJuicy "Pictures/Bandeiras/SenegalF.png";
                 Just cCl <- loadJuicy "Pictures/Bandeiras/ColombiaF.png";
                 return [cAl,cBr,cEg,cEs,cFr,cIs,cJp,cPt,cRu,cAs,cUr,cMa,cIr,cAu,cPr,cDn,cAr,cCr,cNi,cSu,cCo,cSe,cMe,cSc,cCs,cBe,cPa,cTu,cIn,cPo,cSn,cCl]
                }
-- | A função carregaLetras carrega as imagens das letras para o jogo
carregaLetras::IO ([Picture])
carregaLetras = do
                {
                 Just a <- loadJuicy "Pictures/Letras/a.png";
                 Just b <- loadJuicy "Pictures/Letras/b.png";
                 Just c <- loadJuicy "Pictures/Letras/c.png";
                 Just d <- loadJuicy "Pictures/Letras/d.png"; 
                 Just e <- loadJuicy "Pictures/Letras/e.png"; 
                 Just f <- loadJuicy "Pictures/Letras/f.png"; 
                 Just g <- loadJuicy "Pictures/Letras/g.png"; 
                 Just h <- loadJuicy "Pictures/Letras/h.png"; 
                 Just i <- loadJuicy "Pictures/Letras/i.png"; 
                 Just j <- loadJuicy "Pictures/Letras/j.png"; 
                 Just k <- loadJuicy "Pictures/Letras/k.png"; 
                 Just l <- loadJuicy "Pictures/Letras/l.png"; 
                 Just m <- loadJuicy "Pictures/Letras/m.png"; 
                 Just n <- loadJuicy "Pictures/Letras/n.png"; 
                 Just o <- loadJuicy "Pictures/Letras/o.png"; 
                 Just p <- loadJuicy "Pictures/Letras/p.png"; 
                 Just q <- loadJuicy "Pictures/Letras/q.png"; 
                 Just r <- loadJuicy "Pictures/Letras/r.png"; 
                 Just s <- loadJuicy "Pictures/Letras/s.png"; 
                 Just t <- loadJuicy "Pictures/Letras/t.png"; 
                 Just u <- loadJuicy "Pictures/Letras/u.png"; 
                 Just v <- loadJuicy "Pictures/Letras/v.png"; 
                 Just w <- loadJuicy "Pictures/Letras/w.png"; 
                 Just x <- loadJuicy "Pictures/Letras/x.png"; 
                 Just y <- loadJuicy "Pictures/Letras/y.png"; 
                 Just z <- loadJuicy "Pictures/Letras/z.png";
                 return [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
                }
-- | A função ajustaCorR ajusta a cor de uma reta dada a altura, a picture, e o tema ao qual pertence a picture
ajustaCorR::Int->Picture->Int->Picture
ajustaCorR n c t = case t of
                   1 -> if n>=0
                        then Pictures [c,color (makeColor 0 0 1 (toEnum n/10)) imgRecta]
                        else Pictures [c,color (makeColor 1 0 0 (toEnum (abs n)/10)) imgRecta]
                   _ -> if n>=0
                        then Pictures [c,color (makeColor 1 1 1 (toEnum n/10)) imgRecta]
                        else Pictures [c,color (makeColor 0 0 0 (toEnum (abs n)/10)) imgRecta]
-- | A função ajustaCorS ajusta a cor da parte triangular de uma rampa dada a altura, o tema e a orientação
ajustaCorS::Int->Int->Orientacao->Picture
ajustaCorS n t o= case t of
                     1 -> if n>=0
                          then case o of
                               Norte -> rotate 0 (color (makeColor 0 0 1 ((toEnum n+1)/10)) imgSobe)
                               Sul -> rotate 0 (color (makeColor 0 0 1 (toEnum n/10)) imgSobe)
                               Este -> rotate 270 (color (makeColor 0 0 1 (toEnum n/10)) imgSobe)
                               Oeste -> rotate 270 (color (makeColor 0 0 1 ((toEnum n+1)/10)) imgSobe)
                          else case o of
                               Norte -> rotate 0 (color (makeColor 1 0 0 ((toEnum (abs n)-1)/10)) imgSobe)
                               Sul -> rotate 0 (color (makeColor 1 0 0 (toEnum (abs n)/10)) imgSobe)
                               Este -> rotate 270 (color (makeColor 1 0 0 (toEnum (abs n)/10)) imgSobe)
                               Oeste -> rotate 270 (color (makeColor 1 0 0 ((toEnum (abs n)-1)/10)) imgSobe)
                     _ -> if n>=0
                          then case o of
                               Norte -> rotate 0 (color (makeColor 1 1 1 ((toEnum n+1)/10)) imgSobe)
                               Sul -> rotate 0 (color (makeColor 1 1 1 (toEnum n/10)) imgSobe)
                               Este -> rotate 270 (color (makeColor 1 1 1 (toEnum n/10)) imgSobe)
                               Oeste -> rotate 270 (color (makeColor 1 1 1 ((toEnum n+1)/10)) imgSobe)
                          else case o of
                               Norte -> rotate 0 (color (makeColor 0 0 0 ((toEnum (abs n)-1)/10)) imgSobe)
                               Sul -> rotate 0 (color (makeColor 0 0 0 (toEnum (abs n)/10)) imgSobe)
                               Este -> rotate 270 (color (makeColor 0 0 0 (toEnum (abs n)/10)) imgSobe)
                               Oeste -> rotate 270 (color (makeColor 0 0 0 ((toEnum (abs n)-1)/10)) imgSobe)
-- | A função ajustaCorL ajusta a cor das partes laterais de uma rampa dada a altura, o tema e a orientação
ajustaCorL::Int->Int->Orientacao->Picture
ajustaCorL a t o = case t of
                   1 -> if a>=0
                        then case o of
                               Norte -> rotate 0 (color (makeColor 0 0 1 (toEnum a/10)) imgLaterais)
                               Sul -> rotate 0 (color (makeColor 0 0 1 (toEnum (a+1)/10)) imgLaterais)
                               Este -> rotate 270 (color (makeColor 0 0 1 (toEnum (a+1)/10)) imgLaterais)
                               Oeste -> rotate 270 (color (makeColor 0 0 1 (toEnum a/10)) imgLaterais)
                        else case o of
                               Norte -> rotate 0 (color (makeColor 1 0 0 (toEnum (abs a)/10)) imgLaterais)
                               Sul -> rotate 0 (color (makeColor 1 0 0 ((toEnum (abs a)-1)/10)) imgLaterais)
                               Este -> rotate 270 (color (makeColor 1 0 0 ((toEnum (abs a)-1)/10)) imgLaterais)
                               Oeste -> rotate 270 (color (makeColor 1 0 0 (toEnum (abs a)/10)) imgLaterais)
                   _ -> if a>=0
                        then case o of
                               Norte -> rotate 0 (color (makeColor 1 1 1 (toEnum a/10)) imgLaterais)
                               Sul -> rotate 0 (color (makeColor 1 1 1 (toEnum (a+1)/10)) imgLaterais)
                               Este -> rotate 270 (color (makeColor 1 1 1 (toEnum (a+1)/10)) imgLaterais)
                               Oeste -> rotate 270 (color (makeColor 1 1 1 (toEnum a/10)) imgLaterais)
                        else case o of
                               Norte -> rotate 0 (color (makeColor 0 0 0 (toEnum (abs a)/10)) imgLaterais)
                               Sul -> rotate 0 (color (makeColor 0 0 0 ((toEnum (abs a)-1)/10)) imgLaterais)
                               Este -> rotate 270 (color (makeColor 0 0 0 ((toEnum (abs a)-1)/10)) imgLaterais)
                               Oeste -> rotate 270 (color (makeColor 0 0 0 (toEnum (abs a)/10)) imgLaterais)
-- | A função ajustaCorC ajusta a cor de uma curva dada a altura, a picture, a orientação e o tema da picture
ajustaCorC::Int->Picture->Orientacao->Int->Picture
ajustaCorC n c o t = case t of
                  1 -> if n>=0
                       then rotate (toAng o) (color (makeColor 0 0 1 (toEnum n/10))  imgCur)
                       else rotate (toAng o) (color (makeColor 1 0 0 (toEnum (abs n)/10))  imgCur)
                  _ -> if n>=0
                       then rotate (toAng o) (color (makeColor 1 1 1 (toEnum n/10)) imgCur)
                       else rotate (toAng o) (color (makeColor 0 0 0 (toEnum (abs n)/10)) imgCur)
-- | Passa orientações para os seus respetivos angulos
toAng::Orientacao->Float
toAng o = case o of
          Norte -> 0
          Sul -> 180
          Este -> 90
          Oeste -> 270

-- | Imagem de uma Recta
imgRecta::Picture
imgRecta = Polygon [(-50,-50),(-50,50),(50,50),(50,-50)]
-- | Imagem de uma Rampa
imgSobe::Picture
imgSobe = Polygon [(-50,50),(50,50),(0,-50)]

imgLaterais::Picture
imgLaterais = Pictures [Polygon [(0,-50),(-50,-50),(-50,50)],Polygon [(0,-50),(50,-50),(50,50)]]
-- | Imagem da parte Triangular da Curva
imgCur::Picture
imgCur = Polygon [(51,51),(-51,-51),(51,-51)]
-- | Junção da parte triangular da curva com uma peça lava para formar a imagem de uma curva
imgCurva::Picture->Picture->Picture
imgCurva imgLava imgCur = Pictures [imgLava,imgCur]
-- | A função fazRampa faz o desenho de uma rampa com a respetiva altura, dada a orientação da rampa, a imagem de fundo, a altura e o tema
fazRampa::Orientacao->Picture->Int->Int->Picture
fazRampa o p n t = Pictures [p,(ajustaCorL n t o),(ajustaCorS n t o)]
-- | A função desenhaBandeirasGS desenha as bandeiras na fase de grupos da campanha
desenhaBandeirasGS::[Int]->[Picture]->Int->(Float,Float)->[Picture]
desenhaBandeirasGS [] _ _ _ = []
desenhaBandeirasGS (hGS:tGS) imgBand aj (x,y) = if (mod aj 5 == 0)
                                                then (Translate x y inv):desenhaBandeirasGS (hGS:tGS) imgBand (aj+1) (x+24,y)
                                                else (Translate x y (scale 0.26 0.26 imgAdes)):desenhaBandeirasGS tGS imgBand (aj+1) (x+26, y)
                                              where inv::Picture
                                                    inv = color (makeColorI 0 0 0 0) (rectangleSolid 24 24)
                                                    imgAdes = scale 0.2 0.2 (imgBand!!hGS)
-- | A função desenhaBandeirasGS desenha as bandeiras nos quartos de final da campanha
desenhaBandeirasQF::[Int]->[Picture]->Int->(Float,Float)->[Picture]
desenhaBandeirasQF [] _ _ _ = []
desenhaBandeirasQF (hGS:tGS) imgBand aj (x,y) = if (mod aj 5 == 0)
                                                then (Translate x y inv):desenhaBandeirasQF (hGS:tGS) imgBand (aj+1) (x+52,y)
                                                else (Translate x y (scale 0.53 0.53 imgAdes)):desenhaBandeirasQF tGS imgBand (aj+1) (x+53, y)
                                              where inv::Picture
                                                    inv = color (makeColorI 0 0 0 0) (rectangleSolid 52 52)
                                                    imgAdes = scale 0.2 0.2 (imgBand!!hGS)
-- | A função desenhaBandeirasGS desenha as bandeiras na meia final da campanha                                                    
desenhaBandeirasSF::[Int]->[Picture]->Int->(Float,Float)->[Picture]
desenhaBandeirasSF [] _ _ _ = []
desenhaBandeirasSF (hGS:tGS) imgBand aj (x,y) = if (mod aj 5 == 0)
                                                then (Translate x y inv):desenhaBandeirasSF (hGS:tGS) imgBand (aj+1) (x+112,y)
                                                else (Translate x y (scale 1.11 1.11 imgAdes)):desenhaBandeirasSF tGS imgBand (aj+1) (x+111, y)
                                              where inv::Picture
                                                    inv = color (makeColorI 0 0 0 0) (rectangleSolid 112 112)
                                                    imgAdes = scale 0.2 0.2 (imgBand!!hGS)
-- | A função desenhaBandeirasGS desenha as bandeiras na fase final da campanha
desenhaBandeirasF::[Int]->[Picture]->Int->(Float,Float)->[Picture]
desenhaBandeirasF [] _ _ _ = []
desenhaBandeirasF (hGS:tGS) imgBand aj (x,y) = (Translate x y (scale 2.5 2.5 imgAdes)):desenhaBandeirasF tGS imgBand (aj+1) (x+250, y)
                                             where imgAdes = scale 0.2 0.2 (imgBand!!hGS)
-- | A função stringToPic passa uma string para uma lista de pictures com uma dada fonte dada por uma lista de pictures com as letras dessa respetiva fonte
stringToPic::[Picture]->String->(Float,Float)->[Picture]
stringToPic _ [] _ = []
stringToPic lLetras (h:t) (x,y) = (Translate x y (lLetras!!(removeJust (elemIndex h ['a'..'z'])))):stringToPic lLetras t (x+50,y)
-- | Desenha o final de cada fase do modo campanha
desenhaFim::[Int]->[Int]->[Picture]->[Picture]->[Picture]->[Picture]
desenhaFim (h:t) lisA band letras fundo = [fundo!!5,scale 1.1 1.1 (band!!(lisA!!h)),Pictures (stringToPic letras "vencedor" (-185,-400))]
-- | função responsável por desenhar todas as bandeiras na tabela do modo campanha
quantasBandeiras::[[Int]]->[Picture]->[Picture]
quantasBandeiras [x] band =  [(Pictures (desenhaBandeirasGS x band 1 (-488,-488)))]
quantasBandeiras [y,x] band  = [(Pictures (desenhaBandeirasGS x band 1 (-488,-488))),(Pictures (desenhaBandeirasQF y band 1 (-474,-388)))]
quantasBandeiras [z,y,x] band = [(Pictures (desenhaBandeirasGS x band 1 (-488,-488))),(Pictures (desenhaBandeirasQF y band 1 (-474,-388))),(Pictures (desenhaBandeirasSF z band 1 (-446,-288)))]
quantasBandeiras [w,z,y,x] band = [(Pictures (desenhaBandeirasGS x band 1 (-488,-488))),(Pictures (desenhaBandeirasQF y band 1 (-474,-388))),(Pictures (desenhaBandeirasSF z band 1 (-446,-88))),(Pictures (desenhaBandeirasF w band 1 (-375,188)))]

