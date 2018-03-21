module DrawingLib
(pecaToPicture,getScale,makeBoard,playerStatus,timePassed,makeBoardMidOffset,stunStatus,playerNitroStatus,dimensaoMapa,makeLava,scaleLava,makeLavaSprites,carrosToPicture,carroToPicture)
where

import LI11718
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap

{-|Devolve a imagem de uma peca.-}
pecaToPicture :: Peca -> Float -> Picture
pecaToPicture (Peca t h) scale
  = case t of
  Recta       -> Pictures [pReta,pSquare]
  Curva Norte -> Pictures [pCurva,pTriang]
  Rampa Norte -> Pictures [pRampa1,pRampa2,pRampa3,pSquare]
  Lava        -> Blank --Color black (Polygon [(0,0),(scale,0),(scale,-scale),(0,-scale) ])
  Curva Sul   -> Translate scale (-scale) (Rotate 180 (fun (Peca (Curva Norte) h) scale))
  Rampa Sul   -> Translate scale (-scale) (Rotate 180 (fun (Peca (Rampa Norte) h) scale))
  Curva Este  -> Translate scale 0        (Rotate 90  (fun (Peca (Curva Norte) h) scale))
  Rampa Este  -> Translate scale 0        (Rotate 90  (fun (Peca (Rampa Norte) h) scale))
  Curva Oeste -> Translate 0     (-scale) (Rotate 270 (fun (Peca (Curva Norte) h) scale))
  Rampa Oeste -> Translate 0     (-scale) (Rotate 270 (fun (Peca (Rampa Norte) h) scale))
  where
  pRampa1 = colorH (Polygon pathR1)
  pRampa2 = colorH1 (Polygon pathR2)
  pRampa3 = colorH1 (Polygon pathR3)
  pCurva  = colorH (Polygon pathC)
  pReta   = colorH (Polygon pathReta)
  pSquare = Color black (Line (pathReta ++  [(0,0)]))
  pTriang = Color black (Line (pathC ++ [(scale,0)]))
  pathR1  = [ (0,-scale), (scale/2,0), (scale,-scale) ]
  pathR2  = [ (0,-scale), (scale/2,0), (0,0) ]
  pathR3  = [ (scale,0), (scale/2,0), (scale,-scale) ]
  pathC   = [ (scale,0) ,(scale,-scale) ,(0,-scale) ]
  pathReta= [(0,0),(scale,0),(scale,-scale),(0,-scale) ]
  colorH  = Color (heigthToColor  h   )
  colorH1 = Color (heigthToColor (h+1))
  fun     = pecaToPicture

{-|Obtem a escala relativamente ao tamanho de um mapa.-}
getScale :: Mapa -> Float -> Float
getScale m l = scale
             where
             scale = l / (fromIntegral (max dimx dimy))
             (dimx,dimy) = dimensaoMapa m

{-|Obtem a imagem de um mapa com offset alterado-}
makeBoardMidOffset :: Mapa -> Float -> Picture
makeBoardMidOffset mapa scl
  = Translate xOffset yOffset (makeBoard mapa scl)
  where
  xOffset = -(fromIntegral dimx)/2*scl
  yOffset = (fromIntegral dimy)/2*scl
  (dimx,dimy) = dimensaoMapa mapa

{-|Obtem a imagem de um mapa.-}
makeBoard :: Mapa -> Float -> Picture
makeBoard (Mapa (pi,oi) (x:xs)) scale
  = Pictures (((makeBoardAc x xs scale 0 0) ++ [(endLine (pi,oi) scale)]))
  where
  makeBoardAc :: [Peca] -> [[Peca]] -> Float -> Float -> Float -> [Picture]
  makeBoardAc [] (h:t) scale x y = makeBoardAc h t  scale 0 (y-scale)
  makeBoardAc (h:t) ys scale x y = (Translate x y (pecaToPicture h scale)):makeBoardAc t ys scale (x+scale) y
  makeBoardAc [] [] scale x y = [Blank]
  lenX = (fromIntegral (length x))*scale
  lenY = (fromIntegral (length (x:xs)))*(-scale)
  p = Line [(0,0), (lenX,0), (lenX,lenY), (0,lenY), (0,0)]
  endLine :: (Posicao,Orientacao) -> Float -> Picture
  endLine ((x,y),ori) scl
    = let (nx,ny) = ((fromIntegral x)*scl,(fromIntegral y)*(-scl))
      in case ori of
        Norte -> Color green (Line [(nx,ny),(nx+scl,ny)])
        Este  -> Color green (Line [(nx,ny),(nx,ny-scl)])
        Sul   -> Color green (Line [(nx+scl,ny-scl),(nx,ny-scl)])
        Oeste -> Color green (Line [(nx+scl,ny-scl),(nx+scl,ny)])

{-|Produz uma imagem com lava.-}
makeLava :: Float -> Float -> Picture -> Picture
makeLava h scl lavaPic
  = Pictures (makeLavaAc h scl lavaPic 0 (scl))
  where
  makeLavaAc h scl lavaPic x y
    |h<=x       = makeLavaAc h scl lavaPic 0 (y-scl)
    |h< (abs y)-scl = [Blank]
    |otherwise  = (Translate x y lavaPic):(makeLavaAc h scl lavaPic (x+scl) y)

{-|Altera a imagem da lava dependente de uma escala.-}
scaleLava :: Float -> Picture -> Picture
scaleLava x p = Scale x x p

{-|Produz várias imagens com lava que servem para animação.-}
makeLavaSprites :: Picture -> Picture -> Int -> Float -> [Picture]
makeLavaSprites pista lava 0 scl = [(Pictures [lava,pista])]
makeLavaSprites pista lava n scl = ((Pictures [(Translate 0 ((fromIntegral n)*scl) lava),pista])):(makeLavaSprites pista lava (n-1) scl)

{-|Devolve a dimensão de um mapa.-}
dimensaoMapa :: Mapa -> (Int,Int)
dimensaoMapa (Mapa _ (x:xs))
  = (length x ,countCols (x:xs))
  where
  countCols []     = 0
  countCols (x:xs) = 1 + countCols xs

{-|Devolve a cor relativamente à altura de uma peca.-}
heigthToColor :: Altura -> Color
heigthToColor h 
  = let n = ((mapTo (fromIntegral h) (-10,10) (0,0.75)))
  in if h<0 then makeColor 0.6 n n 1
     else makeColor n n n 1

{-|Mapeia um número entre um intervalo para um novo intervalo.-}
mapTo :: Float -> (Float,Float) -> (Float,Float) -> Float
mapTo x (min,max) (nmin,nmax) = nmin + (nmax - nmin) * ((x - min) / (max - min));


{-|Retorna a imagem dos vários carros em jogo.-}
carrosToPicture :: [Carro] -> (Float,(Float,Float)) -> [Bool] -> Picture -> [Picture] -> [Picture]
carrosToPicture [] _ _ _ _= [Blank]
carrosToPicture (c:cs) f (nit:nits) nitImg (p:ps)
  = (carroToPicture c f nit nitImg p) : (carrosToPicture cs f nits nitImg ps) 

{-|Retorna a imagem de um dos carros em jogo.-}
carroToPicture :: Carro -> (Float,(Float,Float)) -> Bool -> Picture -> Picture -> Picture
carroToPicture (Carro (x,y) a _) (scl, (tX, tY) ) nit nitImg picCarro
  = Translate (tX+floatX*scl) (tY-floatY*scl) picCarroNormalized
  where
  floatX = realToFrac x
  floatY = realToFrac y
  floatA = -(realToFrac a)
  picCarroNormalized = Scale imgScl imgScl (rotate (floatA) nPicCarro)
  nPicCarro = if nit then Pictures [Translate (-20) 0 nitImg,picCarro]
              else picCarro
  imgScl = scl/carScl
  carScl = 64

{-|Retorna as imagens com a quantidade de nitro do jogador.-}
playerNitroStatus :: [Tempo] -> Tempo -> Float -> Picture
playerNitroStatus tts maxNitro baseScale
  = Translate (150*baseScale) (200*baseScale) (Pictures (playerNitroStatusH tts 0))
  where
  base1 = Polygon path1
  base2 = Translate (baseW*1.1*baseScale) 0 base1
  base3 = Translate (baseW/10*baseScale) (-baseH/2*baseScale) (Polygon path3)
  nitroBar x = Translate (baseW/10*baseScale) (-baseH/4*baseScale) (Polygon (pathNitro x))
  path1 = [(0,0),(baseW/10*baseScale,0),(baseW/10*baseScale,-baseH*baseScale),(0,-baseH*baseScale)]
  path3 = [(0,0),(baseW*baseScale,0),(baseW*baseScale,-baseH/2*baseScale),(0,-baseH/2*baseScale)]
  pathNitro x = [(0,0),(baseW*baseScale*x,0),(baseW*baseScale*x,-baseH/4*baseScale),(0,-baseH/4*baseScale)] 
  baseH = 30
  baseW = 200
  nitroColor 0 = Color red
  nitroColor 1 = Color blue
  nitroColor 2 = Color green
  nitroColor 3 = Color yellow
  makeNitroBar t mn n
    = nitroColor n (nitroBar (mapTo (realToFrac t) (0,realToFrac mn) (0,1)))
  showNitro t
    = Scale (0.1*baseScale) (0.1*baseScale) (Text (show ((fromIntegral (round (t*100)))/100)))
  playerNitroStatusH :: [Tempo] -> Int -> [Picture]
  playerNitroStatusH [] _ = [Blank]
  playerNitroStatusH (t:ts) n
    = (Translate 0 (-baseScale*100*(fromIntegral n)) (Pictures [base1,base2,base3,makeNitroBar t maxNitro n,showNitro t]))
      : (playerNitroStatusH ts (n+1))

{-|Retorna uma imagem com várias informações sobre os carros do jogadores.-}
playerStatus :: [Carro] -> Float -> Picture
playerStatus ccs baseScale
  = Translate (150*baseScale) (155*baseScale) (Pictures (playerStatusH ccs 0))
  where
  showCarro (Carro (x,y) ang (vx,vy))
    = Scale (0.1*baseScale) (0.1*baseScale) (Text (showPos++showAng++showVel))
    where
    show4 = showNdigits 4
    showPos = "Pos:"++ show4 x ++ "," ++ show4 y ++ " "
    showAng = "Ang:"++ show4 ang ++ " "
    showVel = "Vel:"++ show4 vx ++ "," ++ show4 vy
  playerStatusH [] n = [Blank]
  playerStatusH (c:cs) n = (Translate 0 (-baseScale*100*(fromIntegral n)) (showCarro c))
                            : (playerStatusH cs (n+1))

{-|Faz o mesmo do que a função show, mas limita a string a um certo tamanho.-}
showNdigits :: (Show a) => Int -> a -> String
showNdigits n a
  = showNdigitsH n (show a)
  where
  showNdigitsH _ []= []
  showNdigitsH 0 _ = []
  showNdigitsH n (x:xs) = x : (showNdigitsH (n-1) xs) 

{-|Tira os primeiros elementos de uma lista.-}
takeFirstN :: Int -> [a] -> [a]
takeFirstN 0 x = x
takeFirstN n (x:xs) = takeFirstN (n-1) xs

{-|Devolve a imagem com informação sobre os stuns dos jogadores.-}
stunStatus :: [Tempo] -> Float -> Picture
stunStatus tts baseScale
  = Translate (125*baseScale) (175*baseScale) (Pictures (stunStatus tts 0))
  where
  stunStatus [] _ = [Blank]
  stunStatus (t:ts) n = (getStunPic t n) : (stunStatus ts (n+1))
    where
    getStunPic t n = Translate 0 (-100*(fromIntegral n)*baseScale) (Scale (0.1*baseScale) (0.1*baseScale) (Text (showNdigits 3 t)))

{-|Imagem com o tempo que se passou desde o inicio da corrida.-}
timePassed :: Double -> Int -> Float -> Picture
timePassed frameStep frame baseScale
  = Translate 450 (-350) (Scale 0.3 0.3 timePic)
  where
  timePic = Text (showNdigits 5 (show ((fromIntegral frame)*frameStep)))