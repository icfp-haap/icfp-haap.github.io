module SliderLib (Slider(..),SMove(..),sGetValue,sMove,sMoveAt,sLimitBetween,sGetPic,sChangeToValue)
where

import Graphics.Gloss

data Slider = Slider (Float,Float) -- valor min/max
                      Float         -- valor
                      Float         -- valor de incremento
                      deriving Show

data SMove = Sleft | Sright

{-|Extrai o valor de um slider.-}
sGetValue :: Slider -> Float
sGetValue (Slider _ x _) = x

{-|Move um slider para a esquerda ou direita.-}
sMove :: SMove -> Slider -> Slider
sMove move (Slider a x inc)
  = case move of
    Sleft  -> (Slider a (sLimitBetween (x-inc) a) inc)
    Sright -> (Slider a (sLimitBetween (x+inc) a) inc)

{-|Move um slider com um certo indice numa lista de sliders.-}
sMoveAt :: Int -> SMove -> [(Slider,String)] -> [(Slider,String)]
sMoveAt n move [] = []
sMoveAt 0 move ((slid,strin):xs) = ((sMove move slid),strin) : xs
sMoveAt n move (x:xs) = x:(sMoveAt (n-1) move xs)

{-|Limita o valor atual de um slider.-}
sLimitBetween :: Float -> (Float,Float) -> Float
sLimitBetween x (min,max)
  |x<min = min
  |x>max = max
  |otherwise = x

{-|Devolve a imagem de um slider.-}
sGetPic ::  (Float,Float) -- Posicao (Central)
          ->(Float,Float) -- Largura / Altura
          -> Slider -> String -> Picture
sGetPic (x,y) (w,h) (Slider sA sX _) string
  = Translate (x-(w/2)) y (Pictures [description,currentValue,baseLine,valueLine,minValLine,maxValLine])
  where
  baseLine    = Line [(0,0),(w,0)]
  valueLine   = Line [(mappedValue,-h/2),(mappedValue,h/2)]
  minValLine  = Line [(0,-h/3),(0,h/3)]
  maxValLine  = Line [(w,-h/3),(w,h/3)]
  showMappedval = Translate 0 (h/2) (scale (h/250) (h/250) (Text (show mappedValue)))
  currentValue= Translate 0 (h/2) (scale (h/250) (h/250) (Text (show sX)))
  description = Translate (w/2) (h/2) (scale (h/250) (h/250) (Text string))
  mappedValue = sMapTo sX sA (0,w)


{-|Mapeia um número entre um intervalo para um novo intervalo.-}
sMapTo :: Float -> (Float,Float) -> (Float,Float) -> Float
sMapTo x (min,max) (nmin,nmax)
  = nx
    where
    diff = (nmin) - (min)
    scale = (nmax-nmin)/(max-min)
    nx = scale*x + diff

{-|Altera o valor de um slider.-}
sChangeToValue :: Float -> Slider -> Slider
sChangeToValue n (Slider m _ i) = (Slider m (sLimitBetween n m) i)