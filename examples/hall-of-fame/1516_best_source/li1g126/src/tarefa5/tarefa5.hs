 ---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------          TAREFA 5             ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Gloss
Description : Módulo para calcular as dimensões de uma picture.
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo que __permite__ determinar as  dimensões do menor retângulo 
envolvente de uma /Picture/. Este módulo
recebe uma /Picture/ e retorna a __altura__ e a __largura__ respetivamente 
do retângulo formado em torno da /Picture/.
-}

module Main where

import Data.List
import qualified Data.Text as T
import GlossExtras
import Graphics.Gloss
import Unsafe.Coerce
import Test.HUnit

{- Type-class for angles.

class Angle a where
  sine    :: (Floating x) => a x -> x
  cosine  :: (Floating x) => a x -> x
  tangent :: (Floating x) => a x -> x

  arcsine    :: (Floating x) => x -> a x
  arccosine  :: (Floating x) => x -> a x
  arctangent :: (Floating x) => x -> a x

-}

-- | Ângulo em graus.
type Degree = Float
-- | Ângulo em radianos.
type Radian = Float
-- | Coordenada do tipo (x,y).
type Coord  = (Float,Float)
-- | Coordenada polar do tipo (d , alpha).
type CoordPolar = (Float,Radian)



---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Função /Main/.
main = do inp <- getContents 
          let (x,y) = tarefa5 (readPicture inp)
          putStrLn (show x ++ " " ++ show y)

-- | Função que retorna a __altura__ e a __largura__ respetivamente 
--   do retângulo formado em torno de uma __/Picture/__.
tarefa5 :: Picture -> (Int, Int)
tarefa5 n = if coords n == [] 
            then (0,0)
            else let [(x1,y1),(x2,y2)] = calcExtrem $ coords n
                 in ( round (abs(x1-x2) ) , round (abs(y1-y2)) )



---------------------------------------------------------------------
-- * __Calcular as coordenadas dos pontos do rectângulo que envolve cada /Picture/__
---------------------------------------------------------------------


-- | Função que calcula individualmente os pontos do rectângulo
--   que envolve cada uma das __/Picture/__.
coords :: Picture -> [Coord]
coords p = case p of
        (Circle r)                   -> pointsCircle r
        (Polygon x)                  -> x
        (Line x)                     -> x
        (Blank)                      -> []
        (Bitmap x y _ _)             -> [a,b,c,d]
                                         where a = ( (fromIntegral (-x))*0.5 , (fromIntegral y)   *0.5 )
                                               b = ( (fromIntegral (x)) *0.5 , (fromIntegral (-y))*0.5 )
                                               c = ( (fromIntegral (-x))*0.5 , (fromIntegral (-y))*0.5 )
                                               d = ( (fromIntegral (x)) *0.5 , (fromIntegral (y)) *0.5 )
        (Pictures n)                 -> separa (Pictures n)
        (Color x a)                  -> coords a
        (Translate x y p)            -> translatePic x y p
        (Scale x y a)                -> scalePic x y (coords a)
        (Rotate a e)                 -> rotation a (coords e)
        _                            -> []



-- | Caso o __/input/__ receba um __/Pictures/__, a função @'separa'@ devolve uma lista com as diferentes coordenadas do
--  de cada retângulo envolvente da _/Picture/__.
separa :: Picture -> [Coord]
separa (Pictures []) = []
separa (Pictures n) = concat $ map coords n


-- | Função que fornece as coordenadas dos __extremos__ (extremo superior esquerdo
--   e inferior direito respetivamente) de um retângulo formado 
--   em volta de uma /Picture/.
calcExtrem :: (Ord a, Ord b) => (Num a, Num b) => [(a, b)] -> [(a, b)]
calcExtrem [] = []
calcExtrem l = [((minimum (map fst l)),(maximum (map snd l))) , ((maximum (map fst l)),(minimum (map snd l)))]




-- | Função que determina as coordenadas finais após um __Scale__. 
scalePic :: Float -> Float -> [Coord] -> [Coord]
scalePic _ _ [] = []
scalePic x y ((a,b) : t) = ((a*x),(b*y)) : (scalePic x y t)


-- | Função que determina as coordenadas finais após um __Translate__ @x y@.
translatePic :: Float -> Float -> Picture -> [Coord]
translatePic x y p = if coords p == []
                      then []
                      else let c = coords p
                           in joinLists (map (+x) (map fst c)) (map (+y) (map snd c))

-- | Função que efetua a rotação de um determinado ponto (x,y) segundo 
--   um dado grau. A rotação é efetuada no sentido horário.
rotation :: Degree -> [Coord] -> [Coord]
rotation  _ [] = []
rotation a (p:ps) = b : (rotation a ps) 
        where (x,y) =  auxToPolar p
              b = auxToRet (x, y - (degToRad a) )

-- | Junção de duas listas, uma lista com o 'x' e outra com os 'y', 
--   resultando uma lista com pares ordenados (x,y).
joinLists :: [a] -> [b] -> [(a,b)]
joinLists [] _ = []
joinLists _ [] = []
joinLists (x:xs) (y:ys) = (x,y) : joinLists xs ys


-- | Calcula pontos de um círculo com um determinado intervalo entre eles.
pointsCircle :: Float -> [(Float,Float)]
pointsCircle r = [(r*sin x,r*cos x) | x <- [0,0.26..pi]] ++ [(-r*sin x,r*cos x) | x <- [0,0.26..pi]]




---------------------------------------------------------------------
-- * __Conversão de ângulos e conversão de coordenadas__
---------------------------------------------------------------------

-- | Converte graus em radianos.
degToRad :: Degree -> Radian
degToRad x = x * (pi)/180

-- | Converte radianos em graus.
radToDeg :: Radian -> Degree
radToDeg y = y * 180/(pi)


-- | Converte um ponto para coordenada polar, do tipo __@( d, alpha )@__, 
--   onde d é distância do ponto à origem e alpha o ângulo formado.
auxToPolar :: Coord -> CoordPolar
auxToPolar (x,y)
         | (x > 0 && y > 0)   = ( sqrt (x^2 + y^2) , atan (y/x) )
         | (x > 0 && y < 0)   = ( sqrt (x^2 + y^2) , (2*pi + atan (y/x)) )
         | (x < 0 && y > 0)   = ( sqrt (x^2 + y^2) , (pi - atan (y/(-x))) )
         | (x < 0 && y < 0)   = ( sqrt (x^2 + y^2) , atan ((-y)/(-x)) + pi )
         | (y == 0 && x > 0)  = ( abs (x)          , 0 )
         | (y == 0 && x < 0)  = ( abs (x)          , pi )
         | (x == 0 && y > 0)  = ( abs (y)          , pi/2 )
         | (x == 0 && y < 0)  = ( abs (y)          , 3/2*pi )
         | (x == 0 && y == 0) = ( 0                , 0 )


-- | Converte um ponto para coodenadas retangulares.
auxToRet :: CoordPolar -> Coord
auxToRet (x,y) = (x*cos y , x*sin y)



---------------------------------------------------------------------
-- * __Testes /HUnit/__
---------------------------------------------------------------------

-- | Testar a área de uma Picture com recurso ao __/HUnit/__.
tarefa5test = TestLabel "Teste da área de uma Picture" (
    TestList [
        TestCase $ assertEqual "t1"  (0,0)    (tarefa5 Blank),
        TestCase $ assertEqual "t2"  (0,0)    (tarefa5 (Pictures [])),
        TestCase $ assertEqual "t3"  (2,2)    (tarefa5 (Circle 1)),
        TestCase $ assertEqual "t4"  (5,1)    (tarefa5 (Line [(1,2) , (3,1) , (-2,1) ])),
        TestCase $ assertEqual "t5"  (7,4)    (tarefa5 (Polygon [(-1,1) , (-3,2) , (4,5) ])),
        TestCase $ assertEqual "t6"  (40,40)  (tarefa5 (Color red (Circle 20))),
        TestCase $ assertEqual "t7"  (0,0)    (tarefa5 (Translate 2 2 Blank)),
        TestCase $ assertEqual "t8"  (2,2)    (tarefa5 (Translate 3 1 (Circle 1))),
        TestCase $ assertEqual "t9"  (7,4)    (tarefa5 (Translate 5 (-2) (Polygon [(-1,1) , (-3,2) , (4,5) ]))),
        TestCase $ assertEqual "t10" (40,40)  (tarefa5 (Translate 4 5 (Color red (Circle 20)))),
        TestCase $ assertEqual "t11" (0,0)    (tarefa5 (Scale 3 2 Blank)),
        TestCase $ assertEqual "t12" (2,12)   (tarefa5 (Scale 1 6 (Circle 1))),
        TestCase $ assertEqual "t13" (21,8)   (tarefa5 (Scale 3 2 (Polygon [(-1,1) , (-3,2) , (4,5) ]))),
        TestCase $ assertEqual "t14" (20,5)   (tarefa5 (Scale 4 5 (Line [(1,2) , (3,1) , (-2,1) ]))), 
        TestCase $ assertEqual "t15" (0,0)    (tarefa5 (Rotate 33 Blank)),
        TestCase $ assertEqual "t16" (4,4)    (tarefa5 (Rotate 90 (Circle 2))),
        TestCase $ assertEqual "t17" (7,3)    (tarefa5 (Rotate 45 (Polygon [(-1,1) , (-3,2) , (4,5) ]))),
        TestCase $ assertEqual "t18" (40,40)  (tarefa5 (Rotate 11 (Color red (Circle 20)))),  
        TestCase $ assertEqual "t19" (160,80) (tarefa5 (Scale 4 2 (Color red (Circle 20)))),
        TestCase $ assertEqual "t20" (5,1)    (tarefa5 (Translate 3 2 (Line [(1,2) , (3,1) , (-2,1) ]))),
        TestCase $ assertEqual "t21" (0,0)    (tarefa5 (Translate 4 3 (Rotate 33 Blank))),
        TestCase $ assertEqual "t22" (20,5)   (tarefa5 (Translate 2 5 (Scale 4 5 (Line [(1,2) , (3,1) , (-2,1) ])))),
        TestCase $ assertEqual "t23" (7,4)    (tarefa5 (Translate 7 (-1) (Translate 5 (-2) (Polygon [(-1,1) , (-3,2) , (4,5) ])))),
        TestCase $ assertEqual "t24" (9,9)    (tarefa5 (Rotate 45 (Scale 1 6 (Circle 1)))),
        TestCase $ assertEqual "t25" (6,5)    (tarefa5 (Rotate (-20) (Translate 5 (-2) (Polygon [(-1,1) , (-3,2) , (4,5) ])))),
        TestCase $ assertEqual "t26" (0,0)    (tarefa5 (Rotate 90 (Scale 3 2 Blank))),
        TestCase $ assertEqual "t27" (40,40)  (tarefa5 (Rotate 24 (Rotate 12 (Circle 20)))),
        TestCase $ assertEqual "t28" (0,0)    (tarefa5 (Scale 3 3 (Rotate 33 Blank))),
        TestCase $ assertEqual "t29" (30,6)   (tarefa5 (Scale 5 1 (Translate 3 1 (Circle 3)))),
        TestCase $ assertEqual "t30" (30,5)   (tarefa5 (Scale 3 5 (Scale 2 1 (Line [(1,2) , (3,1) , (-2,1) ])))),
        TestCase $ assertEqual "t31" (40,40)  (tarefa5 (Pictures [Circle 20 , Blank , Line [(20,1) , (3,2) , (5,2) ] ] )),
        TestCase $ assertEqual "t32" (80,80)  (tarefa5 (Scale 2 2 (Pictures [Circle 20 , Blank , Line [(20,1) , (3,2) , (5,2) ] ]) )),
        TestCase $ assertEqual "t33" (40,40)  (tarefa5 (Rotate 25 (Pictures [Circle 20 , Blank , Line [(20,1) , (3,2) , (5,2) ] ]) )),
        TestCase $ assertEqual "t34" (42,40)  (tarefa5 (Pictures [Circle 20 , Blank , Translate 2 3 (Line [(20,1) , (3,2) , (5,2) ]) ] ))
            ])