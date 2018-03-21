{- |
Module: Main
Description: Tarefa 5 da 2ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 5 da 2ª Fase do projeto de LI1.
-}

module Main where

import qualified Data.Text as T
import GlossExtras
import Graphics.Gloss.Data.Picture
import Data.List hiding (find)
import Data.Char
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
{-import System.FilePath.Find
import Test.HUnit hiding (Path)
import Test.Tasty
import Test.Tasty.HUnit.Adapter
import Graphics.Gloss.Rendering


-- * Testes

tests :: IO Test
tests = do
    bmpteste <- loadBMP "bmptest.bmp"
    let hunit = TestLabel "HUnit" $ TestList [testsTE bmpteste]
    mooshak <- testesMooshak
    return $ TestList [hunit,mooshak]

main = do
    tt <- tests
    defaultMain $ testGroup "Tests" $ hUnitTestToTestTree tt


-- ** Testes unitários

testsTE bmpteste = TestLabel "Tarefa E" $ TestList [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20]
   where t1 = TestCase (assertEqual "t1" "40 40" (tarefa5 (Circle 20)))
         t2 = TestCase (assertEqual "t2" "50 50" (tarefa5 (Pictures [Circle 20,Pictures [Blank,Translate 10.3 10.3 $ Circle 20]])))
         t3 = TestCase (assertEqual "t3" "0 0" (tarefa5 Blank))
         t4 = TestCase (assertEqual "t4" "70 40" (tarefa5 (Translate 3 10 $ Color black $ Polygon [(0,0),(0,40),(70,40),(70,0)])))
         t5 = TestCase (assertEqual "t5" "50 50" (tarefa5 (Scale 0.5 0.5 $ Color white $ Circle 50)))
         t6 = TestCase (assertEqual "t6" "4 6" (tarefa5 (Scale 0.1 0.1 $ Polygon [(0,0),(40,60),(40,0),(0,60)])))
         t7 = TestCase (assertEqual "t7" "5 5" (tarefa5 (Pictures [Scale 0.1 0.1 (Circle 25)])))
         t8 = TestCase (assertEqual "t8" "70 70" (tarefa5 (Translate 30 50 $ Color red $ Polygon [(80,10),(10,80),(40,30),(30,40),(70,30)])))
         t9 = TestCase (assertEqual "t9" "100 100" (tarefa5 (Pictures [Blank,Circle 50, Polygon [(10,10),(0,0),(0,10),(10,0)]])))
         t10 = TestCase (assertEqual "t10" "45 160" (tarefa5 (Scale 0.5 2 $ Polygon [(10,20),(30,40),(20,30),(40,10),(50,30),(70,80),(100,90)])))
         t11 = TestCase (assertEqual "t11" "10 5" (tarefa5 (Polygon [(0,0),(10,0),(5,5)])))
         t12 = TestCase (assertEqual "t12" "50 30" (tarefa5 (Line [(30,10),(10,40),(50,10),(60,30)])))
         t13 = TestCase (assertEqual "t13" "4 35" (tarefa5 (Pictures [Blank,Color white $ Scale 0.1 0.5 $ Line [(60,80),(50,40),(90,10)]])))
         t14 = TestCase (assertEqual "t14" "50 50" (tarefa5 (Pictures [Scale 0.5 0.5 $ Translate (-20) (-20) (Circle 20), Line [(10,10),(20,20),(30,30)]])))
         t15 = TestCase (assertEqual "t15" "14 28" (tarefa5 (Rotate 45 $ Polygon [(30,10),(10,30),(5,15)])))
         t16 = TestCase (assertEqual "t16" "19 27" (tarefa5 (Rotate 30 $ Color black $ Translate 20 20 $ Polygon [(30,10),(10,30),(5,15)])))
         t17 = TestCase (assertEqual "t17" "69 40" (tarefa5 (Pictures [Circle 20,Pictures[Blank, Scale 0.7 0.2 $ Line [(40,50),(10,20),(20,30),(30,40),(70,50),(50,70),(60,10)]]])))
         t18 = TestCase (assertEqual "t18" "764 142" (tarefa5 (Pictures [Rotate 50 $ Translate 500 500 $ Circle 60,Line [(20,10),(10,20)], Scale 2 2 $ Polygon [(10,10),(0,10),(10,0),(0,0)]])))
         t19 = TestCase (assertEqual "t19" "540 246" (tarefa5 (Scale 2 2 $ bmpteste)))
         t20 = TestCase (assertEqual "t20" "175 294" (tarefa5 (Translate 60 40 $ Rotate 73 $ bmpteste)))


-- * Testes à la mooshak

testesMooshak :: IO Test
testesMooshak = do
    inputs5 <- find (depth ==? 0) (extension ==? ".in") "../tests/T5"
    let t5 = TestLabel "Tarefa E" $ TestList $ map (testesTarefa tarefa5) inputs5
    return $ TestLabel "Mooshak" $ TestList [t5]

testesTarefa :: (Picture -> String) -> String -> Test
testesTarefa tarefa input = TestLabel nome $ test $ do
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let out = tarefa5 (readPicture inp)
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    return (out == esp)
  where
    -- nome do ficheiro
    nome = reverse $ drop 3 $ reverse input
-}


-- | Função geral da tarefa E.

main = do inp <- getContents
          putStrLn (tarefa5 (readPicture inp))


tarefa5 :: Picture -> String
tarefa5 x = let (z,y) = poli (tarefa x)
            in (show (round z) ++ " " ++ show (round y))

{- ^ Função que dada uma picture, devolve um par de /floats/ (largura, altura) com as dimensões do menor retângulo envolvente dessa picture.

Exemplo de utilização:

@
 >>> tarefa5 (Circle 20)
"40 40"
@

= Funções Secundárias
-}


-- | Função que dada uma lista de pontos devolve um par de /floats/. O primeiro corresponde à diferença entre a abcissa maior e abcissa menor e o segundo corresponde à diferença entre a ordenada maior e a ordenada menor.

poli :: Path -> (Float, Float)
poli [] = (0,0)
poli a = (largura,altura)
     where largura = (maximum (map (\x-> fst x) a)) - (minimum (map (\x-> fst x) a))
           altura = (maximum (map (\x-> snd x) a)) - (minimum (map (\x-> snd x) a))


tarefa :: Picture -> Path
tarefa (Color x y) = tarefa y
tarefa (Translate x y c) = map (\(c,d) -> ((x+c),(y+d))) (tarefa c)
tarefa (Rotate f p) =  map (\(x,y)->((x*cos (rad f) - y* sin (rad f)), (x*sin (rad f) + y* cos (rad f)) )) (tarefa p)
tarefa (Scale x y p) = map (\(c,d) -> ((x*c),(y*d))) (tarefa p)
tarefa (Pictures x) =  concat (map tarefa x)            
tarefa Blank = []
tarefa (Polygon x) = x
tarefa (Line x) = x
tarefa (Circle x) =  circulo 360 (x,0)
tarefa (Bitmap a b _ _) = [((fromIntegral a/2), (fromIntegral b/2)),((fromIntegral a/2),  -(fromIntegral b/2)),(-  (fromIntegral a/2),(fromIntegral b/2)),(-(fromIntegral b/2),- (fromIntegral b/2))]

-- ^ Função que recebe uma picture e a transforma numa lista de pontos.


-- | A partir do raio e de um centro cria uma lista de pontos que formam um círculo.

circulo :: Float -> Point -> Path
circulo 0 _ = [] 
circulo f (x,y) = ((x*cos (rad f) - y* sin (rad f)), (x*sin (rad f) + y* cos (rad f)) ) : circulo (f-5) ((x*cos (rad f) - y* sin (rad f)), (x*sin (rad f) + y* cos (rad f)) )


{- | Converte graus para radianos.

@
 >>> rad (-90)
1.5707964
@

@
 >>> rad 60
-1.0471976
@

-}
rad :: Float -> Float
rad x = -((x*pi)/180)