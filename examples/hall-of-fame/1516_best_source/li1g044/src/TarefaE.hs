--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TarefaE
Description : Resolução da Tarefa E.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém uma resolução correspondente à __Tarefa E__ da segunda fase
do projeto de Laboratórios de Informática I.

O objetivo é o de apresentar no @stdout@ as dimensões do menor retângulo
(alinhado com os eixos) envolvente de uma 'Picture', a qual é descrita pelo
input fornecido através do @stdin@.
-}
module TarefaE where

import Data.Maybe
import GlossExtras
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Test.HUnit
import Test.QuickCheck

--------------------------------------------------------------------------------

main = do inp <- getContents
          let (x,y) = tarefaE (readPicture inp)
          putStrLn (show (round x) ++ " " ++ show (round y))

--------------------------------------------------------------------------------
-- * Entrada

-- | Função inicial da tarefa. Recebe o input do programa e devolve o output
-- correspondente.
tarefaE :: Picture -> (Float, Float)
tarefaE pic = maybe (0,0) rectSize (pictureBounds pic)

--------------------------------------------------------------------------------
-- * Tipos e Funções Auxiliares

-- $
-- Tipos e funções genéricos utilizados pelo programa.

-- | Adiciona dois valores do tipo 'Vector' componente por componente, segundo a
-- equação:
--
-- @(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)@
--
-- ==== Exemplos de utilização:
-- >>> (1,4) |+| (2,5)
-- (3,9)
(|+|) :: Vector -> Vector -> Vector
(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)

tests_vecAdd = TestLabel "(|+|)" $ TestList [
    TestCase $ assertEqual "for ((1,4) |+| (2,5))," (3,9) ((1,4) |+| (2,5)),
    TestCase $ assertEqual "for ((1,4) |+| (0,0))," (1,4) ((1,4) |+| (0,0)),
    TestCase $ assertEqual "for ((0,0) |+| (2,5))," (2,5) ((0,0) |+| (2,5))
    ]

prop_vecAdd_commutativity :: Vector -> Vector -> Bool
prop_vecAdd_commutativity v1 v2 = (v1 |+| v2) == (v2 |+| v1)

prop_vecAdd_leftIdentity :: Vector -> Bool
prop_vecAdd_leftIdentity v = ((0,0) |+| v) == v

prop_vecAdd_rightIdentity :: Vector -> Bool
prop_vecAdd_rightIdentity v = (v |+| (0,0)) == v

-- | Multiplica dois valores do tipo 'Vector' componente por componente, segundo
-- a equação:
--
-- @(x1, y1) |*| (x2, y2) = (x1*x2, y1*y2)@
--
-- ==== Exemplos de utilização:
-- >>> (1,3) |*| (2,3)
-- (2,9)
(|*|) :: Vector -> Vector -> Vector
(x1, y1) |*| (x2, y2) = (x1*x2, y1*y2)

tests_vecMul = TestLabel "(|*|)" $ TestList [
    TestCase $ assertEqual "for ((1,3) |*| (2,3))," (2,9) ((1,3) |*| (2,3)),
    TestCase $ assertEqual "for ((1,3) |*| (0,0))," (0,0) ((1,3) |*| (0,0)),
    TestCase $ assertEqual "for ((0,0) |*| (2,3))," (0,0) ((0,0) |*| (2,3))
    ]

prop_vecMul_commutativity :: Vector -> Vector -> Bool
prop_vecMul_commutativity v1 v2 = (v1 |*| v2) == (v2 |*| v1)

prop_vecMul_leftIdentity :: Vector -> Bool
prop_vecMul_leftIdentity v = ((1,1) |*| v) == v

prop_vecMul_rightIdentity :: Vector -> Bool
prop_vecMul_rightIdentity v = (v |*| (1,1)) == v

prop_vecMul_leftAbsorbing :: Vector -> Bool
prop_vecMul_leftAbsorbing v = ((0,0) |*| v) == (0,0)

prop_vecMul_rightAbsorbing :: Vector -> Bool
prop_vecMul_rightAbsorbing v = (v |*| (0,0)) == (0,0)

-- | Representa uma matrix quadrada de ordem 3.
--
-- Deve ter @9@ elementos.
type Matrix = [Float]

-- | Devolve o elemento de uma matrix quadrada de ordem 3 na posição
-- especificada.
--
-- Os índices da linha e da coluna devem ter valores entre @1@ e @3@.
--
-- ==== Exemplos de utilização:
-- >>> [1,2,3,4,5,6,7,8,9] `matGet` (1,1)
-- 1
--
-- >>> [1,2,3,4,5,6,7,8,9] `matGet` (2,1)
-- 4
--
-- >>> [1,2,3,4,5,6,7,8,9] `matGet` (1,3)
-- 3
--
-- >>> [1,2,3,4,5,6,7,8,9] `matGet` (3,3)
-- 9
matGet :: Matrix     -- ^ Matriz.
       -> (Int, Int) -- ^ (Linha do elemento, Coluna do elemento)
       -> Float      -- ^ Elemento na posição especificada.
matGet m (i,j) = m !! (j + 3*i - 4)

tests_matGet = TestLabel "matGet" $ TestList [
    TestCase $ assertEqual "for ([1,2,3,4,5,6,7,8,9] `matGet` (1,1)),"
        1 ([1,2,3,4,5,6,7,8,9] `matGet` (1,1)),
    TestCase $ assertEqual "for ([1,2,3,4,5,6,7,8,9] `matGet` (2,1)),"
        4 ([1,2,3,4,5,6,7,8,9] `matGet` (2,1)),
    TestCase $ assertEqual "for ([1,2,3,4,5,6,7,8,9] `matGet` (1,3)),"
        3 ([1,2,3,4,5,6,7,8,9] `matGet` (1,3)),
    TestCase $ assertEqual "for ([1,2,3,4,5,6,7,8,9] `matGet` (3,3)),"
        9 ([1,2,3,4,5,6,7,8,9] `matGet` (3,3))
    ]

-- | Multiplica duas matrizes quadradas de ordem 3.
--
-- ==== Exemplos de utilização:
-- >>> [1,4,2,3,0,4,0,1,1] `matMul` [2,0,1,5,2,3,4,1,2]
-- [30,10,17,22,4,11,9,3,5]
--
-- >>> [0,0,0,0,0,0,0,0,0] `matMul` [2,0,1,5,2,3,4,1,2]
-- [0,0,0,0,0,0,0,0,0]
--
-- >>> [1,4,2,3,0,4,0,1,1] `matMul` [0,0,0,0,0,0,0,0,0]
-- [0,0,0,0,0,0,0,0,0]
--
-- >>> [1,0,0,0,1,0,0,0,1] `matMul` [2,0,1,5,2,3,4,1,2]
-- [2,0,1,5,2,3,4,1,2]
--
-- >>> [1,4,2,3,0,4,0,1,1] `matMul` [1,0,0,0,1,0,0,0,1]
-- [1,4,2,3,0,4,0,1,1]
matMul :: Matrix -> Matrix -> Matrix
matMul a b = map f [(i,j) | i <- [1..3], j <- [1..3]]
    where f (i,j) = sum [(a `matGet` (i,k)) * (b `matGet` (k,j)) | k <- [1..3]]

tests_matMul = TestLabel "matMul" $ TestList [
    TestCase $ assertEqual
        "for ([1,4,2,3,0,4,0,1,1] `matMul` [2,0,1,5,2,3,4,1,2]),"
        [30,10,17,22,4,11,9,3,5]
        ([1,4,2,3,0,4,0,1,1] `matMul` [2,0,1,5,2,3,4,1,2]),
    TestCase $ assertEqual
        "for ([0,0,0,0,0,0,0,0,0] `matMul` [2,0,1,5,2,3,4,1,2]),"
        [0,0,0,0,0,0,0,0,0]
        ([0,0,0,0,0,0,0,0,0] `matMul` [2,0,1,5,2,3,4,1,2]),
    TestCase $ assertEqual
        "for ([1,4,2,3,0,4,0,1,1] `matMul` [0,0,0,0,0,0,0,0,0]),"
        [0,0,0,0,0,0,0,0,0]
        ([1,4,2,3,0,4,0,1,1] `matMul` [0,0,0,0,0,0,0,0,0]),
    TestCase $ assertEqual
        "for ([1,0,0,0,1,0,0,0,1] `matMul` [2,0,1,5,2,3,4,1,2]),"
        [2,0,1,5,2,3,4,1,2]
        ([1,0,0,0,1,0,0,0,1] `matMul` [2,0,1,5,2,3,4,1,2]),
    TestCase $ assertEqual
        "for ([1,4,2,3,0,4,0,1,1] `matMul` [1,0,0,0,1,0,0,0,1]),"
        [1,4,2,3,0,4,0,1,1]
        ([1,4,2,3,0,4,0,1,1] `matMul` [1,0,0,0,1,0,0,0,1])
    ]

-- | Constrói uma matriz quadrada de ordem 3 que representa uma translação no
-- plano.
--
-- ==== Exemplos de utilização:
-- >>> translationMatrix (2,3)
-- [1,0,2,
--  0,1,3,
--  0,0,1]
translationMatrix :: Vector -- ^ Vetor da translação.
                  -> Matrix -- ^ Matriz resultante.
translationMatrix (x,y) = [
    1,0,x,
    0,1,y,
    0,0,1
    ]

-- | Constrói uma matriz quadrada de ordem 3 que representa uma rotação no plano
-- em relação à origem.
--
-- O ângulo deve ser especificado em radianos, no sentido contrário aos
-- ponteiros do relógio.
--
-- ==== Exemplos de utilização:
-- >>> rotationMatrix pi
-- [-1, 0, 0,
--   0,-1, 0,
--   0, 0, 1]
rotationMatrix :: Float  -- ^ Ângulo da rotação.
               -> Matrix -- ^ Matriz resultante.
rotationMatrix a = [
    cos a, -sin a, 0,
    sin a,  cos a, 0,
        0,      0, 1
    ]

-- | Constrói uma matriz quadrada de ordem 3 que representa uma transformação de
-- escala no plano em relação à origem.
--
-- ==== Exemplos de utilização:
-- >>> scalingMatrix (4,5)
-- [4,0,0,
--  0,5,0,
--  0,0,1]
scalingMatrix :: Vector -- ^ Vetor da escala.
              -> Matrix -- ^ Matriz resultante.
scalingMatrix (x,y) = [
    x,0,0,
    0,y,0,
    0,0,1
    ]

-- | Representa um retângulo alinhado com os eixos.
data Rect = Rect {
    rectLeft   :: Float, -- ^ Coordenada, ao longo do eixo @Ox@, da aresta
                         -- esquerda do retângulo.
    rectBottom :: Float, -- ^ Coordenada, ao longo do eixo @Oy@, da aresta
                         -- inferior do retângulo.
    rectRight  :: Float, -- ^ Coordenada, ao longo do eixo @Ox@, da aresta
                         -- direita do retângulo.
    rectTop    :: Float -- ^ Coordenada, ao longo do eixo @Oy@, da aresta
                         -- superior do retângulo.
    }
    deriving (Eq, Show)

-- | Calcula as dimensões de um retângulo representado por um valor do tipo
-- 'Rect', através da equação:
--
-- @rectSize (Rect l b r t) = (r-l, t-b)@
--
-- ==== Exemplos de utilização:
-- >>> rectSize (Rect 1 5 4 7)
-- (3,2)
rectSize :: Rect -> Vector
rectSize (Rect l b r t) = (r-l, t-b)

tests_rectSize = TestLabel "rectSize" $ TestList [
    TestCase $ assertEqual "for (rectSize (Rect 1 5 4 7)),"
        (3,2) (rectSize (Rect 1 5 4 7)),
    TestCase $ assertEqual "for (rectSize (Rect 0 5 4 7)),"
        (4,2) (rectSize (Rect 0 5 4 7)),
    TestCase $ assertEqual "for (rectSize (Rect 1 5 1 7)),"
        (0,2) (rectSize (Rect 1 5 1 7))
    ]

-- | Calcula o menor retângulo (alinhado com os eixos) que contém todos os
-- retângulos especificados (também alinhados com os eixos).
--
-- Se a lista de retângulos especificada for vazia, é devolvido o valor
-- 'Nothing'. Em caso contrário, é devolvido o valor @'Just' rect@, onde @rect@
-- corresponde a um valor do tipo 'Rect' que representa o menor retângulo que
-- contém todos os retângulos especificados.
--
-- ==== Exemplos de utilização:
-- >>> joinRects []
-- Nothing
--
-- >>> joinRects [Rect 1 5 4 7]
-- Just (Rect 1 5 4 7)
--
-- >>> joinRects [Rect 1 5 4 7, Rect (-1) 4 4 5]
-- Just (Rect (-1) 4 4 7)
joinRects :: [Rect] -> Maybe Rect
joinRects []    = Nothing
joinRects rects = Just $
    Rect
        (minimum $ map rectLeft   rects)
        (minimum $ map rectBottom rects)
        (maximum $ map rectRight  rects)
        (maximum $ map rectTop    rects)

tests_joinRects = TestLabel "joinRects" $ TestList [
    TestCase $ assertEqual "for (joinRects []),"
        Nothing (joinRects []),
    TestCase $ assertEqual "for (joinRects [Rect 1 5 4 7]),"
        (Just (Rect 1 5 4 7)) (joinRects [Rect 1 5 4 7]),
    TestCase $ assertEqual "for (rectSize (Rect 1 5 1 7)),"
        (Just (Rect (-1) 4 4 7)) (joinRects [Rect 1 5 4 7, Rect (-1) 4 4 5])
    ]

--------------------------------------------------------------------------------
-- * Elementos Geométricos

-- $
-- Tipos e funções relacionados com a representação e manipulação de elementos
-- geométricos no plano.

-- | Representa um ponto, uma linha, um polígono, uma elipse ou um conjunto
-- desses elementos geométricos.
--
-- Funciona como tipo intermédio para o cálculo do retângulo envolvente de uma
-- forma no plano.
data Shape
    = ShapePoints [Vector]
        -- ^ Representa um ponto, uma linha ou um polígono através de uma lista
        -- de pontos no plano. Os pontos devem representar os vértices da forma.
    | ShapeEllipse Matrix
        -- ^ Representa uma elipse através da matriz que transforma a
        -- circunferência unitária nessa elipse.
    | ShapeList [Shape]
        -- ^ Representa um conjunto de elementos geométricos.

-- | Aplica uma translação a um valor do tipo 'Shape'.
translateShape :: Vector -- ^ Translação.
               -> Shape  -- ^ Elemento geométrico ao qual aplicar a translação.
               -> Shape  -- ^ Elemento geométrico resultante.
translateShape t (ShapePoints pts)  = ShapePoints $ map (|+| t) pts
translateShape t (ShapeEllipse m)   = ShapeEllipse $ translationMatrix t `matMul` m
translateShape t (ShapeList shapes) = ShapeList $ map (translateShape t) shapes

-- | Aplica uma rotação (em torno da origem) a um valor do tipo 'Shape'.
rotateShape :: Float -- ^ Rotação; radianos, sentido contrário aos
                     -- ponteiros do relógio.
            -> Shape -- ^ Elemento geométrico ao qual aplicar a rotação.
            -> Shape -- ^ Elemento geométrico resultante.
rotateShape r (ShapePoints pts)  = ShapePoints $ map (rotateV r) pts
rotateShape r (ShapeEllipse m)   = ShapeEllipse $ rotationMatrix r `matMul` m
rotateShape r (ShapeList shapes) = ShapeList $ map (rotateShape r) shapes

-- | Aplica uma transformação de escala a um valor do tipo 'Shape'.
scaleShape :: Vector -- ^ Escala.
           -> Shape  -- ^ Elemento geométrico ao qual aplicar a transformação.
           -> Shape  -- ^ Elemento geométrico resultante.
scaleShape s (ShapePoints pts)  = ShapePoints $ map (s |*|) pts
scaleShape s (ShapeEllipse m)   = ShapeEllipse $ scalingMatrix s `matMul` m
scaleShape s (ShapeList shapes) = ShapeList $ map (scaleShape s) shapes

-- | Converte um valor do tipo 'Picture' num valor do tipo 'Shape' que
-- representa o mesmo elemento geométrico no plano.
--
-- __Nota:__ Os construtores 'ThickCircle', 'Arc', 'ThickArc' e 'Text' não são
-- suportados; a função não está definida nesses casos.
pictureToShape :: Picture -> Shape
pictureToShape Blank               = ShapePoints []
pictureToShape (Polygon pts)       = ShapePoints pts
pictureToShape (Line pts)          = ShapePoints pts
pictureToShape (Circle r)          = ShapeEllipse $ scalingMatrix (r,r)
pictureToShape (Bitmap w h _ _)    =
    ShapePoints [(-hw,-hh), (hw,-hh), (hw,hh), (-hw,hh)]
    where
        hw = (fromIntegral w) / 2
        hh = (fromIntegral h) / 2
pictureToShape (Color _ pic)       = pictureToShape pic
pictureToShape (Translate x y pic) = translateShape (x,y) (pictureToShape pic)
pictureToShape (Rotate r pic)      = rotateShape th (pictureToShape pic)
    where th = degToRad (-r)
pictureToShape (Scale sx sy pic)   = scaleShape (sx,sy) (pictureToShape pic)
pictureToShape (Pictures pics)     = ShapeList $ map pictureToShape pics

-- | Calcula o menor retângulo (alinhado com os eixos) que contém a
-- representação visual do valor do tipo 'Shape' especificado.
--
-- Se o valor especificado não tiver uma representação visual associada (e.g.
-- @'ShapePoints' []@), é devolvido o valor 'Nothing'. Em caso contrário, é
-- devolvido o valor @'Just' rect@, onde @rect@ corresponde a um valor do tipo
-- 'Rect' que representa o menor retângulo (alinhado com os eixos) que contém a
-- representação visual do valor do tipo 'Shape' especificado.
shapeBounds :: Shape -> Maybe Rect
shapeBounds (ShapePoints [])  = Nothing
shapeBounds (ShapePoints pts) =
    Just $ Rect
        (minimum $ map fst pts)
        (minimum $ map snd pts)
        (maximum $ map fst pts)
        (maximum $ map snd pts)
shapeBounds (ShapeEllipse m) = Just $ Rect xMin yMin xMax yMax
    where
        xCenter = m `matGet` (1,3)
        yCenter = m `matGet` (2,3)
        xOffset = sqrt $ (m `matGet` (1,1))^2 + (m `matGet` (1,2))^2
        yOffset = sqrt $ (m `matGet` (2,1))^2 + (m `matGet` (2,2))^2
        xMin = xCenter - xOffset
        xMax = xCenter + xOffset
        yMin = yCenter - yOffset
        yMax = yCenter + yOffset
shapeBounds (ShapeList shapes) = joinRects $ mapMaybe shapeBounds shapes

-- | Calcula o menor retângulo (alinhado com os eixos) que contém a
-- representação visual do valor do tipo 'Picture' especificado.
--
-- Se o valor especificado não tiver uma representação visual associada (e.g.
-- @'Blank' []@), é devolvido o valor 'Nothing'. Em caso contrário, é devolvido
-- o valor @'Just' rect@, onde @rect@ corresponde a um valor do tipo 'Rect' que
-- representa o menor retângulo (alinhado com os eixos) que contém a
-- representação visual do valor do tipo 'Picture' especificado.
--
-- Equivalente a @('shapeBounds' . 'pictureToShape')@.
pictureBounds :: Picture -> Maybe Rect
pictureBounds = shapeBounds . pictureToShape

--------------------------------------------------------------------------------

runHUTests = runTestTT $ TestList [
    tests_vecAdd,
    tests_vecMul,
    tests_matGet,
    tests_matMul,
    tests_rectSize,
    tests_joinRects
    ]

return []
runQCTests = $(quickCheckAll)

--------------------------------------------------------------------------------
