--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Helper
Description : Tipos e funções auxiliares.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém tipos e funções úteis a vários fins.
-}
module Sokoban.Helper (

    -- Coleções

    insertWithAL,

    -- Matemática

    VectorI,
    fromIntegralVector, (|+|), (|-|), vecNeg, (*|), (|*|),
    
    lerp1, lerp2,

    Rect (..),
    makeRect, makeRectAt,
    rectCenter,
    rectSize, rectWidth, rectHeight,
    pointIsInsideRect,
    rectWirePicture,

    -- Gráficos

    bitmapSize,

    RectMapping,
    PictureMapping,

    regionMapX, regionMapY,
    regionMapRect, regionMapPicture, regionMapBitmap,

    zoomRegionToTop, zoomRegionToCenter, zoomRegionToBottom,
    zoomRectToTop, zoomRectToCenter, zoomRectToBottom,
    zoomPictureToTop, zoomPictureToCenter, zoomPictureToBottom,
    
    multBMPToPicture,
    
    -- Input do Utilizador

    normalizeKey,
    keyToString,

    -- Tempo

    secondsBetween,

    -- Sistema de Ficheiros

    fileName, fileNameWithoutExt, removeExt,
    dirName, getFilesInDir, getDirsInDir, getDirContents,

    -- (Unit Testing)

    runHUTests,
    tests_insertWithAL,
    tests_vecAdd,
    tests_vecSub,
    tests_vecNeg,
    tests_numVecMul,
    tests_vecVecMul,
    tests_lerp1,
    tests_lerp2,
    tests_makeRect,
    tests_makeRectAt,
    tests_rectCenter,
    tests_rectSize,
    tests_rectWidth,
    tests_rectHeight,
    tests_pointIsInsideRect,
    tests_regionMapX,
    tests_regionMapY,
    tests_regionMapRect,
    tests_zoomRegionToTop,
    tests_zoomRegionToCenter,
    tests_zoomRegionToBottom,
    tests_zoomRectToTop,
    tests_zoomRectToCenter,
    tests_zoomRectToBottom,
    tests_fileName,
    tests_fileNameWithoutExt,
    tests_removeExt,
    tests_dirName,

    runQCTests

    ) where

import Codec.BMP
import Data.Char
import System.Clock
import System.Directory
import qualified Data.ByteString as B

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

import Test.HUnit
import Test.QuickCheck

--------------------------------------------------------------------------------
-- * Coleções

-- | Insere um valor com a chave especificada numa lista de associação.
--
-- Se já existir um valor com a mesma chave na lista, esse valor é substituido
-- por:
--
-- @joinFun oldValue newValue@
--
-- * @joinFun@ - a função especificada pelo utilizador;
-- * @oldValue@ - o valor já associado com a chave;
-- * @newValue@ - o valor que o utilizador tentou inserir.
--
-- ==== Exemplos de utilização:
-- >>> insertWithAL (\x _ -> x) 2 'b' []
-- [(2,'b')]
--
-- >>> insertWithAL (\x _ -> x) 2 'b' [(3,'a')]
-- [(3,'a'),(2,'b')]
--
-- >>> insertWithAL (\x _ -> x) 2 'b' [(2,'a')]
-- [(2,'b')]
--
-- >>> insertWithAL (\_ y -> y) 2 'b' [(2,'a')]
-- [(2,'a')]
insertWithAL :: (Eq k)
             => (a -> a -> a) -- ^ Função utilizada quando já existe um elemento
                              -- com a mesma chave.
             -> k             -- ^ Chave do elemento a inserir.
             -> a             -- ^ Valor do elemento a inserir.
             -> [(k, a)]      -- ^ Lista na qual inserir o elemento.
             -> [(k, a)]      -- ^ Lista resultante.
insertWithAL _    key val []          = [(key, val)]
insertWithAL join key val ((k,v) : t)
    | key == k  = (k, join val v) : t
    | otherwise = (k, v) : insertWithAL join key val t

tests_insertWithAL = TestLabel "insertWithAL" $ TestList [
    TestCase $ assertEqual
        "for (insertWithAL (\\x _ -> x) 2 'b' []),"
        [(2,'b')]
        (insertWithAL (\x _ -> x) 2 'b' []),
    TestCase $ assertEqual
        "for (insertWithAL (\\x _ -> x) 2 'b' [(3,'a')]),"
        [(3,'a'),(2,'b')]
        (insertWithAL (\x _ -> x) 2 'b' [(3,'a')]),
    TestCase $ assertEqual
        "for (insertWithAL (\\x _ -> x) 2 'b' [(2,'a')]),"
        [(2,'b')]
        (insertWithAL (\x _ -> x) 2 'b' [(2,'a')]),
    TestCase $ assertEqual
        "for (insertWithAL (\\_ y -> y) 2 'b' [(2,'a')]),"
        [(2,'a')]
        (insertWithAL (\_ y -> y) 2 'b' [(2,'a')])
    ]

--------------------------------------------------------------------------------
-- * Matemática

-- | Representa um vetor bidimensional de componentes do tipo 'Int'.
type VectorI = (Int, Int)

-- | Converte um vetor bidimensional de componentes com tipo na classe
-- 'Integral' para um vetor bidimensional de componentes com tipo na classe
-- 'Num'.
--
-- ==== Exemplos de utilização:
-- >>> fromIntegralVector ((1,2) :: (Int, Int))
-- (1,2) :: (Num a) => (a, a)
fromIntegralVector :: (Integral a, Num b) => (a,a) -> (b,b)
fromIntegralVector (x,y) = (fromIntegral x, fromIntegral y)

-- | Adiciona dois vetores bidimensionais componente por componente, segundo a
-- equação:
--
-- @(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)@
--
-- ==== Exemplos de utilização:
-- >>> (1,4) |+| (2,5)
-- (3,9)
(|+|) :: (Num a) => (a,a) -> (a,a) -> (a,a)
(x1,y1) |+| (x2,y2) = (x1+x2, y1+y2)

tests_vecAdd = TestLabel "(|+|)" $ TestList [
    TestCase $ assertEqual "for ((1,4) |+| (2,5))," (3,9) ((1,4) |+| (2,5)),
    TestCase $ assertEqual "for ((1,4) |+| (0,0))," (1,4) ((1,4) |+| (0,0)),
    TestCase $ assertEqual "for ((0,0) |+| (2,5))," (2,5) ((0,0) |+| (2,5))
    ]

prop_vecAdd_commutativity :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
prop_vecAdd_commutativity v1 v2 = (v1 |+| v2) == (v2 |+| v1)

prop_vecAdd_leftIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_vecAdd_leftIdentity v = ((0,0) |+| v) == v

prop_vecAdd_rightIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_vecAdd_rightIdentity v = (v |+| (0,0)) == v

-- | Subtrai dois vetores bidimensionais componente por componente, segundo a
-- equação:
--
-- @(x1, y1) |-| (x2, y2) = (x1-x2, y1-y2)@
--
-- ==== Exemplos de utilização:
-- >>> (2,7) |-| (1,5)
-- (1,2)
(|-|) :: (Num a) => (a,a) -> (a,a) -> (a,a)
(x1,y1) |-| (x2,y2) = (x1-x2, y1-y2)

tests_vecSub = TestLabel "(|-|)" $ TestList [
    TestCase $ assertEqual "for ((2,7) |-| (1,5))," ( 1, 2) ((2,7) |-| (1,5)),
    TestCase $ assertEqual "for ((2,7) |-| (0,0))," ( 2, 7) ((2,7) |-| (0,0)),
    TestCase $ assertEqual "for ((0,0) |-| (1,5))," (-1,-5) ((0,0) |-| (1,5))
    ]

prop_vecSub_anticommutativity :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
prop_vecSub_anticommutativity v1 v2 = (v1 |-| v2) == vecNeg (v2 |-| v1)

prop_vecSub_rightIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_vecSub_rightIdentity v = (v |-| (0,0)) == v

-- | Nega os componentes de um vetor bidimensional, segundo a equação:
--
-- @vecNeg (x,y) = (-x, -y)@
--
-- ==== Exemplos de utilização:
-- >>> vecNeg (1,2)
-- (-1,-2)
vecNeg :: (Num a) => (a,a) -> (a,a)
vecNeg (x,y) = (-x, -y)

tests_vecNeg = TestLabel "vecNeg" $ TestList [
    TestCase $ assertEqual "for (vecNeg (0,0))," ( 0, 0) (vecNeg (0,0)),
    TestCase $ assertEqual "for (vecNeg (1,2))," (-1,-2) (vecNeg (1,2))
    ]

-- | Multiplica um número por um vetor bidimensional, segundo a equação:
--
-- @s *| (x,y) = (s*x, s*y)@
--
-- ==== Exemplos de utilização:
-- >>> 3 *| (1,2)
-- (3,6)
(*|) :: (Num a) => a -> (a,a) -> (a,a)
s *| (x,y) = (s*x, s*y)

tests_numVecMul = TestLabel "(*|)" $ TestList [
    TestCase $ assertEqual "for (3 *| (0,0))," (0,0) (3 *| (0,0)),
    TestCase $ assertEqual "for (0 *| (1,2))," (0,0) (0 *| (1,2)),
    TestCase $ assertEqual "for (3 *| (1,2))," (3,6) (3 *| (1,2))
    ]

prop_numVecMul_leftIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_numVecMul_leftIdentity v = (1 *| v) == v

prop_numVecMul_leftAbsorbing :: (Eq a, Num a) => (a,a) -> Bool
prop_numVecMul_leftAbsorbing v = (0 *| v) == (0,0)

prop_numVecMul_rightAbsorbing :: (Eq a, Num a) => a -> Bool
prop_numVecMul_rightAbsorbing s = (s *| (0,0)) == (0,0)

-- | Multiplica dois vetores bidimensionais componente por componente, segundo
-- a equação:
--
-- @(x1, y1) |*| (x2, y2) = (x1*x2, y1*y2)@
--
-- ==== Exemplos de utilização:
-- >>> (1,3) |*| (2,3)
-- (2,9)
(|*|) :: (Num a) => (a,a) -> (a,a) -> (a,a)
(x1,y1) |*| (x2,y2) = (x1*x2, y1*y2)

tests_vecVecMul = TestLabel "(|*|)" $ TestList [
    TestCase $ assertEqual "for ((1,3) |*| (2,3))," (2,9) ((1,3) |*| (2,3)),
    TestCase $ assertEqual "for ((1,3) |*| (0,0))," (0,0) ((1,3) |*| (0,0)),
    TestCase $ assertEqual "for ((0,0) |*| (2,3))," (0,0) ((0,0) |*| (2,3))
    ]

prop_vecVecMul_commutativity :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
prop_vecVecMul_commutativity v1 v2 = (v1 |*| v2) == (v2 |*| v1)

prop_vecVecMul_leftIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_vecVecMul_leftIdentity v = ((1,1) |*| v) == v

prop_vecVecMul_rightIdentity :: (Eq a, Num a) => (a,a) -> Bool
prop_vecVecMul_rightIdentity v = (v |*| (1,1)) == v

prop_vecVecMul_leftAbsorbing :: (Eq a, Num a) => (a,a) -> Bool
prop_vecVecMul_leftAbsorbing v = ((0,0) |*| v) == (0,0)

prop_vecVecMul_rightAbsorbing :: (Eq a, Num a) => (a,a) -> Bool
prop_vecVecMul_rightAbsorbing v = (v |*| (0,0)) == (0,0)

-- | Interpolação linear em uma dimensão, segundo a equação:
--
-- @lerp1 t p0 p1 = p0 + (p1-p0) * t@
--
-- ==== Exemplos de utilização:
-- >>> lerp1 0 2 4
-- 2
--
-- >>> lerp1 0.5 2 4
-- 3
--
-- >>> lerp1 1 2 4
-- 4
lerp1 :: (Num a) => a -> a -> a -> a
lerp1 t p0 p1 = p0 + (p1-p0) * t

tests_lerp1 = TestLabel "lerp1" $ TestList [
    TestCase $ assertEqual "for (lerp1 0   2 4)," 2 (lerp1 0   2 4),
    TestCase $ assertEqual "for (lerp1 0.5 2 4)," 3 (lerp1 0.5 2 4),
    TestCase $ assertEqual "for (lerp1 1   2 4)," 4 (lerp1 1   2 4)
    ]

prop_lerp1_fstPt :: (Eq a, Num a) => a -> a -> Bool
prop_lerp1_fstPt p0 p1 = (lerp1 0 p0 p1) == p0

prop_lerp1_sndPt :: (Eq a, Num a) => a -> a -> Bool
prop_lerp1_sndPt p0 p1 = (lerp1 1 p0 p1) == p1

-- | Interpolação linear em duas dimensões, segundo a equação:
--
-- @lerp2 t (x0,y0) (x1,y1) = ('lerp1' t x0 x1, 'lerp1' t y0 y1)@
--
-- ==== Exemplos de utilização:
-- >>> lerp2 0 (2,4) (4,8)
-- (2,4)
--
-- >>> lerp2 0.5 (2,4) (4,8)
-- (3,6)
--
-- >>> lerp2 1 (2,4) (4,8)
-- (4,8)
lerp2 :: (Num a) => a -> (a,a) -> (a,a) -> (a,a)
lerp2 t (x0,y0) (x1,y1) = (lerp1 t x0 x1, lerp1 t y0 y1)

tests_lerp2 = TestLabel "lerp2" $ TestList [
    TestCase $ assertEqual "for (lerp2 0   (2,4) (4,8)),"
        (2,4) (lerp2 0   (2,4) (4,8)),
    TestCase $ assertEqual "for (lerp2 0.5 (2,4) (4,8)),"
        (3,6) (lerp2 0.5 (2,4) (4,8)),
    TestCase $ assertEqual "for (lerp2 1   (2,4) (4,8)),"
        (4,8) (lerp2 1   (2,4) (4,8))
    ]

prop_lerp2_fstPt :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
prop_lerp2_fstPt p0 p1 = (lerp2 0 p0 p1) == p0

prop_lerp2_sndPt :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
prop_lerp2_sndPt p0 p1 = (lerp2 1 p0 p1) == p1

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
    deriving (Eq, Show, Read)

-- | Contrói um retângulo alinhado com os eixos centrado na origem e com o
-- tamanho especificado.
--
-- ==== Exemplos de utilização:
-- >>> makeRect (4,2)
-- Rect (-2) (-1) 2 1
makeRect :: Vector -> Rect
makeRect = makeRectAt (0,0)

tests_makeRect = TestLabel "makeRect" $ TestList [
    TestCase $ assertEqual "for (makeRect (4,2)),"
        (Rect (-2) (-1) 2 1) (makeRect (4,2))
    ]

-- | Contrói um retângulo alinhado com os eixos centrado no ponto e com o
-- tamanho especificados.
--
-- ==== Exemplos de utilização:
-- >>> makeRectAt (3,3) (4,2)
-- Rect 1 2 5 4
makeRectAt :: Vector -> Vector -> Rect
makeRectAt (x,y) (w,h) = Rect (x-hw) (y-hh) (x+hw) (y+hh)
    where
        hw = w / 2
        hh = h / 2

tests_makeRectAt = TestLabel "makeRectAt" $ TestList [
    TestCase $ assertEqual "for (makeRectAt (3,3) (4,2)),"
        (Rect 1 2 5 4) (makeRectAt (3,3) (4,2))
    ]

-- | Calcula as coordenadas do centro de um retângulo representado por um valor
-- do tipo 'Rect'.
--
-- ==== Exemplos de utilização:
-- >>> rectCenter (Rect 1 2 5 4)
-- (3,3)
rectCenter :: Rect -> Vector
rectCenter (Rect l b r t) = ((r+l) / 2, (t+b) / 2)

tests_rectCenter = TestLabel "rectCenter" $ TestList [
    TestCase $ assertEqual "for (rectCenter (Rect 1 2 5 4)),"
        (3,3) (rectCenter (Rect 1 2 5 4))
    ]

-- | Calcula as dimensões de um retângulo representado por um valor do tipo
-- 'Rect'.
--
-- ==== Exemplos de utilização:
-- >>> rectSize (Rect 1 5 4 7)
-- (3,2)
rectSize :: Rect -> Vector
rectSize (Rect l b r t) = (r-l, t-b)

tests_rectSize = TestLabel "rectSize" $ TestList [
    TestCase $ assertEqual "for (rectSize (Rect 1 5 4 7)),"
        (3,2) (rectSize (Rect 1 5 4 7))
    ]

-- | Calcula a largura de um retângulo representado por um valor do tipo 'Rect'.
--
-- ==== Exemplos de utilização:
-- >>> rectWidth (Rect 1 5 4 7)
-- 3
rectWidth :: Rect -> Float
rectWidth (Rect l _ r _) = r-l

tests_rectWidth = TestLabel "rectWidth" $ TestList [
    TestCase $ assertEqual "for (rectWidth (Rect 1 5 4 7)),"
        3 (rectWidth (Rect 1 5 4 7))
    ]

-- | Calcula a altura de um retângulo representado por um valor do tipo 'Rect'.
--
-- ==== Exemplos de utilização:
-- >>> rectHeight (Rect 1 5 4 7)
-- 2
rectHeight :: Rect -> Float
rectHeight (Rect _ b _ t) = t-b

tests_rectHeight = TestLabel "rectHeight" $ TestList [
    TestCase $ assertEqual "for (rectHeight (Rect 1 5 4 7)),"
        2 (rectHeight (Rect 1 5 4 7))
    ]

-- | Verifica se um ponto se encontra dentro de um retângulo representado por um
-- valor do tipo 'Rect'.
--
-- ==== Exemplos de utilização:
-- >>> (0,0) `pointIsInsideRect` (Rect 1 5 4 7)
-- False
--
-- >>> (3,5) `pointIsInsideRect` (Rect 1 5 4 7)
-- True
pointIsInsideRect :: Vector -> Rect -> Bool
pointIsInsideRect (x,y) (Rect l b r t) = (x >= l && x <= r && y >= b && y <= t)

tests_pointIsInsideRect = TestLabel "pointIsInsideRect" $ TestList [
    TestCase $ assertEqual "for ((0,0) `pointIsInsideRect` (Rect 1 5 4 7)),"
        False ((0,0) `pointIsInsideRect` (Rect 1 5 4 7)),
    TestCase $ assertEqual "for ((3,5) `pointIsInsideRect` (Rect 1 5 4 7)),"
        True  ((3,5) `pointIsInsideRect` (Rect 1 5 4 7))
    ]

--------------------------------------------------------------------------------
-- * Gráficos

-- | Devolve o tamanho de um bitmap representado por um valor do tipo 'Picture'
-- construído pelo construtor 'Bitmap'.
bitmapSize :: Picture -> Vector
bitmapSize (Bitmap w h _ _) = fromIntegralVector (w,h)

-- | Representa uma transformação entre valores do tipo 'Rect'.
type RectMapping = Rect -> Rect

-- | Representa uma transformação entre valores do tipo 'Picture'.
type PictureMapping = Picture -> Picture

-- | Transforma uma coordenada ao longo do eixo @Ox@, relativa a uma região
-- retangular, de maneira a que esta passe a ser relativa a outra região
-- retangular.
--
-- ==== Exemplos de utilização:
-- >>> regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 0
-- 8
--
-- >>> regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 3
-- -4
regionMapX :: Rect  -- ^ Região original.
           -> Rect  -- ^ Região de destino.
           -> Float -- ^ Coordenada relativa à região original.
           -> Float -- ^ Coordenada relativa à região de destino.
regionMapX src dst x = dstl + (((dstr - dstl) / (srcr - srcl)) * (x - srcl))
    where
        srcl = rectLeft   src
        srcr = rectRight  src
        dstl = rectLeft   dst
        dstr = rectRight  dst

tests_regionMapX = TestLabel "regionMapX" $ TestList [
    TestCase $ assertEqual "for (regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 0),"
        8 (regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 0),
    TestCase $ assertEqual "for (regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 3),"
        (-4) (regionMapX (Rect 1 3 2 7) (Rect 4 5 0 2) 3)
    ]

-- | Transforma uma coordenada ao longo do eixo @Oy@, relativa a uma região
-- retangular, de maneira a que esta passe a ser relativa a outra região
-- retangular.
--
-- ==== Exemplos de utilização:
-- >>> regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 0
-- 7.25
--
-- >>> regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 3
-- 5
regionMapY :: Rect  -- ^ Região original.
           -> Rect  -- ^ Região de destino.
           -> Float -- ^ Coordenada relativa à região original.
           -> Float -- ^ Coordenada relativa à região de destino.
regionMapY src dst y = dstb + (((dstt - dstb) / (srct - srcb)) * (y - srcb))
    where
        srcb = rectBottom src
        srct = rectTop    src
        dstb = rectBottom dst
        dstt = rectTop    dst

tests_regionMapY = TestLabel "regionMapY" $ TestList [
    TestCase $ assertEqual "for (regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 0),"
        7.25 (regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 0),
    TestCase $ assertEqual "for (regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 3),"
        5 (regionMapY (Rect 1 3 2 7) (Rect 4 5 0 2) 3)
    ]

-- | Transforma um retângulo, relativo a uma região retangular, de maneira a que
-- este passe a ser relativo a outra região retangular.
--
-- ==== Exemplos de utilização:
-- >>> regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)
-- Rect 4 5.75 4 4.25
--
-- >>> regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 0 0 1 1)
-- Rect 8 7.25 4 6.5
regionMapRect :: Rect -- ^ Região original.
              -> Rect -- ^ Região de destino.
              -> Rect -- ^ Retângulo relativo à região original.
              -> Rect -- ^ Retângulo relativo à região de destino.
regionMapRect src dst (Rect l b r t) =
    Rect
        (regionMapX src dst l)
        (regionMapY src dst b)
        (regionMapX src dst r)
        (regionMapY src dst t)

tests_regionMapRect = TestLabel "regionMapRect" $ TestList [
    TestCase $ assertEqual
        "for (regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)),"
        (Rect 4 5.75 4 4.25)
        (regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)),
    TestCase $ assertEqual
        "for (regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 0 0 1 1)),"
        (Rect 8 7.25 4 6.5)
        (regionMapRect (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 0 0 1 1))
    ]

-- | Transforma uma 'Picture', relativa a uma região retangular, de maneira a
-- que esta passe a ser relativa a outra região retangular.
regionMapPicture :: Rect    -- ^ Região original.
                 -> Rect    -- ^ Região de destino.
                 -> Picture -- ^ 'Picture' relativa à região original.
                 -> Picture -- ^ 'Picture' relativa à região de destino.
regionMapPicture src dst =
    Translate dstl dstb
    . Scale ((dstr - dstl) / (srcr - srcl)) ((dstt - dstb) / (srct - srcb))
    . Translate (-srcl) (-srcb)
    where
        srcl = rectLeft   src
        srcb = rectBottom src
        srcr = rectRight  src
        srct = rectTop    src
        dstl = rectLeft   dst
        dstb = rectBottom dst
        dstr = rectRight  dst
        dstt = rectTop    dst

-- | Transforma uma 'Picture' do construtor 'Bitmap' de maneira a que esta passe
-- a ocupar a região retangular especificada.
regionMapBitmap :: Rect    -- ^ Região que o bitmap passa a ocupar.
                -> Picture -- ^ 'Picture' a transformar.
                -> Picture -- ^ 'Picture' transformada.
regionMapBitmap dst pic@(Bitmap w h _ _) = regionMapPicture src dst pic
    where src = makeRect $ fromIntegralVector (w,h)

-- | Redimensiona uma região retangular, mantendo o seu aspect ratio, de maneira
-- a ocupar o máximo de espaço de outra região retangular. A região resultante
-- fica, em relação à região de destino, centrada horizontalmente e encostada
-- acima.
--
-- ==== Exemplos de utilização:
-- >>> zoomRegionToTop (Rect 1 3 2 7) (Rect 4 5 0 2)
-- Rect 4 18 0 2
zoomRegionToTop :: Rect -- ^ Região a redimensionar.
                -> Rect -- ^ Região de destino.
                -> Rect -- ^ Região resultante.
zoomRegionToTop src dst =
    Rect
        (cxDst - wResult / 2)
        (dstt - hResult)
        (cxDst + wResult / 2)
        (dstt)
    where
        dstt = rectTop dst
        (sw, sh) = rectSize src
        (dw, dh) = rectSize dst
        scale =
            if (dw * sh) > (sw * dh) -- <=> (sw / sh) > (rw / rh)
                then dh / sh
                else dw / sw
        (cxDst, _) = rectCenter dst
        (wResult, hResult) = scale *| rectSize src

tests_zoomRegionToTop = TestLabel "zoomRegionToTop" $ TestList [
    TestCase $ assertEqual
        "for (zoomRegionToTop (Rect 1 3 2 7) (Rect 4 5 0 2)),"
        (Rect 4 18 0 2)
        (zoomRegionToTop (Rect 1 3 2 7) (Rect 4 5 0 2))
    ]

-- | Redimensiona uma região retangular, mantendo o seu aspect ratio, de maneira
-- a ocupar o máximo de espaço de outra região retangular. A região resultante
-- fica, em relação à região de destino, centrada horizontalmente e
-- verticalmente.
--
-- ==== Exemplos de utilização:
-- >>> zoomRegionToCenter (Rect 1 3 2 7) (Rect 4 5 0 2)
-- Rect 4 11.5 0 (-4.5)
zoomRegionToCenter :: Rect -- ^ Região a redimensionar.
                   -> Rect -- ^ Região de destino.
                   -> Rect -- ^ Região resultante.
zoomRegionToCenter src dst = makeRectAt (rectCenter dst) resultSize
    where
        (sw, sh) = rectSize src
        (dw, dh) = rectSize dst
        scale = if (dw * sh) > (sw * dh) -- <=> (sw / sh) > (rw / rh)
            then dh / sh
            else dw / sw
        resultSize = scale *| rectSize src

tests_zoomRegionToCenter = TestLabel "zoomRegionToCenter" $ TestList [
    TestCase $ assertEqual
        "for (zoomRegionToCenter (Rect 1 3 2 7) (Rect 4 5 0 2)),"
        (Rect 4 11.5 0 (-4.5))
        (zoomRegionToCenter (Rect 1 3 2 7) (Rect 4 5 0 2))
    ]

-- | Redimensiona uma região retangular, mantendo o seu aspect ratio, de maneira
-- a ocupar o máximo de espaço de outra região retangular. A região resultante
-- fica, em relação à região de destino, centrada horizontalmente e encostada
-- abaixo.
--
-- ==== Exemplos de utilização:
-- >>> zoomRegionToBottom (Rect 1 3 2 7) (Rect 4 5 0 2)
-- Rect 4 5 0 (-11)
zoomRegionToBottom :: Rect -- ^ Região a redimensionar.
                   -> Rect -- ^ Região de destino.
                   -> Rect -- ^ Região resultante.
zoomRegionToBottom src dst =
    Rect
        (cxDst - wResult / 2)
        (dstb)
        (cxDst + wResult / 2)
        (dstb + hResult)
    where
        dstb = rectBottom dst
        (sw, sh) = rectSize src
        (dw, dh) = rectSize dst
        scale = if (dw * sh) > (sw * dh) -- <=> (sw / sh) > (rw / rh)
            then dh / sh
            else dw / sw
        (cxDst, _) = rectCenter dst
        (wResult, hResult) = scale *| rectSize src

tests_zoomRegionToBottom = TestLabel "zoomRegionToBottom" $ TestList [
    TestCase $ assertEqual
        "for (zoomRegionToBottom (Rect 1 3 2 7) (Rect 4 5 0 2)),"
        (Rect 4 5 0 (-11))
        (zoomRegionToBottom (Rect 1 3 2 7) (Rect 4 5 0 2))
    ]

-- | Combinação das funções 'zoomRegionToTop' e 'regionMapRect', definida por:
--
-- @
-- zoomRectToTop src dst r = regionMapRect src (zoomRegionToTop src dst) r
-- @
--
-- ==== Exemplos de utilização:
-- >>> zoomRectToTop (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)
-- Rect 4 22 4 14
zoomRectToTop :: Rect -- ^ Região original.
              -> Rect -- ^ Região de destino.
              -> Rect -- ^ Retângulo relativo à região original.
              -> Rect -- ^ Retângulo relativo à região resultante.
zoomRectToTop src dst = regionMapRect src (zoomRegionToTop src dst)

tests_zoomRectToTop = TestLabel "zoomRectToTop" $ TestList [
    TestCase $ assertEqual
        "for (zoomRectToTop (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)),"
        (Rect 4 22 4 14)
        (zoomRectToTop (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4))
    ]

-- | Combinação das funções 'zoomRegionToCenter' e 'regionMapRect', definida
-- por:
--
-- @
-- zoomRectToCenter src dst r = regionMapRect src (zoomRegionToCenter src dst) r
-- @
--
-- ==== Exemplos de utilização:
-- >>> zoomRectToCenter (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)
-- Rect 4 15.5 4 7.5
zoomRectToCenter :: Rect -- ^ Região original.
                 -> Rect -- ^ Região de destino.
                 -> Rect -- ^ Retângulo relativo à região original.
                 -> Rect -- ^ Retângulo relativo à região resultante.
zoomRectToCenter src dst = regionMapRect src (zoomRegionToCenter src dst)

tests_zoomRectToCenter = TestLabel "zoomRectToCenter" $ TestList [
    TestCase $ assertEqual
        "for (zoomRectToCenter (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)),"
        (Rect 4 15.5 4 7.5)
        (zoomRectToCenter (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4))
    ]

-- | Combinação das funções 'zoomRegionToBottom' e 'regionMapRect', definida
-- por:
--
-- @
-- zoomRectToBottom src dst r = regionMapRect src (zoomRegionToBottom src dst) r
-- @
--
-- ==== Exemplos de utilização:
-- >>> zoomRectToBottom (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)
-- Rect 4 9 4 1
zoomRectToBottom :: Rect -- ^ Região original.
                 -> Rect -- ^ Região de destino.
                 -> Rect -- ^ Retângulo relativo à região original.
                 -> Rect -- ^ Retângulo relativo à região resultante.
zoomRectToBottom src dst = regionMapRect src (zoomRegionToBottom src dst)

tests_zoomRectToBottom = TestLabel "zoomRectToBottom" $ TestList [
    TestCase $ assertEqual
        "for (zoomRectToBottom (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4)),"
        (Rect 4 9 4 1)
        (zoomRectToBottom (Rect 1 3 2 7) (Rect 4 5 0 2) (Rect 1 2 1 4))
    ]

-- | Combinação das funções 'zoomRegionToTop' e 'regionMapPicture', definida por:
--
-- @
-- zoomPictureToTop src dst p = regionMapPicture src (zoomRegionToTop src dst) p
-- @
zoomPictureToTop :: Rect    -- ^ Região original.
                 -> Rect    -- ^ Região de destino.
                 -> Picture -- ^ 'Picture' relativa à região original.
                 -> Picture -- ^ 'Picture' relativa à região resultante.
zoomPictureToTop src dst = regionMapPicture src (zoomRegionToTop src dst)

-- | Combinação das funções 'zoomRegionToCenter' e 'regionMapPicture', definida por:
--
-- @
-- zoomPictureToCenter src dst p = regionMapPicture src (zoomRegionToCenter src dst) p
-- @
zoomPictureToCenter :: Rect    -- ^ Região original.
                    -> Rect    -- ^ Região de destino.
                    -> Picture -- ^ 'Picture' relativa à região original.
                    -> Picture -- ^ 'Picture' relativa à região resultante.
zoomPictureToCenter src dst = regionMapPicture src (zoomRegionToCenter src dst)

-- | Combinação das funções 'zoomRegionToBottom' e 'regionMapPicture', definida por:
--
-- @
-- zoomPictureToBottom src dst p = regionMapPicture src (zoomRegionToBottom src dst) p
-- @
zoomPictureToBottom :: Rect    -- ^ Região original.
                    -> Rect    -- ^ Região de destino.
                    -> Picture -- ^ 'Picture' relativa à região original.
                    -> Picture -- ^ 'Picture' relativa à região resultante.
zoomPictureToBottom src dst = regionMapPicture src (zoomRegionToBottom src dst)

-- | Contrói um valor do tipo 'Picture' que representa as arestas do retângulo
-- especificado.
rectWirePicture :: Rect -> Picture
rectWirePicture (Rect l b r t) = Line [
    (l,b), (r,b), (r,t), (l,t), (l,b)
    ]

-- | Multiplica um bitmap representado por um valor do tipo 'BMP' pela cor
-- especificada e converte o resultado para um valor do tipo 'Picture'.
multBMPToPicture :: Color   -- ^ Cor pela qual multiplicar o bitmap.
                 -> BMP     -- ^ Bitmap a alterar.
                 -> Picture -- ^ Bitmap resultante.
multBMPToPicture color bmp = bitmapOfByteString w h (BitmapFormat BottomToTop PxABGR) (B.pack newPixels) True
    where
        (w,h)       = bmpDimensions bmp
        pixels      = B.unpack $ unpackBMPToRGBA32 bmp
        fPixels     = map (\x -> (fromIntegral x) / 255) pixels
        fNewPixels  = multPixels (rgbaOfColor color) fPixels
        newPixels   = map (\x -> round (x * 255)) (convertRGBAToABGR fNewPixels)

-- | Converte uma lista de valores do tipo 'Float' que representa uma série de
-- píxeis cujos componentes têm a ordem RGBA para uma lista equivalente com os
-- componentes na ordem ABGR.
convertRGBAToABGR :: [Float] -> [Float]
convertRGBAToABGR []            = []
convertRGBAToABGR (r:g:b:a : t) = a:b:g:r : convertRGBAToABGR t

-- | Multiplica uma lista de píxeis (com componentes na ordem RGBA) pela cor
-- especificada.
multPixels :: (Float, Float, Float, Float) -- ^ Cor pela qual multiplicar os
                                           -- píxeis (RGBA).
           -> [Float]                      -- ^ Componentes dos píxeis a
                                           -- alterar.
           -> [Float]                      -- ^ Componentes dos píxeis
                                           -- resultantes. 
multPixels _         []                = []
multPixels (r,g,b,a) (mr:mg:mb:ma : t) =
    (r*mr):(g*mg):(b*mb):(a*ma) : multPixels (r,g,b,a) t

--------------------------------------------------------------------------------
-- * Input do Utilizador

-- | Convert valores da forma @'Char' c@ em @'Char' ('toLower' c)@. Valores de
-- outras formas permanecem iguais.
--
-- ==== Exemplos de utilização:
-- >>> normalizeKey (Char 'A')
-- Char 'a'
--
-- >>> normalizeKey (Char 'a')
-- Char 'a'
--
-- >>> normalizeKey (SpecialKey KeyEnter)
-- SpecialKey KeyEnter
normalizeKey :: Key -> Key
normalizeKey (Char c) = Char (toLower c)
normalizeKey key      = key

-- | Convert um valor do tipo 'Key' numa 'String' que o identifique.
--
-- ==== Exemplos de utilização:
-- >>> keyToString (Char 'A')
-- "A"
--
-- >>> keyToString (Char 'a')
-- "A"
--
-- >>> keyToString (SpecialKey KeyEsc)
-- "Esc"
--
-- >>> keyToString (MouseButton LeftButton)
-- "LeftButton"
keyToString :: Key -> String
keyToString (Char c)              = [toUpper c]
keyToString (SpecialKey KeyEnter) = "\8629"
keyToString (SpecialKey k)        = drop 3 (show k)
keyToString (MouseButton b)       = show b

--------------------------------------------------------------------------------
-- * Tempo

-- | Calcula o número de segundos (arredondado por defeito) entre dois instantes
-- de tempo determinados por valores do tipo 'TimeSpec'.
secondsBetween :: TimeSpec -> TimeSpec -> Int
secondsBetween ts1 ts2 =
    fromIntegral $ (timeSpecAsNanoSecs $ diffTimeSpec ts2 ts1) `div` 1000000000

--------------------------------------------------------------------------------
-- * Sistema de Ficheiros

-- | Extrai o nome do ficheiro de um caminho para um ficheiro.
--
-- ==== Exemplos de utilização:
-- >>> fileName ""
-- ""
--
-- >>> fileName "dir1/dir2/"
-- ""
--
-- >>> fileName "dir1/dir2/ficheiro"
-- "ficheiro"
--
-- >>> fileName "dir1/dir2/ficheiro.ext"
-- "ficheiro.ext"
fileName :: FilePath -> FilePath
fileName = reverse . takeWhile (/= '/') . reverse

tests_fileName = TestLabel "fileName" $ TestList [
    TestCase $ assertEqual
        "for (fileName \"\"),"
        ""
        (fileName ""),
    TestCase $ assertEqual
        "for (fileName \"dir1/dir2/\"),"
        ""
        (fileName "dir1/dir2/"),
    TestCase $ assertEqual
        "for (fileName \"dir1/dir2/ficheiro\"),"
        "ficheiro"
        (fileName "dir1/dir2/ficheiro"),
    TestCase $ assertEqual
        "for (fileName \"dir1/dir2/ficheiro.ext\"),"
        "ficheiro.ext"
        (fileName "dir1/dir2/ficheiro.ext")
    ]

-- | Extrai o nome do ficheiro /sem extensão/ de um caminho para um ficheiro.
--
-- ==== Exemplos de utilização:
-- >>> fileNameWithoutExt ""
-- ""
--
-- >>> fileNameWithoutExt "dir1/dir2/"
-- ""
--
-- >>> fileNameWithoutExt "dir1/dir2/ficheiro"
-- "ficheiro"
--
-- >>> fileNameWithoutExt "dir1/dir2/ficheiro.ext"
-- "ficheiro"
fileNameWithoutExt :: FilePath -> FilePath
fileNameWithoutExt = removeExt . fileName

tests_fileNameWithoutExt = TestLabel "fileNameWithoutExt" $ TestList [
    TestCase $ assertEqual
        "for (fileNameWithoutExt \"\"),"
        ""
        (fileNameWithoutExt ""),
    TestCase $ assertEqual
        "for (fileNameWithoutExt \"dir1/dir2/\"),"
        ""
        (fileNameWithoutExt "dir1/dir2/"),
    TestCase $ assertEqual
        "for (fileNameWithoutExt \"dir1/dir2/ficheiro\"),"
        "ficheiro"
        (fileNameWithoutExt "dir1/dir2/ficheiro"),
    TestCase $ assertEqual
        "for (fileNameWithoutExt \"dir1/dir2/ficheiro.ext\"),"
        "ficheiro"
        (fileNameWithoutExt "dir1/dir2/ficheiro.ext")
    ]

-- | Remove a extensão de um caminho para um ficheiro.
--
-- ==== Exemplos de utilização:
-- >>> removeExt ""
-- ""
--
-- >>> removeExt "dir1/dir2/"
-- "dir1/dir2/"
--
-- >>> removeExt "dir1/dir2/ficheiro"
-- "dir1/dir2/ficheiro"
--
-- >>> removeExt "dir1/dir2/ficheiro.ext"
-- "dir1/dir2/ficheiro"
removeExt :: FilePath -> FilePath
removeExt path =
    if '.' `elem` (takeWhile (/= '/') revName)
        then reverse . drop 1 . dropWhile (/= '.') $ revName
        else path
    where
        revName = reverse path

tests_removeExt = TestLabel "removeExt" $ TestList [
    TestCase $ assertEqual
        "for (removeExt \"\"),"
        ""
        (removeExt ""),
    TestCase $ assertEqual
        "for (removeExt \"dir1/dir2/\"),"
        "dir1/dir2/"
        (removeExt "dir1/dir2/"),
    TestCase $ assertEqual
        "for (removeExt \"dir1/dir2/ficheiro\"),"
        "dir1/dir2/ficheiro"
        (removeExt "dir1/dir2/ficheiro"),
    TestCase $ assertEqual
        "for (removeExt \"dir1/dir2/ficheiro.ext\"),"
        "dir1/dir2/ficheiro"
        (removeExt "dir1/dir2/ficheiro.ext")
    ]

-- | Extrai o nome da diretoria de um caminho para uma diretoria.
--
-- O caminho especificado deve terminar com @\'/\'@.
--
-- ==== Exemplos de utilização:
-- >>> dirName "/"
-- ""
--
-- >>> dirName "dir1/dir2/"
-- "dir2"
dirName :: FilePath -> FilePath
dirName = reverse . takeWhile (/= '/') . drop 1 . reverse

tests_dirName = TestLabel "dirName" $ TestList [
    TestCase $ assertEqual
        "for (dirName \"/\"),"
        ""
        (dirName "/"),
    TestCase $ assertEqual
        "for (dirName \"dir1/dir2/\"),"
        "dir2"
        (dirName "dir1/dir2/")
    ]

-- | Devolve uma lista com os caminhos para os ficheiros na diretoria
-- especificada.
--
-- O caminho especificado deve terminar com @\'/\'@.
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = do
    contents <- getDirectoryContents dir
    return (map (dir ++) . filter ('.' `elem`) $ contents)

-- | Devolve uma lista com os caminhos para as subdiretorias na diretoria
-- especificada.
--
-- O caminho especificado deve terminar com @\'/\'@.
getDirsInDir :: FilePath -> IO [FilePath]
getDirsInDir dir = do
    contents <- getDirectoryContents dir
    return (map (\x -> dir ++ x ++ "/") . filter ('.' `notElem`) $ contents)

-- | Devolve uma lista com os caminhos para os ficheiros e subdiretorias na
-- diretoria especificada.
--
-- O caminho especificado deve terminar com @\'/\'@.
getDirContents :: FilePath -> IO [FilePath]
getDirContents dir = do
    contents <- getDirectoryContents dir
    return $ map (dir ++) contents

--------------------------------------------------------------------------------

runHUTests = runTestTT $ TestList [
    tests_insertWithAL,
    tests_vecAdd,
    tests_vecSub,
    tests_vecNeg,
    tests_numVecMul,
    tests_vecVecMul,
    tests_lerp1,
    tests_lerp2,
    tests_makeRect,
    tests_makeRectAt,
    tests_rectCenter,
    tests_rectSize,
    tests_rectWidth,
    tests_rectHeight,
    tests_pointIsInsideRect,
    tests_regionMapX,
    tests_regionMapY,
    tests_regionMapRect,
    tests_zoomRegionToTop,
    tests_zoomRegionToCenter,
    tests_zoomRegionToBottom,
    tests_zoomRectToTop,
    tests_zoomRectToCenter,
    tests_zoomRectToBottom,
    tests_fileName,
    tests_fileNameWithoutExt,
    tests_removeExt,
    tests_dirName
    ]

return []
runQCTests = $(quickCheckAll)

--------------------------------------------------------------------------------
