--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Board
Description : Tabuleiro de jogo.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o armazenamento e representação visual de tabuleiros de jogo.
-}
module Sokoban.Game.Board (
    Board,
    Cell (..),
    create,
    cellIsEmpty, cellIsTarget,
    drawingRegion, draw
    ) where

import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.Assets as Assets

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa um tabuleiro de jogo.
data Board = Board {
    boardCells     :: [Cell],  -- ^ Células do tabuleiro.
    boardSize      :: VectorI, -- ^ Dimensões, em células, do tabuleiro.

    boardPic       :: Picture, -- ^ Imagem do tabuleiro.
    boardPicRegion :: Rect     -- ^ Região de desenho da imagem do tabuleiro.
    }

-- | Representa uma célula de um tabuleiro.
data Cell
    = Ground -- ^ A célula é uma célula vazia.
    | Target -- ^ A célula é um local de arrumação.
    | Wall   -- ^ A célula é uma parede.
    deriving (Eq)

--------------------------------------------------------------------------------

-- | Constrói um tabuleiro dadas as suas células e dimensões.
create :: Assets.Assets     -- ^ Assets.
       -> Assets.Theme      -- ^ Tema.
       -> ([Cell], VectorI) -- ^ (Células do tabuleiro, Dimensões do tabuleiro)
       -> IO Board
create assets theme (cells, size) = do

    (pic, picRegion) <- createBoardPic assets theme (cells, size)

    return Board {
        boardCells     = cells,
        boardSize      = size,

        boardPic       = pic,
        boardPicRegion = picRegion
        }

-- | Constrói a imagem de um tabuleiro.
createBoardPic :: Assets.Assets      -- ^ Assets.
               -> Assets.Theme       -- ^ Tema.
               -> ([Cell], VectorI)  -- ^ (Células do tabuleiro, Dimensões do
                                     -- tabuleiro)
               -> IO (Picture, Rect) -- ^ (Imagem do tabuleiro, Área de desenho
                                     -- da imagem do tabuleiro)
createBoardPic assets theme (cells, (w,h)) = do

    let tileCoords = [(x,y) | x <- [-15..w+15], y <- [-5..h+5]]
    tilePics <- mapM (createCellPic assets theme (cells, (w,h))) tileCoords

    let region = Rect
            (-8)
            (-8)
            (16 * fromIntegral w - 8)
            (16 * fromIntegral h - 8)

    return (Pictures tilePics, region)

-- | Constrói a imagem de uma célula de um tabuleiro.
createCellPic :: Assets.Assets     -- ^ Assets.
              -> Assets.Theme      -- ^ Tema.
              -> ([Cell], VectorI) -- ^ (Células do tabuleiro, Dimensões do
                                   -- tabuleiro)
              -> VectorI           -- ^ Coordenadas da célula.
              -> IO Picture        -- ^ Imagem da célula.
createCellPic assets theme cells (x,y) = do

    let cellCoords = [
            (x-1, y+1), (x  , y+1), (x+1,y+1),
            (x-1, y  ), (x  , y  ), (x+1,y  ),
            (x-1, y-1), (x  , y-1), (x+1,y-1)
            ]

    let cellToChar Ground = 'g'
        cellToChar Target = 'g'
        cellToChar Wall   = 'w'

    let pattern = map (cellToChar . getCell cells) cellCoords

    pic <-
        if getCell cells (x,y) == Target
            then Assets.targetBitmap theme assets
            else Assets.tileBitmap pattern theme assets

    return $ Translate
        (16 * fromIntegral x)
        (16 * fromIntegral y)
        pic

--------------------------------------------------------------------------------

-- | Obtém a célula de tabuleiro nas coordenadas especificadas.
--
-- Se as coordenadas especificadas se encontrarem fora do tabuleiro, é devolvido
-- o valor 'Wall'.
getCell :: ([Cell], VectorI) -- ^ (Células do tabuleiro, Dimensões do tabuleiro)
        -> VectorI           -- ^ Coordenadas da célula.
        -> Cell              -- ^ Célula nas coordenadas especificadas.
getCell (cells, (w,h)) (x,y) =
    if x >= 0 && y >= 0 && x < w && y < h
        then cells !! (x + (h-y-1)*w)
        else Wall

-- | Verifica se a célula nas coordenadas especificadas é __vazia__ ou um
-- __local de arrumação__.
cellIsEmpty :: Board   -- ^ Tabuleiro.
            -> VectorI -- ^ Coordenadas da célula.
            -> Bool
cellIsEmpty board pos =
    getCell (boardCells board, boardSize board) pos /= Wall

-- | Verifica se a célula nas coordenadas especificadas é um __local de
-- arrumação__.
cellIsTarget :: Board   -- ^ Tabuleiro.
             -> VectorI -- ^ Coordenadas da célula.
             -> Bool
cellIsTarget board pos =
    getCell (boardCells board, boardSize board) pos == Target

--------------------------------------------------------------------------------

-- | Determina região em que um tabuleiro será desenhado.
drawingRegion :: Board -> Rect
drawingRegion = boardPicRegion

-- | Desenha um tabuleiro.
draw :: Board -> Picture
draw = boardPic

--------------------------------------------------------------------------------
