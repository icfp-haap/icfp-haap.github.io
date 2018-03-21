--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Boxes
Description : Gere um conjunto de caixas.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o armazenamento e representação visual das caixas que o
utilizador deve movimentar para terminar o nível.
-}
module Sokoban.Game.Boxes (
    Boxes,
    create,
    boxPositions, hasBoxInPos,
    move, fakeMove, undo,
    update, draw
    ) where

import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.Assets        as Assets
import qualified Sokoban.Game.GameDefs as GameDefs

--------------------------------------------------------------------------------
-- * Tipos

-- | Representa um conjunto de caixas.
type Boxes = [Box]

-- | Representa uma caixa.
data Box = Box {
    boxPos          :: VectorI, -- Posição lógica da caixa.

    boxAnimPos      :: Vector, -- Posição de animação.
    boxAnimInitPos  :: Vector, -- Posição inicial de animação.
    boxAnimFinalPos :: Vector, -- Posição final de animação.

    boxPic          :: Picture, -- Imagem da caixa.

    boxSteps        :: [Step]
    }

-- | Armazena informação sobre um movimento de uma caixa.
data Step = Step {
    stepInvDir :: VectorI -- Direção oposta ao movimento.
    }

--------------------------------------------------------------------------------
-- * Construção

-- | Contrói um valor do tipo 'Boxes', dadas as posições iniciais de todas as
-- caixas.
create :: Assets.Assets -- ^ Assets.
       -> Assets.Theme  -- ^ Tema.
       -> [VectorI]     -- ^ Posições iniciais da caixa.
       -> IO Boxes
create assets theme boxPos = mapM (createBox assets theme) boxPos

-- | Contrói um valor do tipo 'Box', dada a posição inicial da caixa.
createBox :: Assets.Assets -- ^ Assets.
          -> Assets.Theme  -- ^ Tema.
          -> VectorI       -- Posição inicial da caixa.
          -> IO Box
createBox assets theme pos = do

    pic <- Assets.boxBitmap theme assets

    let animPos = GameDefs.tileSize |*| fromIntegralVector pos

    return Box {
        boxPos          = pos,

        boxAnimInitPos  = animPos,
        boxAnimFinalPos = animPos,
        boxAnimPos      = animPos,

        boxPic          = pic,

        boxSteps        = []
        }

--------------------------------------------------------------------------------
-- * Estado

-- | Determina as coordenadas das caixas representadas por um valor do tipo
-- 'Boxes'.
boxPositions :: Boxes -> [VectorI]
boxPositions = map boxPos

-- | Determina se existe uma caixa nas coordenadas especificadas num valor do
-- tipo 'Boxes'.
hasBoxInPos :: VectorI -- ^ Coordenadas a verificar.
            -> Boxes   -- ^ Caixas.
            -> Bool
hasBoxInPos pos = (pos `elem`) . boxPositions

--------------------------------------------------------------------------------
-- * Ações

-- | Move as caixas que se encontram nas posições especificadas por um célula na
-- direção especificada.
move :: VectorI   -- ^ Direção do movimento.
     -> [VectorI] -- ^ Coordenadas das caixas a movimentar.
     -> Boxes
     -> Boxes
move dir boxesToMove = map (moveBox dir boxesToMove)

-- | Utilizado quando não se quer mover nenhuma caixa, mas se quer registar um
-- movimento nulo, que pode depois ser anulado.
fakeMove :: Boxes -> Boxes
fakeMove = move undefined []

-- | Move uma caixa por uma célula na direção especificada, se as coordenadas
-- desta se encontrarem na lista especificada.
moveBox :: VectorI   -- ^ Direção do movimento.
        -> [VectorI] -- ^ Coordenadas das caixas a movimentar.
        -> Box
        -> Box
moveBox dir boxesToMove box =
    if pos `elem` boxesToMove
        then box {
            boxPos = newPos,

            boxAnimPos      = animPos,
            boxAnimInitPos  = animPos,
            boxAnimFinalPos = newAnimPos,

            boxSteps        = Step { stepInvDir = vecNeg dir } : boxSteps box
            }
        else box {
            boxAnimPos      = animPos,
            boxAnimInitPos  = animPos,
            boxAnimFinalPos = animPos,

            boxSteps        = Step { stepInvDir = (0,0) } : boxSteps box
            } 
    where
        pos    = boxPos box
        newPos = pos |+| dir

        animPos    = GameDefs.tileSize |*| fromIntegralVector pos
        newAnimPos = GameDefs.tileSize |*| fromIntegralVector newPos

-- | Anula o último movimento das caixas.
undo :: Boxes -> Boxes
undo = map undoBox

-- | Anula o último movimento de uma caixa.
undoBox :: Box -> Box
undoBox box =
    box {
        boxPos = newPos,

        boxAnimPos      = animPos,
        boxAnimInitPos  = animPos,
        boxAnimFinalPos = newAnimPos,

        boxSteps        = tail (boxSteps box)
        }
    where
        invDir = stepInvDir $ head (boxSteps box)

        pos    = boxPos box
        newPos = pos |+| invDir

        animPos    = GameDefs.tileSize |*| fromIntegralVector pos
        newAnimPos = GameDefs.tileSize |*| fromIntegralVector newPos

--------------------------------------------------------------------------------
-- * Atualização

-- | Atualiza o estado de um conjunto de caixas.
update :: Float -- ^ Fator, de @0@ a @1@, que determina o estado da animação
                -- atual.
       -> Boxes
       -> Boxes
update animFactor = map (updateBox animFactor)

-- | Atualiza o estado de uma caixa.
updateBox :: Float -- ^ Fator, de @0@ a @1@, que determina o estado da animação
                   -- atual.
          -> Box
          -> Box
updateBox animFactor box = box {
    boxAnimPos = lerp2 animFactor (boxAnimInitPos box) (boxAnimFinalPos box)
    }

--------------------------------------------------------------------------------
-- * Conversão para 'Picture'

-- | Desenha um conjunto de caixas.
draw :: Boxes -> Picture
draw = Pictures . map drawBox

-- | Desenha uma caixa.
drawBox :: Box -> Picture
drawBox box = Translate x y $ boxPic box
    where (x,y) = boxAnimPos box

--------------------------------------------------------------------------------
