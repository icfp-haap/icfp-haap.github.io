--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Menu.PlayerSelector
Description : Selecionador de personagem.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo permite a utilização de um selecionador de personagem interativo na
interface gráfica do utilizador.
-}
module Sokoban.Menu.PlayerSelector (
    PlayerSelector,
    create,
    selection,
    update, handleEvent, draw
    ) where

import Data.List

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets          as Assets
import qualified Sokoban.Game.PlayerAttr as PlayerAttr
import qualified Sokoban.UI.Font         as Font
import qualified Sokoban.UI.UIDefs       as UIDefs

--------------------------------------------------------------------------------

-- | Dimensões de cada visualizador de personagem.
itemSize :: Vector
itemSize = (300, 130)

-- | Espaço entre cada visualizador de personagem.
itemSpacing :: Float
itemSpacing = 10

-- | Duração da animação do personagem selecionado.
itemAnimDuration :: Float
itemAnimDuration = 0.8

-- | Cor do retângulo envolvente da visualizador de personagem selecionada.
selectionColor :: Color
selectionColor = makeColor 1 1 1 0.6

-- | Cor do retângulo envolvente da visualizador de personagem em que o cursor
-- se encontra.
mouseSelectionColor :: Color
mouseSelectionColor = makeColor 1 1 1 0.3

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa um selecionador de personagem.
data PlayerSelector = PlayerSelector {
    selectorTextPic        :: Picture,      -- ^ Imagem do texto de cabeçalho.
    selectorItems          :: [Item],       -- ^ Visualizadores dos personagens.

    selectorSelection      :: VectorI,      -- ^ Coordenadas do visualizador
                                            -- atualmente selecionado.
    selectorMouseSelection :: Maybe VectorI -- ^ Coordenadas do visualizador
                                            -- sobre o qual o cursor se
                                            -- encontra.
    }

-- | Representa a área de um personagem.
data Item = Item {
    itemCoords   :: VectorI,            -- ^ Coordenadas que identificam o
                                        -- visualizador.
    itemRegion   :: Rect,               -- ^ Região que o visualizador ocupa.

    itemAnimTime :: Float,              -- ^ Tempo atual de animação.
    itemPicFun   :: (Float -> Picture), -- ^ Função que obtém a imagem do
                                        -- personagem dado um determinado tempo
                                        -- de animação.
    itemPic      :: Picture,            -- ^ Imagem atual do personagem.

    itemText     :: Picture             -- ^ Imagem do texto do visualizador.
    }

-- | Seleção atual de um selecionador de personagem.
selection :: PlayerSelector  -- ^ O selecionador de personagem.
          -> PlayerAttr.PlayerAttr -- ^ Os atributos correspondentes ao personagem
                             -- selecionado.
selection selector =
    PlayerAttr.attributes !! (coordsIndex $ selectorSelection selector)

-- | Faz corresponder um índice a um par de coordenadas que identificam um
-- visualizador.
coordsIndex :: VectorI -> Int
coordsIndex (x,y) = (x + y*2)

-- | Verifica sobre que visualizador o cursor se encontra.
mouseSelection :: PlayerSelector -- ^ Selecionador de personagem.
               -> Vector         -- ^ Posição do cursor no espaço da janela.
               -> RectMapping    -- ^ Transformação entre espaço do selecionador 
                                 -- e espaço da janela.
               -> Maybe VectorI  -- ^ Coordenadas do visualizador.
mouseSelection selector mousePos rectMap =
    fmap itemCoords $ find
        ((mousePos `pointIsInsideRect`) . rectMap . itemRegion)
        (selectorItems selector)

--------------------------------------------------------------------------------

-- | Contrói um selecionador de personagem.
create :: Assets.Assets  -- ^ Assets.
       -> Vector         -- ^ Posição do centro do selecionador.
       -> PlayerSelector
create assets pos =
    PlayerSelector {
        selectorTextPic        = textPic,
        selectorItems          = map (createItem assets) attrs,

        selectorSelection      = (0,0),
        selectorMouseSelection = Nothing
        }
    where
        textPic  = Font.draw (pos |+| (0, 175)) (Assets.uiFont assets) textText
        textText =
            Font.Text
                "Seleciona um jogador:"
                20
                Font.Center
                UIDefs.mainTextColor
                UIDefs.mainTextShadowColor

        (w,h) = itemSize
        itemRegions = [
            makeRectAt (pos |+| ((-w-itemSpacing)/2, ( h+itemSpacing)/2)) (w,h),
            makeRectAt (pos |+| (( w+itemSpacing)/2, ( h+itemSpacing)/2)) (w,h),
            makeRectAt (pos |+| ((-w-itemSpacing)/2, (-h-itemSpacing)/2)) (w,h),
            makeRectAt (pos |+| (( w+itemSpacing)/2, (-h-itemSpacing)/2)) (w,h)
            ]

        attrs = zip3
            [(0,0), (1,0), (0,1), (1,1)]
            itemRegions
            PlayerAttr.attributes

-- | Contrói um visualizador.
createItem :: Assets.Assets                    -- ^ Assets.
           -> (VectorI, Rect, PlayerAttr.PlayerAttr) -- ^ (Coordenadas, Região,
                                               -- Atributos do personagem)
           -> Item
createItem assets (coords, region, attr) =
    Item {
        itemCoords   = coords,
        itemRegion   = region,

        itemAnimTime = 0,
        itemPicFun   = picFun,
        itemPic      = picFun 0,

        itemText     = textPic
        }
    where
        (x,y) = rectCenter region

        picFun = createItemPic
            (\n -> Assets.playerBitmap (PlayerAttr.bitmapName attr) n assets)
            (x-115, y+13)

        nameText = Font.Text
            (PlayerAttr.name attr)
            20
            Font.LeftCenter
            UIDefs.mainTextColor
            UIDefs.mainTextShadowColor
        descText = Font.Text
            (PlayerAttr.description attr)
            12
            Font.LeftCenter
            UIDefs.secTextColor
            UIDefs.secTextShadowColor

        font    = Assets.uiFont assets
        textPic = Pictures [
            Font.draw (x-75, y+45) font nameText,
            Font.draw (x-75, y-15) font descText
            ]

-- | Obtém a imagem de um personagem dado um tempo de animação.
createItemPic :: (String -> Picture) -- ^ Função que obtém uma imagem do
                                     -- personagem dado o nome dessa imagem.
              -> Vector              -- ^ Posição do centro da imagem.
              -> Float               -- ^ Tempo de animação.
              -> Picture             -- ^ Imagem do personagem.
createItemPic fun (x,y) time =
    Translate x y . Scale scale scale $ fun name
    where
        scale  = 40 / 16
        factor = time / itemAnimDuration
        name
            | factor == 0    = "down_idle"
            | factor <  0.25 = "down_walk0"
            | factor <  0.5  = "down_idle"
            | factor <  0.75 = "down_walk1"
            | otherwise      = "down_idle"

--------------------------------------------------------------------------------

-- | Processa um evento.
handleEvent :: Event          -- ^ Evento.
            -> RectMapping    -- ^ Transformação entre espaço do selecionador 
                              -- e espaço da janela.
            -> PlayerSelector -- ^ Selecionador de personagem.
            -> PlayerSelector

handleEvent (EventMotion mousePos) rectMap selector =
    selector {
        selectorMouseSelection = mouseSelection selector mousePos rectMap
        }

handleEvent (EventKey (MouseButton LeftButton) _ _ mousePos) rectMap selector =
    maybe
        selector
        (\s -> selector { selectorSelection = s })
        (mouseSelection selector mousePos rectMap)

handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) _ selector =
    selector { selectorSelection = (0, y) }
    where (_,y) = selectorSelection selector

handleEvent (EventKey (SpecialKey KeyDown) _ _ _) _ selector =
    selector { selectorSelection = (x, 1) }
    where (x,_) = selectorSelection selector

handleEvent (EventKey (SpecialKey KeyRight) _ _ _) _ selector =
    selector { selectorSelection = (1, y) }
    where (_,y) = selectorSelection selector

handleEvent (EventKey (SpecialKey KeyUp) _ _ _) _ selector =
    selector { selectorSelection = (x, 0) }
    where (x,_) = selectorSelection selector

handleEvent _ _ selector = selector

--------------------------------------------------------------------------------

-- | Atualiza o estado de um selecionador de personagem.
update :: Float          -- ^ Tempo desde a última atualização, em segundos.
       -> PlayerSelector -- ^ Selecionador de personagem.
       -> PlayerSelector
update dt selector =
    selector {
        selectorItems = map
            (updateItem dt (selectorSelection selector))
            (selectorItems selector)
        }

-- | Atualiza o estado de um visualizador.
updateItem :: Float         -- ^ Tempo desde a última atualização, em segundos.
           -> VectorI       -- ^ Seleção atual.
           -> Item          -- ^ Visualizador.
           -> Item
updateItem dt selection item =
    item {
        itemAnimTime = newTime,
        itemPic      = (itemPicFun item) newTime
        }
    where
        incrTime =
            if selection == itemCoords item
                then itemAnimTime item + dt
                else 0

        newTime =
            if incrTime > itemAnimDuration
                then 0
                else incrTime

--------------------------------------------------------------------------------

-- | Desenha um selecionador de personagem.
draw :: PlayerSelector -> Picture
draw selector =
    Pictures $
        concatMap drawItem (selectorItems selector) ++
        mouseSelectionRect ++
        [selectorTextPic selector, selectionRect]
    where
        rect color selection = Color color $ rectWirePicture
            (itemRegion $ selectorItems selector !! coordsIndex selection)

        selectionRect = rect selectionColor (selectorSelection selector)

        mouseSelectionRect = maybe
            []
            (\selection -> [rect mouseSelectionColor selection])
            (selectorMouseSelection selector)

-- | Desenha um visualizador.
drawItem :: Item -> [Picture]
drawItem item = [itemPic item, itemText item]

--------------------------------------------------------------------------------
