--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.UI.Button
Description : Botões interativos.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo permite a utilização botões interativos na interface gráfica do
utilizador.
-}
module Sokoban.UI.Button (
    Button,
    create,
    handleEvent, draw
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets      as Assets
import qualified Sokoban.UI.Font     as Font
import qualified Sokoban.UI.UIDefs   as UIDefs
import qualified Sokoban.UI.UIHelper as UIHelper

--------------------------------------------------------------------------------

-- $
-- Um botão é constituído por uma imagem de fundo comum a todos os botões e
-- algum texto que identifica o botão. O utilizador pode interagir com o botão
-- utilizando o rato ou carregando na tecla associada a esse botão.

-- | Tipo /abstrato/ que representa um botão.
data Button = Button {
    buttonRegion      :: Rect,        -- ^ Região onde o botão será desenhado.
    buttonBackPicUp   :: Picture,     -- ^ Imagem do botão quando o cursor se
                                      -- encontra fora da região do botão.
    buttonBackPicOver :: Picture,     -- ^ Imagem do botão quando o cursor se
                                      -- encontra dentro da região do botão.
    buttonBackPicDown :: Picture,     -- ^ Imagem do botão quando o cursor se
                                      -- encontra dentro da região do botão e o
                                      -- botão esquerdo do rato está premido.
    buttonTextPic     :: Picture,     -- ^ Texto do botão.
    buttonKey         :: Key,         -- ^ Tecla associada com o botão.
    buttonState       :: ButtonState, -- ^ Estado a atual do botão.
    buttonMouseDown   :: Bool         -- ^ Define se o botão esquerdo do rato
                                      -- foi pressionado dentro da sua região.     
    }

-- | Representa o estado de um botão.
data ButtonState
    = ButtonStateUp   -- ^ O cursor encontra-se __fora__ da região do botão.
    | ButtonStateOver -- ^ O cursor encontra-se __dentro__ da região do botão e
                      -- o botão esquerdo do rato __não__ está premido.
    | ButtonStateDown -- ^ O cursor encontra-se __dentro__ da região do botão e
                      -- o botão esquerdo do rato está premido.

--------------------------------------------------------------------------------

-- | Contrói um botão.
create :: Assets.Assets -- ^ Assets.
       -> Rect          -- ^ Região onde o botão será desenhado.
       -> String        -- ^ Texto do botão.
       -> Key           -- ^ Tecla associada com o botão.
       -> Button        -- ^ Botão resultante.
create assets region text key =
    Button {
        buttonRegion      = region,
        buttonBackPicUp   = backPicUp,
        buttonBackPicOver = backPicOver,
        buttonBackPicDown = backPicDown,
        buttonTextPic     = textPic,
        buttonKey         = normalizeKey key,
        buttonState       = ButtonStateUp,
        buttonMouseDown   = False
        }
    where
        textString = text ++ " [" ++ keyToString key ++ "]"
        textText = Font.Text
            textString
            (0.44 * rectHeight region)
            Font.Center
            UIDefs.mainTextColor
            UIDefs.mainTextShadowColor
        backPicUp   = UIHelper.createBackground assets region UIHelper.Light
        backPicOver = UIHelper.createBackground assets region UIHelper.Dark
        backPicDown = UIHelper.createBackground assets region UIHelper.Darker
        textPic = Font.draw (rectCenter region) (Assets.uiFont assets) textText

--------------------------------------------------------------------------------

-- | Processa um evento.
--
-- É devolvido um tuplo com o novo botão e um booleano que indica se o botão
-- acabou de ser premido.
handleEvent :: Event          -- ^ Evento.
            -> RectMapping    -- ^ Transformação entre espaço do botão e espaço
                              -- da janela.
            -> Button         -- ^ Botão.
            -> (Button, Bool)

handleEvent (EventMotion mp) rectMap button =
    (
        button {
            buttonState = state
            },
        False
        )
    where
        mouseIsInside = pointIsInside mp rectMap button
        state
            | not mouseIsInside      = ButtonStateUp
            | buttonMouseDown button = ButtonStateDown
            | otherwise              = ButtonStateOver

handleEvent (EventKey (MouseButton LeftButton) Down _ mp) rectMap button =
    (
        button {
            buttonState     = state,
            buttonMouseDown = mouseIsInside
            },
        False
        )
    where
        mouseIsInside = pointIsInside mp rectMap button
        state =
            if mouseIsInside
                then ButtonStateDown
                else ButtonStateUp

handleEvent (EventKey (MouseButton LeftButton) Up _ mp) rectMap button =
    (
        button {
            buttonState     = state,
            buttonMouseDown = False
            },
        buttonMouseDown button && mouseIsInside
        )
    where
        mouseIsInside = pointIsInside mp rectMap button
        state =
            if mouseIsInside
                then ButtonStateOver
                else ButtonStateUp

handleEvent (EventKey key Down _ _) _ button =
    (
        button,
        normalizeKey key == buttonKey button
        )

handleEvent _ _ button =
    (
        button,
        False
        )

-- | Verifica se um ponto se encontra dentro da região de um botão, tendo em
-- conta a transformação especificada.
pointIsInside :: Vector      -- ^ Ponto.
              -> RectMapping -- ^ Transformação entre espaço do botão e espaço
                             -- da janela.
              -> Button      -- ^ Botão.
              -> Bool
pointIsInside mousePos rectMap button =
    mousePos `pointIsInsideRect` (rectMap $ buttonRegion button)

--------------------------------------------------------------------------------

-- | Desenha um botão.
draw :: Button -> Picture
draw button = Pictures [backPic, buttonTextPic button]
    where
        backPic =
            case buttonState button of
                ButtonStateUp   -> buttonBackPicUp   button
                ButtonStateOver -> buttonBackPicOver button
                ButtonStateDown -> buttonBackPicDown button

--------------------------------------------------------------------------------
