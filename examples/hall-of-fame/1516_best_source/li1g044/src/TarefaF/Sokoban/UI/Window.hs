--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.UI.Window
Description : Estado da janela.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo destina-se a permitir armazenar e atualizar informação sobre o
estado da janela.
-}
module Sokoban.UI.Window (
    Window,
    create,
    size, region,
    handleEvent
    ) where

import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que armazena informação sobre a janela.
newtype Window = Window Vector

-- | Cria um valor do tipo 'Window' com a resolução especificada.
create :: VectorI -> Window
create res = Window (fromIntegralVector res)

--------------------------------------------------------------------------------

-- | Resolução atual da janela.
size :: Window -> Vector
size (Window res) = res

-- | Região da janela disponível para desenho.
region :: Window -> Rect
region (Window res) = makeRect res

--------------------------------------------------------------------------------

-- | Processa um evento.
handleEvent :: Event  -- ^ Evento.
            -> Window
            -> Window
handleEvent (EventResize newRes) _      = Window (fromIntegralVector newRes)
handleEvent _                    window = window

--------------------------------------------------------------------------------
