--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.UI.UIHelper
Description : Tipos e funções auxiliares à UI.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém tipos e funções úteis a vários fins relacionados com a
interface do utilizador (UI).
-}
module Sokoban.UI.UIHelper (
    BackgroundStyle (..),
    createBackground
    ) where

import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.Assets as Assets

--------------------------------------------------------------------------------
-- * Background

-- | Define o estilo de um background.
data BackgroundStyle
    = Light  -- ^ Background claro.
    | Dark   -- ^ Background escuro.
    | Darker -- ^ Background ainda mais escuro.

-- | Cria uma imagem de fundo que pode ser utilizada em várias situações.
createBackground :: Assets.Assets   -- ^ Assets.
                 -> Rect            -- ^ Região onde a imagem será desenhada.
                 -> BackgroundStyle -- ^ Estilo do background.
                 -> Picture         -- ^ Imagem resultante.
createBackground assets region style =
    Pictures [
        Translate cx cy . Scale wMid scl $ midPic,
        Translate (rectLeft  region + wLeft/2) cy . Scale    scl scl $ leftPic,
        Translate (rectRight region - wLeft/2) cy . Scale (-scl) scl $ leftPic
        ]
    where
        nameSuffix =
            case style of
                Light  -> "light"
                Dark   -> "dark"
                Darker -> "darker"
        leftPic = Assets.uiBitmap ("background_left_"   ++ nameSuffix) assets
        midPic  = Assets.uiBitmap ("background_middle_" ++ nameSuffix) assets
        (wLeftPic, hPic) = bitmapSize leftPic |-| (2,2)
        (cx,cy) = rectCenter region
        (w,h)   = rectSize region
        scl     = h / hPic
        wLeft   = wLeftPic * scl
        wMid    = w - wLeft

--------------------------------------------------------------------------------
