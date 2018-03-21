--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.UI.UIDefs
Description : Definições relativas à UI.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém constantes e definições relativas à Interface do Utilizador
(UI).
-}
module Sokoban.UI.UIDefs (
    initialResolution,

    backgroundColor,
    mainTextColor, mainTextShadowColor,
    secTextColor, secTextShadowColor,
    supportedTextColors
    ) where

import Data.Maybe

import Graphics.Gloss.Data.Color

import Sokoban.Helper

--------------------------------------------------------------------------------
-- * Visualização

-- | Resolução inicial da janela.
initialResolution :: VectorI
initialResolution = (800, 600)

--------------------------------------------------------------------------------
-- * Esquema de Cores

-- | Cor de fundo da janela.
backgroundColor :: Color
backgroundColor = makeColorI 100 149 237 255

-- | Cor do texto principal.
mainTextColor :: Color
mainTextColor = makeColorI 96 96 96 255

-- | Cor da sombra do texto principal.
mainTextShadowColor :: Maybe Color
mainTextShadowColor = Just $ makeColorI 208 208 208 255

-- | Cor do texto secundário.
secTextColor :: Color
secTextColor = makeColorI 208 208 208 255

-- | Cor da sombra do texto secundário.
secTextShadowColor :: Maybe Color
secTextShadowColor = Nothing

-- | Lista de cores de texto suportadas.
--
-- Esta lista é utilizada ao carregar o tipo de letra do jogo. Todo o texto no
-- jogo terá de ter uma destas cores.
supportedTextColors :: [Color]
supportedTextColors =
    [mainTextColor, secTextColor] ++
    catMaybes [mainTextShadowColor, secTextShadowColor]

--------------------------------------------------------------------------------
