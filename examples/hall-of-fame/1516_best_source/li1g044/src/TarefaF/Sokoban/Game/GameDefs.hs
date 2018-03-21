--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.GameDefs
Description : Definições relativas à fase do jogo.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém constantes e definições relativas à fase do jogo.
-}
module Sokoban.Game.GameDefs (
    tileSize,
    animDuration, undoAnimDuration, restartAnimDuration
    ) where

--------------------------------------------------------------------------------

-- | Dimensões, em píxeis, de uma tile.
tileSize :: (Num a) => (a, a)
tileSize = (16, 16)

-- | Duração da animação do movimento do jogador e das caixas quando o
-- utilizador movimenta o jogador.
animDuration :: Float
animDuration = 0.17

-- | Duração da animação do movimento do jogador e das caixas quando se anula
-- uma jogada.
undoAnimDuration :: Float
undoAnimDuration = 0.05

-- | Duração da animação de cada movimento do jogador e das caixas quando se
-- anula todas as jogadas já feitas.
restartAnimDuration :: Float
restartAnimDuration = 0.03

--------------------------------------------------------------------------------
