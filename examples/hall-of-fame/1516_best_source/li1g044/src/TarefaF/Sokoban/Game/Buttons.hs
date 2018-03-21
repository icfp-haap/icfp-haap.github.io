--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Buttons
Description : Botões de controlo do jogo.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o painel de botões que permite efetuar ações sobre o estado do
jogo, e.g. anular uma jogada ou desistir e passar ao próximo nível.
-}
module Sokoban.Game.Buttons (
    Buttons,
    Result (..),
    createPlaying, createFinished, createNext,
    handleEvent, drawingRegion, draw
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets    as Assets
import qualified Sokoban.UI.Button as Button

--------------------------------------------------------------------------------

-- | Dimensões de cada botão.
buttonSize :: Vector
buttonSize = (buttonWidth, buttonHeight)

-- | Largura de cada botão.
buttonWidth :: Float
buttonWidth = 330

-- | Altura de cada botão.
buttonHeight :: Float
buttonHeight = 70

-- | Espaço livre entre dois botões.
buttonSpacing :: Float
buttonSpacing = 20

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa um painel de botões.
data Buttons
    = GameButtonsPlaying {
        buttonsPlayingUndo    :: Button.Button, -- ^ Botão que anula uma jogada.
        buttonsPlayingRestart :: Button.Button, -- ^ Botão que recomeça o jogo.
        buttonsPlayingGiveUp  :: Button.Button  -- ^ Botão que termina o jogo
                                                -- por desistência.
        }
    | GameButtonsNext {
        buttonsNextNext :: Button.Button, -- ^ Botão que passa ao próximo nível.
        buttonsNextQuit :: Button.Button  -- ^ Botão que termina a fase do jogo,
                                          -- regressando-se ao menu principal.
        }
    | GameButtonsFinished {
        buttonsFinishedQuit :: Button.Button -- ^ Botão que termina a fase do
                                             -- jogo, regressando-se ao menu
                                             -- principal.
        }

-- | Representa o resultado de premir num dos botões de um painel de botões.
data Result
    = None    -- ^ Nenhum dos botões foi premido.
    | Undo    -- ^ Deve-se anular uma jogada.
    | Restart -- ^ O jogo deve recomeçar.
    | GiveUp  -- ^ O jogo deve terminar por desistência.
    | Next    -- ^ Deve-se passar ao próximo nível.
    | Quit    -- ^ Deve-se regressar ao menu principal.

--------------------------------------------------------------------------------

-- | Contrói um painel com os botões de anular uma jogada, recomeçar o jogo e
-- terminar o jogo por desistência.
createPlaying :: Assets.Assets -- ^ Assets.
              -> Buttons
createPlaying assets =
    GameButtonsPlaying {
        buttonsPlayingUndo    = undoButton,
        buttonsPlayingRestart = restartButton,
        buttonsPlayingGiveUp  = giveUpButton
        }
    where
        undoButton =
            Button.create
                assets
                (makeRectAt (-buttonWidth - buttonSpacing, 0) buttonSize)
                "Anular"
                (Char 'u')
        restartButton =
            Button.create
                assets
                (makeRectAt (0,0) buttonSize)
                "Recomeçar"
                (Char 'r')
        giveUpButton =
            Button.create
                assets
                (makeRectAt (buttonWidth + buttonSpacing, 0) buttonSize)
                "Desistir"
                (SpecialKey KeyEsc)

-- | Contrói um painel com os botões de passar ao próximo nível e de regressar
-- ao menu principal.
createNext :: Assets.Assets -- ^ Assets.
           -> Buttons
createNext assets =
    GameButtonsNext {
        buttonsNextNext = nextButton,
        buttonsNextQuit = quitButton
        }
    where
        nextButton =
            Button.create
                assets
                (makeRectAt ((buttonWidth + buttonSpacing) / (-2), 0) buttonSize)
                "Próximo"
                (SpecialKey KeyEnter)
        quitButton =
            Button.create
                assets
                (makeRectAt ((buttonWidth + buttonSpacing) / 2, 0) buttonSize)
                "Sair"
                (SpecialKey KeyEsc)

-- | Contrói um painel com o botão de regressar ao menu principal.
createFinished :: Assets.Assets -- ^ Assets.
               -> Buttons
createFinished assets =
    GameButtonsFinished {
        buttonsFinishedQuit = quitButton
        }
    where
        quitButton =
            Button.create
                assets
                (makeRectAt (0,0) buttonSize)
                "Sair"
                (SpecialKey KeyEsc)

--------------------------------------------------------------------------------

-- | Processa um evento.
--
-- É devolvido um tuplo com o novo painel de botões e o resultado do
-- processamento do evento.
handleEvent :: Event             -- ^ Evento.
            -> RectMapping       -- ^ Transformação entre espaço do painel de
                                 -- botões e espaço da janela.
            -> Buttons           -- ^ Painel de botões.
            -> (Buttons, Result)

handleEvent event rectMap buttons@(GameButtonsPlaying {}) = (
        buttons {
            buttonsPlayingUndo    = newUndoButton,
            buttonsPlayingRestart = newRestartButton,
            buttonsPlayingGiveUp  = newGiveUpButton
            },
        result
        )
    where
        undoButton    = buttonsPlayingUndo    buttons
        restartButton = buttonsPlayingRestart buttons
        giveUpButton  = buttonsPlayingGiveUp  buttons

        (newUndoButton, pressedUndo) =
            Button.handleEvent event rectMap undoButton
        (newRestartButton, pressedRestart) =
            Button.handleEvent event rectMap restartButton
        (newGiveUpButton, pressedGiveUp) =
            Button.handleEvent event rectMap giveUpButton

        result
            | pressedUndo    = Undo
            | pressedRestart = Restart
            | pressedGiveUp  = GiveUp
            | otherwise      = None

handleEvent event rectMap buttons@(GameButtonsNext {}) = (
        buttons {
            buttonsNextNext = newNextButton,
            buttonsNextQuit = newQuitButton
            },
        result
        )
    where
        nextButton = buttonsNextNext buttons
        quitButton = buttonsNextQuit buttons

        (newNextButton, pressedNext) =
            Button.handleEvent event rectMap nextButton
        (newQuitButton, pressedQuit) =
            Button.handleEvent event rectMap quitButton

        result
            | pressedNext = Next
            | pressedQuit = Quit
            | otherwise   = None

handleEvent event rectMap buttons@(GameButtonsFinished {}) = (
        buttons {
            buttonsFinishedQuit = newQuitButton
            },
        result
        )
    where
        quitButton = buttonsFinishedQuit buttons

        (newQuitButton, pressedQuit) =
            Button.handleEvent event rectMap quitButton

        result
            | pressedQuit = Quit
            | otherwise   = None

--------------------------------------------------------------------------------

-- | Determina a região em que um painel de botões será desenhado.
drawingRegion :: Rect
drawingRegion = makeRect (
    3*buttonWidth + 4*buttonSpacing,
    buttonHeight + 2*buttonSpacing
    )

-- | Desenha um painel de botões.
draw :: Buttons -> Picture

draw buttons@(GameButtonsPlaying {}) = Pictures [
    Button.draw $ buttonsPlayingUndo    buttons,
    Button.draw $ buttonsPlayingRestart buttons,
    Button.draw $ buttonsPlayingGiveUp  buttons
    ]

draw buttons@(GameButtonsNext {}) = Pictures [
    Button.draw $ buttonsNextNext buttons,
    Button.draw $ buttonsNextQuit buttons
    ]

draw buttons@(GameButtonsFinished {}) = Pictures [
    Button.draw $ buttonsFinishedQuit buttons
    ]

--------------------------------------------------------------------------------
