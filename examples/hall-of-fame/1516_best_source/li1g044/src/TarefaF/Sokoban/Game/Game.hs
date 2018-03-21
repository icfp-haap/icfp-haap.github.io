--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Game
Description : Fase do jogo.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere a fase do jogo, durante a qual o utilizador movimenta um
jogador para resolver níveis.
-}
module Sokoban.Game.Game (
    Game,
    create,
    handleEvent, update, draw
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets          as Assets
import qualified Sokoban.Game.Buttons    as Buttons
import qualified Sokoban.Game.Level      as Level
import qualified Sokoban.Game.PlayerAttr as PlayerAttr
import qualified Sokoban.Game.Stats      as Stats
import qualified Sokoban.Shared          as Shared
import qualified Sokoban.UI.Window       as Window

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa a fase do jogo, onde o utilizador pode
-- movimentar o personagem ao longo de vários níveis.
data Game = Game {
    gameLevels     :: [String],              -- ^ Níveis a serem jogados.
    gamePlayerAttr :: PlayerAttr.PlayerAttr, -- ^ Atributos do personagem a ser
                                             -- utilizado.

    gameLevel      :: Level.Level,    -- ^ Nível atual.
    gameStats      :: Stats.Stats,    -- ^ Stats.
    gameButtons    :: Buttons.Buttons -- ^ Botões.
    }

--------------------------------------------------------------------------------

-- | Constrói um valor do tipo 'Game', dados os níveis a serem jogados e os
-- atributos do personagem a utilizar.
create :: Shared.Shared         -- ^ Estado partilhado.
       -> [String]              -- ^ Níveis a serem jogados.
       -> PlayerAttr.PlayerAttr -- ^ Atributos do personagem a utilizar.
       -> IO Game
create shared levels playerAttr = do

    let assets = Shared.assets shared

    level <- Level.create assets (head levels) playerAttr
    stats <- Stats.create assets (length levels)
    let buttons = Buttons.createPlaying assets

    return Game {
        gameLevels     = tail levels,
        gamePlayerAttr = playerAttr,

        gameLevel      = level,
        gameStats      = stats,
        gameButtons    = buttons
        }

--------------------------------------------------------------------------------

-- | Percentagem mínima da altura da janela que o nível deve ocupar.
minLevelRegionHeightFactor :: Float
minLevelRegionHeightFactor = 0.8

-- | Calcula a região da janela que o nível ocupa.
dstLevelRegion :: Shared.Shared -- ^ Estado partilhado.
               -> Rect
dstLevelRegion shared = makeRect (wWind, hWind - h*2)
    where
        statsReg =
            zoomRegionToTop Stats.drawingRegion (dstGameStatsRegion shared)
        buttonsReg =
            zoomRegionToBottom Buttons.drawingRegion (dstButtonsRegion shared)

        h = max (rectHeight statsReg) (rectHeight buttonsReg)

        (wWind, hWind) = Window.size . Shared.window $ shared

-- | Calcula a região da janela que os stats ocupam.
dstGameStatsRegion :: Shared.Shared -- ^ Estado partilhado.
                   -> Rect
dstGameStatsRegion shared = makeRectAt (0, (hWind-h) / 2) (wWind, h)
    where
        (wWind, hWind) = Window.size . Shared.window $ shared
        h = hWind * ((1 - minLevelRegionHeightFactor) / 2)

-- | Calcula a região da janela que os botões ocupam.
dstButtonsRegion :: Shared.Shared -- ^ Estado partilhado.
                 -> Rect
dstButtonsRegion shared = makeRectAt (0, (h-hWind) / 2) (wWind, h)
    where
        (wWind, hWind) = Window.size . Shared.window $ shared
        h = hWind * ((1 - minLevelRegionHeightFactor) / 2)

-- | Avança para o próximo nível.
advanceLevel :: Assets.Assets -- ^ Assets.
             -> Game
             -> IO Game
advanceLevel assets game = do

    let levels = gameLevels game

    newLevel <- Level.create assets (head levels) (gamePlayerAttr game)
    newStats <- Stats.advanceLevel assets (gameStats game)
    let newButtons = Buttons.createPlaying assets

    return game {
        gameLevels = tail levels,

        gameLevel   = newLevel,
        gameStats   = newStats,
        gameButtons = newButtons
        }

--------------------------------------------------------------------------------

-- | Processa um evento.
--
-- Se o 'Game' resultante já tiver terminado (i.e. está pronto para voltar ao
-- menu), é devolvido o valor 'Nothing'. Em caso contrário, é devolvido o valor
-- @'Just' newGame@, onde @newGame@ corresponde ao novo estado do 'Game'.
handleEvent :: Shared.Shared   -- ^ Estado partilhado.
            -> Event           -- ^ Evento.
            -> Game
            -> IO (Maybe Game)
handleEvent shared event game = do

    let assets = Shared.assets shared

    let newLevel = Level.handleEvent event (gameLevel game)

    let buttonMapping = zoomRectToBottom Buttons.drawingRegion (dstButtonsRegion shared)
        (newButtons, buttonsResult) = Buttons.handleEvent event buttonMapping (gameButtons game)

    let newGame = game {
        gameLevel   = newLevel,
        gameButtons = newButtons
        }

    let result =
            case buttonsResult of
                Buttons.None -> do
                    return $ Just newGame
                Buttons.Undo -> do
                    return $ Just newGame {
                        gameLevel = Level.undo (gameLevel newGame)
                        }
                Buttons.Restart -> do
                    return $ Just newGame {
                        gameLevel = Level.restart (gameLevel newGame)
                        }
                Buttons.GiveUp -> do
                    return $ Just newGame {
                        gameLevel   = Level.giveUp (gameLevel newGame),
                        gameStats   = Stats.freezeTimer (gameStats newGame),

                        gameButtons =
                            if null (gameLevels newGame)
                                then Buttons.createFinished assets
                                else Buttons.createNext     assets
                        }
                Buttons.Next -> do
                    newerGame <- advanceLevel assets newGame
                    return $ Just newerGame
                Buttons.Quit -> do
                    return Nothing

    result

--------------------------------------------------------------------------------

-- | Atualiza o estado do 'Game'.
update :: Shared.Shared -- ^ Estado partilhado.
       -> Float         -- ^ Tempo desde a última atualização, em segundos.
       -> Game
       -> IO Game
update shared dt game = do

    let assets = Shared.assets shared

    let (newLevel, levelJustEnded) = Level.update dt (gameLevel game)

    newStats <-
        Stats.update assets
        . Stats.setSteps assets (Level.numSteps newLevel)
        $ (gameStats game)

    let newGame = game {
        gameLevel = newLevel,
        gameStats = newStats
        }

    let result
            | levelJustEnded =
                return newGame {
                    gameStats   = Stats.freezeTimer (gameStats newGame),
                    gameButtons =
                        if null (gameLevels newGame)
                            then Buttons.createFinished assets
                            else Buttons.createNext     assets
                    }
            | otherwise =
                return newGame

    result

--------------------------------------------------------------------------------

-- | Converte o 'Game' numa 'Picture'.
draw :: Shared.Shared -- ^ Estado partilhado.
     -> Game
     -> Picture
draw shared game = Pictures [
    zoomPictureToCenter
        (Level.drawingRegion $ gameLevel game)
        (dstLevelRegion shared)
        (Level.draw $ gameLevel game),

    zoomPictureToTop
        (Stats.drawingRegion)
        (dstGameStatsRegion shared)
        (Stats.draw $ gameStats game),

    zoomPictureToBottom
        (Buttons.drawingRegion)
        (dstButtonsRegion shared)
        (Buttons.draw $ gameButtons game)
    ]

--------------------------------------------------------------------------------
