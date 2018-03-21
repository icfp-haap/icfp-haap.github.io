--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Menu.Menu
Description : Menu principal.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o menu principal, através do qual o jogador pode selecionar o
personagem.
-}
module Sokoban.Menu.Menu (
    Menu,
    create,
    handleEvent, update, draw
    ) where

import System.Exit

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets              as Assets
import qualified Sokoban.Game.PlayerAttr     as PlayerAttr
import qualified Sokoban.Menu.PlayerSelector as PlayerSelector
import qualified Sokoban.Shared              as Shared
import qualified Sokoban.UI.Button           as Button
import qualified Sokoban.UI.Window           as Window

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa um menu.
data Menu = Menu {
    menuTitlePic   :: Picture,       -- ^ Imagem do título.
    menuSelector   :: PlayerSelector.PlayerSelector, -- ^ Selecionador de
                                                     -- personagem.
    menuPlayButton :: Button.Button, -- ^ Botão para jogar.
    menuQuitButton :: Button.Button  -- ^ Botão para sair.
    }

--------------------------------------------------------------------------------

-- | Região de desenho do menu.
menuRegion :: Rect
menuRegion = makeRect (800, 600)

-- | Gera uma transformação de retângulos entre espaço do menu e espaço da
-- janela.
makeRectMapping :: Shared.Shared -> RectMapping
makeRectMapping shared =
    zoomRectToCenter
        menuRegion
        (Window.region . Shared.window $ shared)

-- | Gera uma transformação de @'Picture's@ entre espaço do menu e espaço da
-- janela.
makePictureMapping :: Shared.Shared -> PictureMapping
makePictureMapping shared =
    zoomPictureToCenter
        menuRegion
        (Window.region . Shared.window $ shared)

--------------------------------------------------------------------------------

-- | Cria uma instância do menu principal.
create :: Shared.Shared -- ^ Estado partilhado.
       -> Menu
create shared =
    Menu {
        menuTitlePic   = titlePic,
        menuSelector   = PlayerSelector.create assets (0, -55),
        menuPlayButton = playButton,
        menuQuitButton = quitButton
        }
    where
        assets = Shared.assets shared
        titlePic =
            regionMapBitmap
                (makeRectAt (0, 220) (600, 100))
                (Assets.uiBitmap "title" assets)
        playButton =
            Button.create
                assets
                (makeRectAt (-100, -250) (180, 45))
                "Jogar"
                (SpecialKey KeyEnter)
        quitButton =
            Button.create
                assets
                (makeRectAt (100, -250) (180, 45))
                "Sair"
                (SpecialKey KeyEsc)

--------------------------------------------------------------------------------

-- | Processa um comando.
--
-- Se o menu deve continuar a existir, devolve @'Left' newMenu@, onde @newMenu@
-- corresponde ao novo estado do menu.
--
-- Se o menu tiver terminado, devolve @'Right' attr@, onde @attr@ corresponde
-- aos atributos do personagem selecionado.
handleEvent :: Shared.Shared                    -- ^ Estado partilhado.
            -> Event                            -- ^ Evento.
            -> Menu                             -- ^ Menu.
            -> IO (Either Menu PlayerAttr.PlayerAttr)
handleEvent shared event menu = do

    let mapping = makeRectMapping shared

    let newSelector =
            PlayerSelector.handleEvent
                event
                mapping
                (menuSelector menu)

    let (newPlayButton, pressedPlay) =
            Button.handleEvent
                event
                mapping
                (menuPlayButton menu)

    let (newQuitButton, pressedQuit) =
            Button.handleEvent
                event
                mapping
                (menuQuitButton menu)

    let result
            | pressedPlay = do
                return $ Right (PlayerSelector.selection newSelector)

            | pressedQuit = do
                exitSuccess

            | otherwise = do
                return $ Left menu {
                    menuSelector   = newSelector,
                    menuPlayButton = newPlayButton,
                    menuQuitButton = newQuitButton
                    }

    result

--------------------------------------------------------------------------------

-- | Atualiza o estado do menu.
update :: Shared.Shared -- ^ Estado partilhado.
       -> Float         -- ^ Tempo desde a última atualização, em segundos.
       -> Menu          -- ^ Menu.
       -> Menu
update _ dt menu =
    menu {
        menuSelector = PlayerSelector.update dt (menuSelector menu)
        }

--------------------------------------------------------------------------------

-- | Desenha um menu.
draw :: Shared.Shared -- ^ Estado partilhado.
     -> Menu          -- ^ Menu.
     -> Picture
draw shared menu =
    makePictureMapping shared $ Pictures [
        menuTitlePic menu,
        PlayerSelector.draw (menuSelector menu),
        Button.draw (menuPlayButton menu),
        Button.draw (menuQuitButton menu)
        ]

--------------------------------------------------------------------------------
