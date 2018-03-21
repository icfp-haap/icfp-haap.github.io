--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Shared
Description : Partilha de estado com todo o programa.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém um tipo e funções destinados a facilitar a partilha de certas
estruturas de dados cuja utilização é recorrente em várias partes do programa.
-}
module Sokoban.Shared (
    Shared,
    assets, window,
    create,
    handleEvent
    ) where

import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets    as Assets
import qualified Sokoban.UI.Window as Window

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que armazena estruturas de dados que são frequentemente
-- utilizadas por várias partes do programa.
data Shared = Shared {
    assets :: Assets.Assets,
    window :: Window.Window
    }

-- | Cria um valor do tipo 'Shared', cuja 'Window.Window' tem a resolução
-- inicial especificada.
create :: VectorI   -- Resolução inicial da 'Window'.
       -> IO Shared
create resolution = do

    sharedAssets <- Assets.load

    return Shared {
        assets = sharedAssets,
        window = Window.create resolution
        }

--------------------------------------------------------------------------------

-- | Processa um evento.
handleEvent :: Event  -- ^ Evento.
            -> Shared
            -> Shared
handleEvent event shared = shared { window = newWindow }
    where newWindow = Window.handleEvent event (window shared)

--------------------------------------------------------------------------------
