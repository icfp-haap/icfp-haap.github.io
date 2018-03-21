--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban
Description : Módulo principal do programa.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Módulo inicial da resolução correspondente à __Tarefa F__ da segunda fase do
projeto de Laboratórios de Informática I.
-}
module Sokoban (main) where

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Shared    as Shared
import qualified Sokoban.StageMgr  as StageMgr
import qualified Sokoban.UI.UIDefs as UIDefs

--------------------------------------------------------------------------------

-- | Função inicial do programa.
main :: IO ()
main = do

    state <- createState UIDefs.initialResolution

    playIO
        (InWindow "Sokoban" UIDefs.initialResolution (150, 50))
        UIDefs.backgroundColor
        60
        state
        draw
        handleEvent
        update

--------------------------------------------------------------------------------

-- | Armazena todo o estado do programa.
data State = State {
    stateShared   :: Shared.Shared,
    stateStageMgr :: StageMgr.StageMgr
    }

-- | Cria o estado inicial do programa.
createState :: VectorI  -- ^ Resolução inicial da janela.
            -> IO State
createState resolution = do

    shared <- Shared.create resolution
    stage  <- StageMgr.create shared

    return State {
        stateShared   = shared,
        stateStageMgr = stage
        }

--------------------------------------------------------------------------------

-- | Processa um evento.
handleEvent :: Event    -- ^ Evento.
            -> State
            -> IO State
handleEvent event state = do

    let shared = stateShared   state
        stage  = stateStageMgr state

    let newShared = Shared.handleEvent event shared
    newStageMgr <- StageMgr.handleEvent newShared event stage

    return state {
        stateShared   = newShared,
        stateStageMgr = newStageMgr
        }

--------------------------------------------------------------------------------

-- | Atualiza o estado do programa.
update :: Float    -- ^ Tempo desde a última atualização, em segundos.
       -> State
       -> IO State
update dt state = do

    let shared = stateShared   state
        stage  = stateStageMgr state

    newStageMgr <- StageMgr.update shared dt stage

    return state {
        stateStageMgr = newStageMgr
        }

--------------------------------------------------------------------------------

-- | Transforma o estado do programa num valor do tipo 'Picture' para ser
-- desenhado no ecrã.
draw :: State
     -> IO Picture
draw state = do

    let shared = stateShared   state
        stage  = stateStageMgr state

    return $ StageMgr.draw shared stage

--------------------------------------------------------------------------------
