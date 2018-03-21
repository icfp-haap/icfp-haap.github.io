--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.StageMgr
Description : Fases do programa.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

O programa está concetualmente dividido em duas fases: a fase dos menus (seleção
de personagem) e a fase do jogo (tabuleiro de jogo, controlo da personagem,
etc.). Este módulo gere essas fases e a transição entre elas.
-}
module Sokoban.StageMgr (
    StageMgr,
    create,
    handleEvent, update, draw
    ) where

import Data.List
import Data.List.Split
import System.Directory
import System.Environment

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Game.Game as Game
import qualified Sokoban.Menu.Menu as Menu
import qualified Sokoban.Shared    as Shared

--------------------------------------------------------------------------------

-- | Gere o estado e transições entre fases do programa.
data StageMgr = StageMgr {
    stageMgrLevels :: [String], -- ^ Níveis disponíveis para serem jogados.
    stageMgrStage  :: Stage     -- ^ Fase atual do programa.
    }

-- | Armazena o estado de uma das fases do programa.
data Stage
    = StageMenu Menu.Menu -- ^ Armazena o estado da fase dos menus.
    | StageGame Game.Game -- ^ Armazena o estado da fase do jogo.

--------------------------------------------------------------------------------

-- | Constrói um valor do tipo 'Stage'. A fase inicial é a fase dos menus.
create :: Shared.Shared -- ^ Estado partilhado.
       -> IO StageMgr
create shared = do
    levels <- loadLevels
    let menu = Menu.create shared
    return $ StageMgr levels (StageMenu menu)

-- | Carrega os níveis disponíveis para serem jogados.
loadLevels :: IO [String]
loadLevels = do

    args <- getArgs

    if null args
        then do
            loadLevelsFromStdin
        else do
            levels <- mapM loadLevelsFromPath args
            return $ concat levels

-- | Carrega os níveis que se encontram no @stdin@.
loadLevelsFromStdin :: IO [String]
loadLevelsFromStdin = do

    let splitter =
            split
            . dropFinalBlank
            . dropInnerBlanks
            . dropInitBlank
            . dropDelims
            . onSublist

    input <- getContents

    return (map unlines . splitter [""] . lines $ input)

-- | Carrega níveis a partir de um caminho.
loadLevelsFromPath :: FilePath -> IO [String]
loadLevelsFromPath path = do

    if not ("." `isSuffixOf` path)
        then do

            isFile <- doesFileExist      path
            isDir  <- doesDirectoryExist path

            case (isFile, isDir) of
                (False, False) -> fail $ "Level file not found: " ++ path
                (True , _    ) -> do
                    file <- readFile path
                    return [file]
                (_    , True ) -> do
                    contents <- getDirContents (path ++ "/")
                    levels   <- mapM loadLevelsFromPath (sort contents)
                    return $ concat levels

        else do

            return []

--------------------------------------------------------------------------------

-- | Processa um evento.
handleEvent :: Shared.Shared -- ^ Estado partilhado.
            -> Event         -- ^ Evento.
            -> StageMgr
            -> IO StageMgr

handleEvent shared event (StageMgr levels (StageMenu menu)) = do

    result <- Menu.handleEvent shared event menu

    case result of
        Left newMenu -> do
            return $ StageMgr levels (StageMenu newMenu)
        Right playerAttr -> do
            game <- Game.create shared levels playerAttr
            return $ StageMgr levels (StageGame game)

handleEvent shared event (StageMgr levels (StageGame game)) = do

    result <- Game.handleEvent shared event game

    case result of
        Just newGame -> do
            return $ StageMgr levels (StageGame newGame)
        Nothing -> do
            let menu = Menu.create shared
            return $ StageMgr levels (StageMenu menu)

--------------------------------------------------------------------------------

-- | Atualiza o estado de uma fase.
update :: Shared.Shared -- ^ Estado partilhado.
       -> Float         -- ^ Tempo desde a última atualização, em segundos.
       -> StageMgr
       -> IO StageMgr

update shared dt (StageMgr levels (StageMenu menu)) = do
    let newMenu = Menu.update shared dt menu
    return $ StageMgr levels (StageMenu newMenu)

update shared dt (StageMgr levels (StageGame game)) = do
    newGame <- Game.update shared dt game
    return $ StageMgr levels (StageGame newGame)

--------------------------------------------------------------------------------

-- | Desenha uma fase.
draw :: Shared.Shared -- ^ Estado partilhado.
     -> StageMgr
     -> Picture

draw shared (StageMgr _ (StageMenu menu)) = Menu.draw shared menu

draw shared (StageMgr _ (StageGame game)) = Game.draw shared game

--------------------------------------------------------------------------------
