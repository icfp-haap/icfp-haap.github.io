--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Stats
Description : Painel de informação sobre o jogo.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o painel de informações sobre o jogo, que apresenta o número do
nível atual, o número total de níveis, o número de passos já efetuados e o tempo
decorrido desde o início do nível.
-}
module Sokoban.Game.Stats (
    Stats,
    create,
    advanceLevel, setSteps, freezeTimer,
    update, drawingRegion, draw,

    secondsToString,

    -- (Unit Testing)

    Sokoban.Game.Stats.runHUTests,
    tests_secondsToString

    ) where

import System.Clock
import Text.Printf

import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.Assets      as Assets
import qualified Sokoban.UI.Font     as Font
import qualified Sokoban.UI.UIDefs   as UIDefs
import qualified Sokoban.UI.UIHelper as UIHelper

import Test.HUnit

--------------------------------------------------------------------------------

-- | Centro da região de desenho da imagem de fundo.
backgroundCenter :: Vector
backgroundCenter = rectCenter backgroundRegion

-- | Região de desenho da imagem de fundo.
backgroundRegion :: Rect
backgroundRegion = makeRect (backgroundWidth, backgroundHeight)

-- | Largura da região de desenho da imagem de fundo.
backgroundWidth :: Float
backgroundWidth = 850

-- | Altura da região de desenho da imagem de fundo.
backgroundHeight :: Float
backgroundHeight = 70

-- | Tamanho das margens entre o espaço de desenho da imagem de fundo e o
-- espaço de desenho do painel de informação.
backgroundSpacing :: Float
backgroundSpacing = 20

-- | Altura do texto do painel de informação.
textHeight :: Float
textHeight = 28

--------------------------------------------------------------------------------

-- | Tipo /abstrato/ que representa um painel de informação.
data Stats = Stats {
    statsBackPic    :: Picture,  -- ^ Imagem de fundo do painel.
    statsTextPic    :: Picture,  -- ^ Imagem do texto do painel.

    statsLevelCur   :: Int,      -- ^ Número do nível atual.
    statsLevelNum   :: Int,      -- ^ Número total de níveis.

    statsSteps      :: Int,      -- ^ Número de passos.

    statsTimeInit   :: TimeSpec, -- ^ Instante de tempo no início do nível
                                 -- atual.
    statsTimeSec    :: Int,      -- ^ Número de segundos decorridos desde o
                                 -- início do nível atual.
    statsTimeFrozen :: Bool      -- ^ Determina se o temporizador está parado.
    }

--------------------------------------------------------------------------------

-- | Contrói um painel de informação sobre o jogo, dado o número total de níveis
-- disponíveis para serem jogados.
create :: Assets.Assets -- ^ Assets.
       -> Int           -- ^ Número total de níveis.
       -> IO Stats
create assets numLevels = do

    let backPic =
            UIHelper.createBackground
                assets
                backgroundRegion
                UIHelper.Light

    let textPic = createTextPic assets 1 numLevels 0 0

    time <- getTime Monotonic

    return Stats {
        statsBackPic    = backPic,
        statsTextPic    = textPic,

        statsLevelCur   = 1,
        statsLevelNum   = numLevels,

        statsSteps      = 0,

        statsTimeInit   = time,
        statsTimeSec    = 0,
        statsTimeFrozen = False
        }

-- | Contrói a imagem do texto de um painel de informação.
createTextPic :: Assets.Assets -- ^ Assets.
              -> Int           -- ^ Número do nível atual.
              -> Int           -- ^ Número total de níveis.
              -> Int           -- ^ Número de passos.
              -> Int           -- ^ Número de segundos decorridos desde o início
                               -- do nível atual.
              -> Picture
createTextPic assets levelCur levelNum steps time =
    Font.draw backgroundCenter (Assets.uiFont assets) text
    where
        str =
            "Nível " ++ show levelCur ++ "/" ++ show levelNum ++ "         " ++
            "Passos " ++ show steps ++ "         " ++
            "Tempo " ++ secondsToString time
        text = Font.Text
            str
            textHeight
            Font.Center
            UIDefs.mainTextColor
            UIDefs.mainTextShadowColor

-- | Converte um número de segundos numa representação textual adequada.
--
-- ==== Exemplos de utilização:
-- >>> secondsToString 0
-- "00:00"
--
-- >>> secondsToString 42
-- "00:42"
--
-- >>> secondsToString 1672
-- "27:52"
secondsToString :: Int -> String
secondsToString s = printf "%02d:%02d" (s `div` 60) (s `mod` 60)

tests_secondsToString = TestLabel "secondsToString" $ TestList [
    TestCase $ assertEqual "for (secondsToString 0),"
        "00:00" (secondsToString 0),
    TestCase $ assertEqual "for (secondsToString 42),"
        "00:42" (secondsToString 42),
    TestCase $ assertEqual "for (secondsToString 1672),"
        "27:52" (secondsToString 1672)
    ]

--------------------------------------------------------------------------------

-- | Incrementa o número do nível atual e anula o número de passos e o
-- temporizador de um painel de informação.
advanceLevel :: Assets.Assets -- ^ Assets.
             -> Stats
             -> IO Stats
advanceLevel assets stats = do

    let newLevelCur = statsLevelCur stats + 1
        levelNum    = statsLevelNum stats

    time <- getTime Monotonic

    return stats {
        statsTextPic    = createTextPic assets newLevelCur levelNum 0 0,

        statsLevelCur   = newLevelCur,

        statsSteps      = 0,

        statsTimeInit   = time,
        statsTimeSec    = 0,
        statsTimeFrozen = False
        }

-- | Define o número de passos de um painel de informação.
setSteps :: Assets.Assets -- ^ Assets.
         -> Int           -- ^ Número de passos.
         -> Stats
         -> Stats
setSteps assets steps stats =
    if steps == statsSteps stats
        then stats
        else stats {
            statsTextPic =
                createTextPic
                    assets
                    levelCur
                    levelNum
                    steps
                    seconds,
            statsSteps = steps
            }
    where
        levelCur = statsLevelCur stats
        levelNum = statsLevelNum stats
        seconds  = statsTimeSec  stats

-- | Bloqueia o temporizador de um painel de informação.
freezeTimer :: Stats -> Stats
freezeTimer stats = stats { statsTimeFrozen = True }

--------------------------------------------------------------------------------

-- | Atualiza o estado de um painel de informação.
update :: Assets.Assets -- ^ Assets.
       -> Stats         -- ^ Tempo desde a última atualização, em segundos.
       -> IO Stats
update assets stats =
    if statsTimeFrozen stats
        then do

            return stats

        else do

            let levelCur = statsLevelCur stats
                levelNum = statsLevelNum stats
                steps    = statsSteps    stats
            
            time <- getTime Monotonic
            let seconds = secondsBetween (statsTimeInit stats) time

            if seconds == statsTimeSec stats
                then return stats
                else return stats {
                    statsTextPic =
                        createTextPic
                            assets
                            levelCur
                            levelNum
                            steps
                            seconds,

                    statsTimeSec = seconds
                    }

--------------------------------------------------------------------------------

-- | Determina a região em que um painel de informação será desenhado.
drawingRegion :: Rect
drawingRegion = makeRect (
    backgroundWidth  + 2*backgroundSpacing,
    backgroundHeight + 2*backgroundSpacing
    )

-- | Desenha um painel de informação.
draw :: Stats -> Picture
draw stats = Pictures [
    statsBackPic stats,
    statsTextPic stats
    ]

--------------------------------------------------------------------------------

runHUTests = runTestTT $ TestList [
    tests_secondsToString
    ]

--------------------------------------------------------------------------------
