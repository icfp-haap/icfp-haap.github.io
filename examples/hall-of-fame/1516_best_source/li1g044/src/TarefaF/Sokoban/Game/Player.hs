--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Player
Description : Gere um jogador.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere o armazenamento e representação visual das personagens que o
utilizador controla ao jogar o jogo.
-}
module Sokoban.Game.Player ( 
    Player,
    create,
    pos, dir, isGrabbing, teleportPos, numSteps, canUndo,
    setDir, move, grabBox, releaseBox, saveTeleport, doTeleport, finish, undo,
    update, draw
    ) where

import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.Assets          as Assets
import qualified Sokoban.Game.GameDefs   as GameDefs
import qualified Sokoban.Game.PlayerAttr as PlayerAttr

--------------------------------------------------------------------------------
-- * Tipos

-- | Tipo /abstrato/ que representa um jogador.
data Player = Player {
    playerPos          :: VectorI, -- ^ Coordenadas da célula do tabuleiro onde
                                   -- o jogador se encontra.
    playerDir          :: VectorI, -- ^ Direção para onde o jogador está virado.

    playerIsGrabbing   :: Bool,          -- ^ Determina se o jogador está a
                                         -- agarrar uma caixa.
    playerTeleportPos  :: Maybe VectorI, -- ^ Posição de destino do teleporte.
    playerFinished     :: Bool, -- ^ Determina se o jogador terminou o jogo.

    playerAnimPos      :: Vector, -- ^ Posição visual atual do jogador.
    playerAnimInitPos  :: Vector, -- ^ Posição visual do jogador no início da
                                  -- animação.
    playerAnimFinalPos :: Vector, -- ^ Posição visual do jogador no final da
                                  -- animação.
    playerAnimPhase    :: Int,    -- ^ Fase atual da animação. Toma o valor @0@
                                  -- ou @1@.

    playerPicFun       :: String -> Picture, -- ^ Função que obtém uma imagem do
                                             -- jogador dado o identificador de
                                             -- um bitmap.
    playerPic          :: Picture, -- ^ Imagem atual do jogador.

    playerSteps        :: [Step] -- ^ Movimentos que o jogador já realizou.
    }

-- | Armazena informação sobre um movimento do jogador.
data Step
    = StepMovement {
        stepInvDir      :: VectorI, -- ^ Direção oposta á do movimento.
        stepTurnDir     :: VectorI, -- ^ Direção para o qual o jogador estava
                                    -- virado durante o movimento.

        stepIsGrabbing  :: Bool, -- ^ Determina se o jogador estava a agarrar
                                 -- numa caixa durante o movimento.
        stepTeleportPos :: Maybe VectorI -- ^ Posição de destino do teleporte
                                         -- antes do movimento.
        }
    | StepTeleport {
        stepPrevPos     :: VectorI -- ^ Posição do jogador antes do teleporte.
        }

--------------------------------------------------------------------------------
-- * Construção

-- | Contrói um valor do tipo 'Player', dados os atributos do personagem e a
-- posição inicial do jogador.
create :: Assets.Assets         -- ^ Assets.
       -> PlayerAttr.PlayerAttr -- ^ Atributos do personagem.
       -> VectorI               -- ^ Posição inicial no tabuleiro.
       -> Player
create assets attr pos =
    Player {
        playerPos          = pos,
        playerDir          = (0, -1),

        playerIsGrabbing   = False,
        playerTeleportPos  = Nothing,
        playerFinished     = False,

        playerAnimPos      = animPos,
        playerAnimInitPos  = animPos,
        playerAnimFinalPos = animPos,
        playerAnimPhase    = 0,

        playerPicFun       = picFun,
        playerPic          = picFun "down_idle",

        playerSteps        = []
        }
    where
        animPos = GameDefs.tileSize |*| fromIntegralVector pos
        picFun = \x -> Assets.playerBitmap (PlayerAttr.bitmapName attr) x assets

--------------------------------------------------------------------------------
-- * Estado

-- | Determina as coordenadas da célula do tabuleiro onde um jogador se
-- encontra.
pos :: Player -> VectorI
pos = playerPos

-- | Determina a direção para onde um jogador está virado.
dir :: Player -> VectorI
dir = playerDir

-- | Verifica se um jogador está a agarrar uma caixa.
isGrabbing :: Player -> Bool
isGrabbing = playerIsGrabbing

-- | Determina a posição de destino de teleporte de um jogador.
teleportPos :: Player -> Maybe VectorI
teleportPos = playerTeleportPos

-- | Determina o número de movimentos que o jogador já efetuou.
numSteps :: Player -> Int
numSteps = length . playerSteps

-- | Verifica se o jogador pode anular uma jogada, i.e., se o jogador tem
-- jogadas para anular.
canUndo :: Player -> Bool
canUndo = not . null . playerSteps

--------------------------------------------------------------------------------
-- * Ações

-- | Move o jogador uma célula na direção especificada.
move :: VectorI -- Direção do movimento.
     -> Player
     -> Player
move dir player =
    player {
        playerPos          = newPos,

        playerAnimPos      = initAnimPos,
        playerAnimInitPos  = initAnimPos,
        playerAnimFinalPos = finalAnimPos,
        playerAnimPhase    = 1 - (playerAnimPhase player),

        playerSteps        = step : playerSteps player
        }
    where
        pos    = playerPos player
        newPos = pos |+| dir

        initAnimPos  = GameDefs.tileSize |*| fromIntegralVector pos
        finalAnimPos = GameDefs.tileSize |*| fromIntegralVector newPos

        step = StepMovement {
            stepInvDir      = vecNeg dir,
            stepTurnDir     = playerDir player,
            stepIsGrabbing  = playerIsGrabbing player,
            stepTeleportPos = playerTeleportPos player
            }

-- | Altera a direção para onde um jogador está virado.
setDir :: VectorI -- ^ Direção para onde o jogador deve ficar virado.
       -> Player
       -> Player
setDir dir player = player { playerDir = dir }

-- | Faz com que o jogador agarre uma caixa, na direção para onde ele se
-- encontra virado.
grabBox :: Player -> Player
grabBox player = player { playerIsGrabbing = True }

-- | Faz com que o jogador deixe de agarrar uma caixa.
releaseBox :: Player -> Player
releaseBox player = player { playerIsGrabbing = False }

-- | Armazena a posição atual do jogador como a sua posição de teleporte.
saveTeleport :: Player -> Player
saveTeleport player = player { playerTeleportPos = Just (playerPos player) }

-- | Teleporta o jogador para a posição previamente armazenada.
doTeleport :: Player -> Player
doTeleport player =
    case playerTeleportPos player of
        Nothing     -> player
        Just newPos -> player {
            playerPos          = newPos,

            playerIsGrabbing   = False,

            playerAnimPos      = animPos,
            playerAnimInitPos  = animPos,
            playerAnimFinalPos = animPos,

            playerSteps        = step : playerSteps player
            }
            where
                animPos = GameDefs.tileSize |*| fromIntegralVector newPos
                step = StepTeleport {
                    stepPrevPos = playerPos player
                    }

-- | Altera o aspeto do jogador para uma imagem que indique que o nível foi
-- terminado.
finish :: Player -> Player
finish player = player {
    playerPic      = (playerPicFun player) "finished",
    playerFinished = True
    }

-- | Anula o último movimento do jogador.
undo :: Player -> Player
undo player = newPlayer { playerSteps = tail steps }
    where
        steps = playerSteps player
        newPlayer = undoStep (head steps) player

-- | Função auxiliar utilizada pela função 'undo', que anula um movimento do
-- jogador.
undoStep :: Step   -- ^ Movimento a anular.
         -> Player
         -> Player

undoStep step@(StepMovement {}) player =
    player {
        playerPos          = newPos,
        playerDir          = stepTurnDir step,

        playerIsGrabbing   = stepIsGrabbing step,
        playerTeleportPos  = stepTeleportPos step,
        playerFinished     = False,

        playerAnimPos      = initAnimPos,
        playerAnimInitPos  = initAnimPos,
        playerAnimFinalPos = finalAnimPos,
        playerAnimPhase    = 1 - (playerAnimPhase player)
        }
    where
        pos    = playerPos player
        newPos = pos |+| stepInvDir step

        initAnimPos  = GameDefs.tileSize |*| fromIntegralVector pos
        finalAnimPos = GameDefs.tileSize |*| fromIntegralVector newPos

undoStep step@(StepTeleport {}) player =
    player {
        playerPos          = newPos,

        playerIsGrabbing   = False,
        playerFinished     = False,

        playerAnimPos      = animPos,
        playerAnimInitPos  = animPos,
        playerAnimFinalPos = animPos
        }
    where
        newPos  = stepPrevPos step
        animPos = GameDefs.tileSize |*| fromIntegralVector newPos

--------------------------------------------------------------------------------
-- * Atualização

-- | Atualiza o estado de um jogador.
update :: Float  -- ^ Fator, de @0@ a @1@, que determina o estado da animação
                 -- atual.
       -> Player
       -> Player
update animFactor player =
    if playerFinished player
        then player
        else player {
            playerAnimPos = newAnimPos,
            playerPic = (playerPicFun player) picName
            }
    where
        newAnimPos = lerp2
            animFactor
            (playerAnimInitPos player)
            (playerAnimFinalPos player)

        picNamePrefix = case playerDir player of
            (-1, 0) -> "left"
            (0, -1) -> "down"
            (1, 0)  -> "right"
            (0, 1)  -> "up"
        picNameMiddle
            | animFactor < 0.5 = "_walk" ++ show (playerAnimPhase player)
            | otherwise = "_idle"
        picNameSuffix = if playerIsGrabbing player then "_grab" else ""
        picName = picNamePrefix ++ picNameMiddle ++ picNameSuffix

--------------------------------------------------------------------------------
-- * Conversão para 'Picture'

-- | Desenha uma jogador.
draw :: Player -> Picture
draw player = Translate x (y + hTile/2) $ playerPic player
    where
        (x,y) = playerAnimPos player
        (_,hTile) = GameDefs.tileSize

--------------------------------------------------------------------------------
