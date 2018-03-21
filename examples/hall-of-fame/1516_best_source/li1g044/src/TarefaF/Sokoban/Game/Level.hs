--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Game.Level
Description : Gere um nível.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo gere todos os aspetos relativos à representação e interação do
utilizador com o estado de um nível.
-}
module Sokoban.Game.Level (
    Level,
    create,
    numSteps,
    undo, restart, giveUp,
    handleEvent, update, drawingRegion, draw
    ) where

import Data.Char

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

import Sokoban.Helper
import qualified Sokoban.Assets          as Assets
import qualified Sokoban.Game.Board      as Board
import qualified Sokoban.Game.Boxes      as Boxes
import qualified Sokoban.Game.GameDefs   as GameDefs
import qualified Sokoban.Game.Player     as Player
import qualified Sokoban.Game.PlayerAttr as PlayerAttr

--------------------------------------------------------------------------------
-- * Tipos

-- | Tipo /abstrato/ que representa um nível.
data Level = Level {
    levelState        :: LevelState, -- ^ Estado do nível.

    levelReplayCmds   :: [Command], -- ^ Comandos que devem ser executados
                                    -- automaticamente no início do jogo.

    levelBoard        :: Board.Board,           -- ^ Tabuleiro.
    levelPlayerAttr   :: PlayerAttr.PlayerAttr, -- ^ Atributos do personagem.
    levelPlayer       :: Player.Player,         -- ^ Jogador.
    levelBoxes        :: Boxes.Boxes,           -- ^ Caixas.

    levelAnimTime     :: Float, -- ^ Tempo atual da animação.
    levelAnimDuration :: Float, -- ^ Duração da animação.

    levelMoveLeft     :: Bool, -- ^ Determina se o utilizador pretende que o
                               -- jogador se mova para a esquerda.
    levelMoveDown     :: Bool, -- ^ Determina se o utilizador pretende que o
                               -- jogador se mova para baixo.
    levelMoveRight    :: Bool, -- ^ Determina se o utilizador pretende que o
                               -- jogador se mova para a direita.
    levelMoveUp       :: Bool, -- ^ Determina se o utilizador pretende que o
                               -- jogador se mova para cima.
    levelKeySpace     :: Bool  -- ^ Determina se a tecla de espaço está premida.
    }

-- | Representa o estado de um nível.
data LevelState
    = Playing    -- ^ O nível não está terminado e o utilizador pode interagir
                 -- com o personagem.
    | Replaying  -- ^ O nível está a executar comandos automáticos.
    | Restarting -- ^ O nível está anular todos os movimentos do jogador de modo
                 -- a regressar ao estado inicial.
    | Ended      -- ^ O nível terminou, quer por o jogador ter arrumado todas as
                 -- caixas, quer por desistência.
    deriving (Eq)

-- | Representa um comando que deve ser executado automaticamente no início do
-- nível. Estes comandos descrevem movimentos do jogador.
type Command = Char

--------------------------------------------------------------------------------
-- * Construção

-- | Contrói um valor do tipo 'Level', dada uma 'String' que representa o estado
-- inicial do nível e os atributos do personagem a utilizar.
create :: Assets.Assets         -- ^ Assets.
       -> String                -- ^ Nível a ser jogado.
       -> PlayerAttr.PlayerAttr -- ^ Atributos do personagem a utilizar.
       -> IO Level
create assets level playerAttr = do

    theme <- Assets.randomTheme assets

    (board, player, boxes, commands) <- parseLevel assets theme playerAttr level

    return Level {
        levelState        = Replaying,

        levelReplayCmds   = commands,

        levelBoard        = board,
        levelPlayerAttr   = playerAttr,
        levelPlayer       = player,
        levelBoxes        = boxes,

        levelAnimTime     = GameDefs.animDuration,
        levelAnimDuration = GameDefs.animDuration,

        levelMoveLeft     = False,
        levelMoveDown     = False,
        levelMoveRight    = False,
        levelMoveUp       = False,
        levelKeySpace     = False
        }

--------------------------------------------------------------------------------
-- ** Parsing

-- | Contrói valores do tipo 'Board.Board', 'Player.Player', 'Boxes.Boxes' e
-- @['Command']@ a partir de uma representação textual adequada.
parseLevel :: Assets.Assets         -- ^ Assets.
           -> Assets.Theme          -- ^ Tema.
           -> PlayerAttr.PlayerAttr -- ^ Atributos do personagem a utilizar.
           -> String                -- ^ Nível a ser jogado.
           -> IO (Board.Board, Player.Player, Boxes.Boxes, [Command])
parseLevel assets theme playerAttr level = do

    let levelLns = lines level
        (boardLns, nonBoardLns) = span ((== '#') . head) levelLns
        (coordLns, commandLns) = span (isDigit . head) nonBoardLns
        boardCells = parseCells boardLns
        (playerCoords : boxCoords) = parseCoords coordLns
        commands = parseCommands commandLns

    board <- Board.create assets theme boardCells
    let player = Player.create assets playerAttr playerCoords
    boxes <- Boxes.create assets theme boxCoords

    return (board, player, boxes, commands)
    where

-- | Contrói as células de um tabuleiro a partir de uma representação textual
-- adequada.
parseCells :: [String] -> ([Board.Cell], VectorI)
parseCells lns = (
    concatMap (map charToCell) lns,
    (length $ head lns, length lns)
    )
    where
        charToCell ' ' = Board.Ground
        charToCell '.' = Board.Target
        charToCell '#' = Board.Wall

-- | Contrói uma lista de pontos a partir de uma representação textual adequada.
parseCoords :: [String] -> [VectorI]
parseCoords = map parsePos

-- | Contrói um ponto a partir de uma representação textual adequada.
parsePos :: String -> VectorI
parsePos str = (read x, read y) :: VectorI
    where [x,y] = words str

-- | Contrói uma lista de comandos a partir de uma representação textual
-- adequada.
parseCommands :: [String] -> [Command]
parseCommands []         = []
parseCommands (cmds : _) = cmds

--------------------------------------------------------------------------------
-- * Estado

-- $
-- Funções que inspecionam o estado de um nível.

-- | Verifica se o nível se encontra a meio de uma animação.
isAnimating :: Level -> Bool
isAnimating level = levelAnimTime level /= levelAnimDuration level

-- | Determina o número de movimentos que o jogador já efetuou.
numSteps :: Level -> Int
numSteps = Player.numSteps . levelPlayer

-- | Verifica se o tabuleiro e as caixas de um nível se encontram num estado
-- terminal, i.e. se o nível já foi terminado.
levelFinished :: Level -> Bool
levelFinished level = 
    all (Board.cellIsTarget board) (Boxes.boxPositions boxes)
    where
        board = levelBoard level
        boxes = levelBoxes level

--------------------------------------------------------------------------------
-- * Ações

-- $
-- Funções exportadas que executam ações sobre um nível, e.g. anular uma jogada.

-- | Anula a última ação realizada pelo jogador, se existente.
undo :: Level -> Level
undo level =
    if canUndo
        then level {
            levelPlayer = Player.undo (levelPlayer level),
            levelBoxes = Boxes.undo (levelBoxes level),

            levelAnimTime = 0,
            levelAnimDuration = GameDefs.undoAnimDuration
            }
        else level
    where
        canUndo =
            levelState level == Playing &&
            not (isAnimating level) &&
            Player.canUndo (levelPlayer level)

-- | Anula todas as ações já realizadas pelo jogador.
restart :: Level -> Level
restart level =
    if levelState level == Playing
        then level { levelState = Restarting }
        else level

-- | Termina o jogo por desistência.
giveUp :: Level -> Level
giveUp level = level { levelState = Ended }

--------------------------------------------------------------------------------
-- * Movimento do Jogador

-- $
-- Funções responsáveis por gerir o movimento do jogador.

-- | Tenta mover o jogador numa certa direção.
playerMove :: VectorI -- ^ Direção do movimento.
           -> Level
           -> Level
playerMove dir level =
    if Player.isGrabbing (levelPlayer level)
        then playerMoveGrabbing dir level
        else playerMoveFree     dir level

-- | Tenta mover o jogador numa certa direção, no caso de o jogador /não/ se
-- encontrar a agarrar uma caixa.
playerMoveFree :: VectorI -- ^ Direção do movimento.
               -> Level
               -> Level
playerMoveFree dir level =
    case canMove of
        Nothing -> level {
            levelPlayer = Player.setDir dir player
            }
        Just movedBoxes -> level {
            levelPlayer = Player.move dir . Player.setDir dir $ player,
            levelBoxes  = Boxes.move dir movedBoxes (levelBoxes level),

            levelAnimTime = 0,
            levelAnimDuration = GameDefs.animDuration
            }
    where
        player  = levelPlayer level
        canMove = playerCanMove
            (PlayerAttr.pushStrength . levelPlayerAttr $ level)
            (Player.pos player |+| dir)
            dir
            level

-- | Tenta mover o jogador numa certa direção, no caso de o jogador se encontrar
-- a agarrar uma caixa.
playerMoveGrabbing :: VectorI -- ^ Direção do movimento.
                   -> Level 
                   -> Level
playerMoveGrabbing dir level =
    case canMove of
        Nothing -> level
        Just movedBoxes -> level {
            levelPlayer = Player.move dir player,
            levelBoxes  = Boxes.move dir movedBoxes (levelBoxes level),

            levelAnimTime = 0,
            levelAnimDuration = GameDefs.animDuration
            }
    where
        player  = levelPlayer level
        playerDir = Player.dir player
        boxInFront = Player.pos player |+| playerDir
        canMove
            | dir == playerDir = playerCanMove
                (PlayerAttr.pushStrength . levelPlayerAttr $ level)
                (Player.pos player |+| dir)
                dir
                level
            | dir == vecNeg playerDir = fmap (boxInFront:) $ playerCanMove
                0
                (Player.pos player |+| dir)
                dir
                level
            | otherwise = Nothing

-- | Determina se o jogador se pode mover numa certa direção, tendo em conta o
-- número de caixas que este pode empurrar.
--
-- Se o jogador não se puder mover, é devolvido o valor 'Nothing'. Em caso
-- contrário, é devolvido o valor @'Just' boxes@, onde @boxes@ corresponde às
-- posições de todas as caixas que o jogador iria movimentar ao realizar o
-- movimento em questão.
playerCanMove :: Int     -- ^ Número de caixas que o jogador ainda pode
                         -- empurrar.
              -> VectorI -- ^ Posição para onde o jogador se deve mover.
              -> VectorI -- ^ Direção na qual o jogador se deve mover.
              -> Level
              -> Maybe [VectorI]
playerCanMove n pos dir level
    | n == 0 =
        if Board.cellIsEmpty board pos && not (Boxes.hasBoxInPos pos boxes)
            then Just []
            else Nothing
    | Boxes.hasBoxInPos pos boxes = fmap (pos:) $ playerCanMove (n-1) (pos |+| dir) dir level
    | Board.cellIsEmpty board pos = Just []
    | otherwise                   = Nothing
    where
        board  = levelBoard level
        boxes  = levelBoxes level

--------------------------------------------------------------------------------

-- | Faz com que o jogador agarre na caixa que se encontra na direção
-- especificada, se possível.
playerGrab :: VectorI -> Level -> Level
playerGrab dir level =
    if canGrab
        then level {
            levelPlayer = Player.grabBox . Player.setDir dir $ player
            }
        else level {
            levelPlayer = Player.setDir dir $ player
            }
    where
        player    = levelPlayer level
        playerPos = Player.pos player
        canGrab   = Boxes.hasBoxInPos (playerPos |+| dir) (levelBoxes level)

--------------------------------------------------------------------------------

-- | Armazena a posição atual do jogador como a posição de destino do teleporte.
playerSaveTeleport :: Level -> Level
playerSaveTeleport level = level {
    levelPlayer = Player.saveTeleport (levelPlayer level)
    }

-- | Teleporta o jogador para a posição previamente armazenada, se possível.
playerDoTeleport :: Level -> Level
playerDoTeleport level =
    if PlayerAttr.canTeleport (levelPlayerAttr level) && playerCanTeleport level
        then level {
                levelPlayer = Player.doTeleport (levelPlayer level),
                levelBoxes  = Boxes.fakeMove (levelBoxes level),

                levelAnimTime = levelAnimDuration level
            }
        else level

-- | Verifica se o jogador se pode teleportar para a posição previamente
-- armazenada.
playerCanTeleport :: Level -> Bool
playerCanTeleport level =
    case teleportPos of
        Nothing -> False
        Just p  -> p /= (Player.pos player) && not (Boxes.hasBoxInPos p boxes)
    where
        player = levelPlayer level
        boxes = levelBoxes level
        teleportPos = Player.teleportPos player

--------------------------------------------------------------------------------
-- * Processamento de Eventos

-- | Processa um evento.
handleEvent :: Event -- ^ Evento.
            -> Level
            -> Level
handleEvent event level =
    if levelState level /= Playing
        then level
        else case event of
            EventKey (SpecialKey KeyLeft ) Down _ _ -> handleDirKey KeyLeft  level
            EventKey (SpecialKey KeyDown ) Down _ _ -> handleDirKey KeyDown  level
            EventKey (SpecialKey KeyRight) Down _ _ -> handleDirKey KeyRight level
            EventKey (SpecialKey KeyUp   ) Down _ _ -> handleDirKey KeyUp    level
            EventKey (SpecialKey KeyLeft ) Up   _ _ -> level { levelMoveLeft  = False }
            EventKey (SpecialKey KeyDown ) Up   _ _ -> level { levelMoveDown  = False }
            EventKey (SpecialKey KeyRight) Up   _ _ -> level { levelMoveRight = False }
            EventKey (SpecialKey KeyUp   ) Up   _ _ -> level { levelMoveUp    = False }

            EventKey (SpecialKey KeySpace) Down _ _ -> level {
                levelKeySpace = True
                }
            EventKey (SpecialKey KeySpace) Up   _ _ -> level {
                levelPlayer   = Player.releaseBox (levelPlayer level),
                levelKeySpace = False
                }

            EventKey (Char 's') Down _ _ -> playerSaveTeleport level
            EventKey (Char 'S') Down _ _ -> playerSaveTeleport level
            EventKey (Char 'x') Down _ _ -> playerDoTeleport   level
            EventKey (Char 'X') Down _ _ -> playerDoTeleport   level

            _ -> level

-- | Processa um evento relativo a uma tecla de direção.
handleDirKey :: SpecialKey -- ^ Tecla premida.
             -> Level
             -> Level
handleDirKey key level =
    if canGrab && levelKeySpace level
        then playerGrab dir level
        else
            case key of
                KeyLeft  -> level { levelMoveLeft  = True }
                KeyDown  -> level { levelMoveDown  = True }
                KeyRight -> level { levelMoveRight = True }
                KeyUp    -> level { levelMoveUp    = True }
    where
        canGrab =
            PlayerAttr.canPull (levelPlayerAttr level) &&
            not (Player.isGrabbing $ levelPlayer level)
        dir =
            case key of
                KeyLeft  -> (-1, 0)
                KeyDown  -> ( 0,-1)
                KeyRight -> ( 1, 0)
                KeyUp    -> ( 0, 1)

--------------------------------------------------------------------------------
-- * Atualização

-- | Atualiza o estado de um nível.
--
-- É devolvido um tuplo com o novo estado do nível e um booleano que determina
-- se durante esta atualização se verificou, pela primeira vez, que o nível
-- terminou.
update :: Float         -- ^ Tempo desde a última atualização, em segundos.
       -> Level
       -> (Level, Bool)
update dt level
    | isAnimating newLevel || levelState newLevel == Ended =
        (newLevel, False)
    | levelFinished newLevel = (
        newLevel {
            levelState = Ended,

            levelPlayer = Player.finish (levelPlayer newLevel)
            },
        True
        )
    | otherwise =
        case levelState newLevel of
            Playing    -> (updatePlaying    newLevel, False)
            Replaying  -> (updateReplaying  newLevel, False)
            Restarting -> (updateRestarting newLevel, False)
    where
        newLevel = level {
            levelPlayer    = Player.update animFactor $ levelPlayer level,
            levelBoxes     = Boxes.update animFactor $ levelBoxes level,

            levelAnimTime  = newAnimTime
            }

        newAnimTime = min (levelAnimDuration level) (levelAnimTime level + dt)
        animFactor  = levelAnimTime level / levelAnimDuration level

-- | Função auxiliar da função 'update', utilizada quando o estado do nível tem
-- o valor 'Playing'.
updatePlaying :: Level -> Level
updatePlaying level
    | levelMoveLeft  level = playerMove (-1, 0) level
    | levelMoveDown  level = playerMove ( 0,-1) level
    | levelMoveRight level = playerMove ( 1, 0) level
    | levelMoveUp    level = playerMove ( 0, 1) level
    | otherwise            = level

-- | Função auxiliar da função 'update', utilizada quando o estado do nível tem
-- o valor 'Replaying'.
updateReplaying :: Level -> Level
updateReplaying level =
    if null $ levelReplayCmds level
        then level { levelState = Playing }
        else
            playerMove
                (cmdDir $ head cmds)
                (level { levelReplayCmds = tail cmds })
    where
        cmds = levelReplayCmds level
        cmdDir 'L' = (-1, 0)
        cmdDir 'D' = ( 0,-1)
        cmdDir 'R' = ( 1, 0)
        cmdDir 'U' = ( 0, 1)

-- | Função auxiliar da função 'update', utilizada quando o estado do nível tem
-- o valor 'Restarting'.
updateRestarting :: Level -> Level
updateRestarting level =
    if Player.canUndo (levelPlayer level)
        then level {
            levelPlayer = Player.undo (levelPlayer level),
            levelBoxes = Boxes.undo (levelBoxes level),

            levelAnimTime = 0,
            levelAnimDuration = GameDefs.restartAnimDuration
            }
        else level {
            levelState = Playing
            }

--------------------------------------------------------------------------------
-- * Conversão para 'Picture'

-- | Determina a região em que um nível será desenhado.
drawingRegion :: Level -> Rect
drawingRegion = Board.drawingRegion . levelBoard

-- | Desenha um nível.
draw :: Level -> Picture
draw level = Pictures [
    Board.draw (levelBoard level),
    Boxes.draw (levelBoxes level),
    Player.draw (levelPlayer level)
    ]

--------------------------------------------------------------------------------
