--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TarefaD
Description : Resolução da Tarefa D.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém uma resolução correspondente à __Tarefa D__ da segunda fase
do projeto de Laboratórios de Informática I.

O objetivo é o de ler do @stdin@ o estado de um tabuleiro de jogo e uma
sequência de comandos que descrevem o movimento do jogador. Os comandos são
depois executados até não haver mais comandos ou o jogo atingir um estado
terminal. É apresentado no @stdout@ o número de comandos executados e a
indicação de que o jogo foi terminado ou não.
-}
module TarefaD where

import Data.Char
import qualified Data.Text as T

import Test.HUnit hiding (State)
import Test.QuickCheck

--------------------------------------------------------------------------------

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

main = do inp <- getContents
          putStr (outStr (tarefaD (inStr inp)))

--------------------------------------------------------------------------------
-- * Entrada

-- | Função inicial da tarefa. Recebe o input do programa e devolve o output
-- correspondente.
tarefaD :: [String] -> [String]
tarefaD input = [showState finalState]
    where
        (initialState, commands) = parseInput input
        finalState = runCommands initialState commands        

--------------------------------------------------------------------------------
-- * Tipos e Funções Auxiliares

-- $
-- Tipos e funções genéricos utilizados pelo programa.

-- | Representa um vetor bidimensional.
--
-- Pode ser utilizado para representar pontos, translações, direções, dimensões,
-- etc. 
type Vector = (Int, Int)

-- | Adiciona dois valores do tipo 'Vector' componente por componente, segundo a
-- equação:
--
-- @(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)@
--
-- ==== Exemplos de utilização:
-- >>> (1,4) |+| (2,5)
-- (3,9)
(|+|) :: Vector -> Vector -> Vector
(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)

tests_vecAdd = TestLabel "(|+|)" $ TestList [
    TestCase $ assertEqual "for ((1,4) |+| (2,5))," (3,9) ((1,4) |+| (2,5)),
    TestCase $ assertEqual "for ((1,4) |+| (0,0))," (1,4) ((1,4) |+| (0,0)),
    TestCase $ assertEqual "for ((0,0) |+| (2,5))," (2,5) ((0,0) |+| (2,5))
    ]

prop_vecAdd_commutativity :: Vector -> Vector -> Bool
prop_vecAdd_commutativity v1 v2 = (v1 |+| v2) == (v2 |+| v1)

prop_vecAdd_leftIdentity :: Vector -> Bool
prop_vecAdd_leftIdentity v = ((0,0) |+| v) == v

prop_vecAdd_rightIdentity :: Vector -> Bool
prop_vecAdd_rightIdentity v = (v |+| (0,0)) == v

-- | Substitui um elemento de uma lista pelo valor especificado.
--
-- ==== Exemplos de utilização:
-- >>> replaceElemAt 1 'E' ['a','b','c','d']
-- ['a','E','c','d']
--
-- >>> replaceElemAt 3 9 [1,2,3,4]
-- [1,2,3,9]
replaceElemAt :: Int -- ^ Índice do elemento a substituir.
              -> a   -- ^ Valor pelo qual substituir o elemento.
              -> [a] -- ^ Lista original.
              -> [a] -- ^ Lista resultante.
replaceElemAt i v (x:xs)
    | i >  0 = x : replaceElemAt (i-1) v xs
    | i == 0 = v : xs

tests_replaceElemAt = TestLabel "replaceElemAt" $ TestList [
    TestCase $ assertEqual
        "for (replaceElemAt 1 'E' ['a','b','c','d']),"
        ['a','E','c','d']
        (replaceElemAt 1 'E' ['a','b','c','d']),
    TestCase $ assertEqual
        "for (replaceElemAt 3 9 [1,2,3,4]),"
        [1,2,3,9]
        (replaceElemAt 3 9 [1,2,3,4])
    ]

--------------------------------------------------------------------------------
-- * Processamento do Input

-- $
-- Tipos e funções destinados ao processamento e manipulação do input do
-- programa.

-- | Contrói um tuplo com valores dos tipos 'State' e @['Command']@ a partir de
-- uma representação textual adequada.
parseInput :: [String] -> (State, [Command])
parseInput input = (parseState lnsState, parseCommands lnCommands)
    where
        (lnsState, lnsCommands) = span
            (\l -> not (null l) && (head l == '#' || isDigit (head l)))
            input
        lnCommands = if null lnsCommands then [] else head lnsCommands

--------------------------------------------------------------------------------
-- ** Estado de Jogo

-- | Armazena o estado de um mapa de jogo, i.e. o tabuleiro, a posição do
-- jogador e o número de comandos já executados.
data State = State {
    stateBoard     :: Board,  -- ^ Tabuleiro de jogo.
    statePlayer    :: Vector, -- ^ Posição do jogador.
    stateTickCount :: Int     -- ^ Número de comandos já executados.
    }
    deriving (Eq, Show)

-- | Verifica se um mapa de jogo está num estado terminal.
stateIsSolved :: State -> Bool
stateIsSolved = all ('H' `notElem`) . boardCells . stateBoard

-- | Constrói um valor do tipo 'State' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseState ["######", "#  .  #", "######", "2 1", "5 1"]
-- State {
--     stateBoard     = Board { boardSize = (6,3), boardCells = ["######","#  . H#","######"] },
--     statePlayer    = (2,1),
--     stateTickCount = 0
--     }
parseState :: [String] -> State
parseState lnsState =
    State {
        stateBoard     = parseBoard lnsBoard coords,
        statePlayer    = coordsPlayer coords,
        stateTickCount = 0
        }
    where
        (lnsBoard, lnsCoords) = span ((== '#') . head) lnsState
        coords = parseCoords lnsCoords 

tests_parseState = TestLabel "parseState" $ TestList [
    TestCase $ assertEqual
        "for (parseState [\"######\", \"#  .  #\", \"######\", \"2 1\", \"5 1\"]),"
        (State {
            stateBoard     = Board { boardSize = (6,3), boardCells = ["######","#  . H#","######"] },
            statePlayer    = (2,1),
            stateTickCount = 0
            })
        (parseState ["######", "#  .  #", "######", "2 1", "5 1"])
    ]

-- | Transforma um valor do tipo 'State' numa 'String' que indica se o mapa de
-- jogo está num estado terminal e o número de comandos executados.
showState :: State -> String
showState state = solved ++ ticks
    where
        solved = if stateIsSolved state then "FIM " else "INCOMPLETO "
        ticks  = show $ stateTickCount state

--------------------------------------------------------------------------------
-- ** Tabuleiro

-- $
-- Tipos e funções específicos ao processamento de tabuleiros de jogo.

-- | Representa um tabuleiro.
data Board = Board {
    boardSize  :: Vector,  -- ^ O tamanho do tabuleiro.
    boardCells :: [[Cell]] -- ^ As células do tabuleiro.
    }
    deriving (Eq, Show)

-- | Representa uma célula de um tabuleiro.
type Cell = Char

-- | Contrói um valor do tipo 'Board' a partir de uma representação textual
-- adequada.
parseBoard :: [String] -> Coords -> Board
parseBoard cells@(l:_) coords = applyBoxesToBoard boxes (Board size cells) 
    where size  = (length l, length cells) 
          boxes = coordsBoxes coords

-- | Devolve a célula de um tabuleiro com as coordenadas especificadas.
getCell :: Vector -- ^ Coordenadas da célula.
        -> Board  -- ^ Tabuleiro.
        -> Cell   -- ^ Célula nas coordenadas especificadas.
getCell (x,y) (Board (_,h) cells) = (cells !! line) !! column
    where (line, column) = (h-y-1, x)

-- | Substitui uma célula de um tabuleiro pelo valor especificado e devolve o
-- tabuleiro resultante.
setCell :: Cell   -- ^ Valor pelo qual substituir a célula.
        -> Vector -- ^ Coordenadas da célula.
        -> Board  -- ^ Tabuleiro original.
        -> Board  -- ^ Tabuleiro resultante.
setCell val (x,y) (Board (w,h) cells) =
    Board
        (w,h)
        (replaceElemAt line (replaceElemAt column val (cells !! line)) cells)
    where (line, column) = (h-y-1, x)

-- | Verifica se uma célula se encontra vazia.
--
-- ==== Exemplos de utilização:
-- >>> cellIsClear ' '
-- True
--
-- >>> cellIsClear '.'
-- True
--
-- >>> cellIsClear 'H'
-- False
--
-- >>> cellIsClear 'I'
-- False
--
-- >>> cellIsClear '#'
-- False
cellIsClear :: Cell -> Bool
cellIsClear c = (c == ' ' || c == '.')

tests_cellIsClear = TestLabel "cellIsClear" $ TestList [
    TestCase $ assertEqual "for (cellIsClear ' ')," True  (cellIsClear ' '),
    TestCase $ assertEqual "for (cellIsClear '.')," True  (cellIsClear '.'),
    TestCase $ assertEqual "for (cellIsClear 'H')," False (cellIsClear 'H'),
    TestCase $ assertEqual "for (cellIsClear 'I')," False (cellIsClear 'I'),
    TestCase $ assertEqual "for (cellIsClear '#')," False (cellIsClear '#')
    ]

-- | Verifica se uma célula contém uma caixa.
--
-- ==== Exemplos de utilização:
-- >>> cellIsBox ' '
-- False
--
-- >>> cellIsBox '.'
-- False
--
-- >>> cellIsBox 'H'
-- True
--
-- >>> cellIsBox 'I'
-- True
--
-- >>> cellIsBox '#'
-- False
cellIsBox :: Cell -> Bool
cellIsBox c = (c == 'H' || c == 'I')

tests_cellIsBox = TestLabel "cellIsBox" $ TestList [
    TestCase $ assertEqual "for (cellIsBox ' ')," False (cellIsBox ' '),
    TestCase $ assertEqual "for (cellIsBox '.')," False (cellIsBox '.'),
    TestCase $ assertEqual "for (cellIsBox 'H')," True  (cellIsBox 'H'),
    TestCase $ assertEqual "for (cellIsBox 'I')," True  (cellIsBox 'I'),
    TestCase $ assertEqual "for (cellIsBox '#')," False (cellIsBox '#')
    ]

-- | Coloca uma caixa na célula de um tabuleiro na posição especificada.
--
-- ==== Exemplos de utilização:
-- >>> boardPutBox (1,1) (Board { boardSize = (5,3), boardCells = ["#####","# H.#","#####"] })
-- Board { boardSize = (5,3), boardCells = ["#####","#HH.#","#####"] }
boardPutBox :: Vector -> Board -> Board
boardPutBox p board = setCell (cellPutBox (getCell p board)) p board

tests_boardPutBox = TestLabel "boardPutBox" $ TestList [
    TestCase $ assertEqual
        "for (boardPutBox (1,1) (Board { boardSize = (5,3), boardCells = [\"#####\",\"# H.#\",\"#####\"] })),"
        (Board { boardSize = (5,3), boardCells = ["#####","#HH.#","#####"] })
        (boardPutBox (1,1) (Board { boardSize = (5,3), boardCells = ["#####","# H.#","#####"] }))
    ]

-- | Remove uma caixa da célula de um tabuleiro na posição especificada.
--
-- ==== Exemplos de utilização:
-- >>> boardRemoveBox (1,1) (Board { boardSize = (5,3), boardCells = ["#####","#HH.#","#####"] })
-- Board { boardSize = (5,3), boardCells = ["#####","# H.#","#####"] }
boardRemoveBox :: Vector -> Board -> Board
boardRemoveBox p board = setCell (cellRemoveBox (getCell p board)) p board

tests_boardRemoveBox = TestLabel "boardRemoveBox" $ TestList [
    TestCase $ assertEqual
        "for (boardRemoveBox (1,1) (Board { boardSize = (5,3), boardCells = [\"#####\",\"#HH.#\",\"#####\"] })),"
        (Board { boardSize = (5,3), boardCells = ["#####","# H.#","#####"] })
        (boardRemoveBox (1,1) (Board { boardSize = (5,3), boardCells = ["#####","#HH.#","#####"] }))
    ]

-- | Coloca uma caixa numa célula vazia.
--
-- ==== Exemplos de utilização:
-- >>> cellPutBox ' '
-- 'H'
--
-- >>> cellPutBox '.'
-- 'I'
cellPutBox :: Cell -> Cell
cellPutBox ' ' = 'H'
cellPutBox '.' = 'I'

tests_cellPutBox = TestLabel "cellPutBox" $ TestList [
    TestCase $ assertEqual "for (cellPutBox ' ')," 'H' (cellPutBox ' '),
    TestCase $ assertEqual "for (cellPutBox '.')," 'I' (cellPutBox '.')
    ]

-- | Retira uma caixa de uma célula com uma caixa.
--
-- ==== Exemplos de utilização:
-- >>> cellRemoveBox 'H'
-- ' '
--
-- >>> cellRemoveBox 'I'
-- '.'
cellRemoveBox :: Cell -> Cell
cellRemoveBox 'H' = ' '
cellRemoveBox 'I' = '.'

tests_cellRemoveBox = TestLabel "cellRemoveBox" $ TestList [
    TestCase $ assertEqual "for (cellRemoveBox 'H')," ' ' (cellRemoveBox 'H'),
    TestCase $ assertEqual "for (cellRemoveBox 'I')," '.' (cellRemoveBox 'I')
    ]

--------------------------------------------------------------------------------
-- ** Coordenadas

-- $
-- Tipos e funções relacionados com o processamento de coordenadas.

-- | Representa as coordenadas (posições no tabuleiro) do boneco e de todas as
-- caixas.
data Coords = Coords {
    coordsPlayer :: Vector,  -- ^ As coordenadas do boneco.
    coordsBoxes  :: [Vector] -- ^ As coordenadas de todas as caixas.
    }
    deriving (Eq, Show)

-- | Constrói um valor do tipo 'Coords' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseCoords ["11 2", "5 8", "7 7", "5 6"]
-- Coords { coordsPlayer = (11,2), coordsBoxes = [(5,8),(7,7),(5,6)] }
parseCoords :: [String] -> Coords
parseCoords (player:boxes) = Coords (parse player) (map parse boxes)
    where parse l = let [x,y] = words l in (read x, read y) :: Vector

tests_parseCoords = TestLabel "parseCoords" $ TestList [
    TestCase $ assertEqual
        "for (parseCoords [\"11 2\", \"5 8\", \"7 7\", \"5 6\"]),"
        (Coords { coordsPlayer = (11,2), coordsBoxes = [(5,8),(7,7),(5,6)] })
        (parseCoords ["11 2", "5 8", "7 7", "5 6"])
    ]

-- | Modifica um tabuleiro para que nele fiquem representadas as caixas nas
-- células em que estas se encontrem.
--
-- As células nas quais se encontram caixas passam a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
applyBoxesToBoard :: [Vector] -- ^ Coordenadas das caixas.
                  -> Board    -- ^ Tabuleiro original.
                  -> Board    -- ^ Tabuleiro resultante.
applyBoxesToBoard boxes board = foldr applyBoxToBoard board boxes

-- | Modifica um tabuleiro para que nele fique representada uma caixa na célula
-- em que esta se encontre.
--
-- A célula na qual se encontra a caixa passa a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
applyBoxToBoard :: Vector -- ^ Coordenadas da caixa.
                -> Board  -- ^ Tabuleiro original.
                -> Board  -- ^ Tabuleiro resultante.
applyBoxToBoard box board = setCell val box board
    where val = if getCell box board == '.' then 'I' else 'H'

-------------------------------------------------------------------------------
-- ** Comandos

-- $
-- Tipos e funções relacionados com o comando que deve ser executado.

-- | Representa um comando.
--
-- Pode tomar os seguintes valores: @\'L\'@, @\'R\'@, @\'U\'@, @\'D\'@.
type Command = Char

-- | Contrói um valor do tipo 'Command' a partir de uma representação textual
-- adequada.
parseCommands :: String -> [Command]
parseCommands = id

-- | Converte um valor do tipo 'Command' num valor do tipo 'Vector' que
-- representa a direção correspondente a esse comando.
--
-- ==== Exemplos de utilização:
-- >>> commandDir 'L'
-- (-1,0)
--
-- >>> commandDir 'R'
-- (1,0)
--
-- >>> commandDir 'U'
-- (0,1)
--
-- >>> commandDir 'D'
-- (0,-1)
commandDir :: Command -> Vector
commandDir 'L' = (-1, 0)
commandDir 'R' = (1, 0)
commandDir 'U' = (0, 1)
commandDir 'D' = (0, -1)

-- | Executa (ou tenta executar) uma série de comandos, dado um certo estado de
-- jogo (cf. 'runCommand').
runCommands :: State     -- ^ Estado inicial.
            -> [Command] -- ^ Comandos a serem executados.
            -> State     -- ^ Estado após a execução dos comandos.
runCommands state []     = state
runCommands state (c:cs) =
    if stateIsSolved state
        then state
        else runCommands (runCommand state c) cs

-- | Executa (ou tenta executar) um comando, dado um certo estado de jogo.
-- Devolve o estado após a execução do programa.
--
-- O boneco apenas se pode movimentar se:
--
-- * a célula de destino estiver vazia, __OU__
-- * a célula de destino contiver uma caixa mas a célula imediatamente a seguir
-- estiver vazia.
--
-- Se não for possível mover o boneco na direção representada pelo comando, o
-- estado não é alterado.
runCommand :: State   -- ^ Estado inicial.
           -> Command -- ^ Comando a ser executado.
           -> State   -- ^ Estado após a execução do comando.
runCommand state cmd
    | cellIsClear cellFwd1 =
        State {
            stateBoard     = board,
            statePlayer    = fwd1,
            stateTickCount = ticks + 1
            }
    | cellIsBox cellFwd1 && cellIsClear cellFwd2 =
        State {
            stateBoard     = boardPutBox fwd2 . boardRemoveBox fwd1 $ board,
            statePlayer    = fwd1,
            stateTickCount = ticks + 1
            }
    | otherwise =
        state
    where
        board    = stateBoard state
        player   = statePlayer state
        ticks    = stateTickCount state
        dir      = commandDir cmd
        fwd1     = player |+| dir
        fwd2     = fwd1   |+| dir
        cellFwd1 = getCell fwd1 board
        cellFwd2 = getCell fwd2 board

--------------------------------------------------------------------------------

runHUTests = runTestTT $ TestList [
    tests_vecAdd,
    tests_replaceElemAt,
    tests_parseState,
    tests_cellIsClear,
    tests_cellIsBox,
    tests_boardPutBox,
    tests_boardRemoveBox,
    tests_cellPutBox,
    tests_cellRemoveBox,
    tests_parseCoords
    ]

return []
runQCTests = $(quickCheckAll)

--------------------------------------------------------------------------------
