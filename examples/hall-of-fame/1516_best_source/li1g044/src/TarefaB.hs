--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TarefaB
Description : Resolução da Tarefa B.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém uma resolução correspondente à __Tarefa B__ da primeira fase
do projeto de Laboratórios de Informática I.

O objetivo é o de apresentar no @stdout@ uma visualização do tabuleiro de jogo
que inclua as caixas e o boneco, a partir do input fornecido através do @stdin@.
-}
module TarefaB where

import qualified Data.Text as T

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
          putStr (outStr (tarefaB (inStr inp)))

--------------------------------------------------------------------------------
-- * Entrada

-- | Função inicial da tarefa. Recebe o input do programa e devolve o output
-- correspondente.
tarefaB :: [String] -> [String]
tarefaB input = boardCells $ makePrettyBoard board coords
    where (board, coords) = parseInput input

--------------------------------------------------------------------------------
-- * Tipos e Funções Auxiliares

-- $
-- Tipos e funções genéricos utilizados pelo programa.

-- | Representa um ponto no plano.
type Point = (Int, Int)

-- | Representa uma dimensão no plano.
type Size = (Int, Int)

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

--------------------------------------------------------------------------------
-- * Processamento do Input

-- $
-- Tipos e funções destinados ao processamento e manipulação do input do
-- programa.

-- | Contrói um tuplo com valores dos tipos 'Board' e 'Coords' a partir de uma
-- representação textual adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseInput ["#####", "#  .#", "#####", "1 1", "2 1", "3 1"]
-- ( Board { boardSize = (5,3), boardCells = ["#####","#  .#","#####"] },
-- Coords { coordsPlayer = (1,1), coordsBoxes = [(2,1),(3,1)] } )
parseInput :: [String] -> (Board, Coords)
parseInput input1 = (parseBoard lnsBoard, parseCoords lnsCoords)
    where
        (lnsBoard, input2) = span ((== '#') . head) input1
        lnsCoords          = takeWhile (not . null) input2

--------------------------------------------------------------------------------
-- ** Tabuleiro

-- $
-- Tipos e funções específicos ao processamento de tabuleiros de jogo.

-- | Representa um tabuleiro.
data Board =
    Board {
        boardSize  :: Size,    -- ^ O tamanho do tabuleiro.
        boardCells :: [[Cell]] -- ^ As células do tabuleiro.
    }

-- | Representa uma célula de um tabuleiro.
type Cell = Char

-- | Constrói um valor do tipo 'Board' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseBoard ["#####", "#  .#", "#####"]
-- Board { boardSize = (5,3), boardCells = ["#####","#  .#","#####"] }
parseBoard :: [String] -> Board
parseBoard cells@(l:_) = Board size cells
    where size = (length l, length cells)

-- | Devolve a célula de um tabuleiro com as coordenadas especificadas.
--
-- ==== Exemplos de utilização:
-- >>> getCell (3,1) (parseBoard ["#####", "#  .#", "#####"])
-- '.'
--
-- >>> getCell (0,1) (parseBoard ["#####", "#  .#", "#####"])
-- '#'
getCell :: Point -- ^ Coordenadas da célula.
        -> Board -- ^ Tabuleiro.
        -> Cell  -- ^ Célula nas coordenadas especificadas.
getCell (x,y) (Board (_,h) cells) = (cells !! line) !! column
    where (line, column) = (h-y-1, x)

-- | Substitui uma célula de um tabuleiro pelo valor especificado e devolve o
-- tabuleiro resultante.
--
-- ==== Exemplos de utilização:
-- >>> setCell ' ' (3,1) (parseBoard ["#####", "#  .#", "#####"])
-- Board { boardSize = (5,3), boardCells = ["#####","#   #","#####"] }
--
-- >>> setCell '#' (2,1) (parseBoard ["#####", "#  .#", "#####"])
-- Board { boardSize = (5,3), boardCells = ["#####","# #.#","#####"] }
setCell :: Cell  -- ^ Valor pelo qual substituir a célula.
        -> Point -- ^ Coordenadas da célula.
        -> Board -- ^ Tabuleiro original.
        -> Board -- ^ Tabuleiro resultante.
setCell val (x,y) (Board (w,h) cells) =
    Board
        (w,h)
        (replaceElemAt line (replaceElemAt column val (cells !! line)) cells)
    where (line, column) = (h-y-1, x)

-- | Verifica se uma célula de um tabuleiro é uma parede.
--
-- Se as coordenadas especificadas não corresponderem a uma posição válida
-- dentro do tabuleiro, considera-se que o resultado é verdadeiro.
--
-- ==== Exemplos de utilização:
-- >>> cellIsWall (parseBoard ["#####", "#  .#", "#####"]) (1,1)
-- False
--
-- >>> cellIsWall (parseBoard ["#####", "#  .#", "#####"]) (1,2)
-- True
--
-- >>> cellIsWall (parseBoard ["#####", "#  .#", "#####"]) (-1,-3)
-- True
cellIsWall :: Board -- ^ Tabuleiro.
           -> Point -- ^ Coordenadas da célula.
           -> Bool  -- ^ 'True' se a célula em questão for considerada uma
                    -- parede. Em caso contrário, 'False'.
cellIsWall board (x,y) =
    (x < 0 || y < 0 || x >= w || y >= h) || (getCell (x,y) board == '#')
    where (w,h) = boardSize board

-- | Substitui por uma célula vazia as células de um tabuleiro que sejam paredes
-- e estejam rodeadas por paredes.
--
-- ==== Exemplos de utilização:
-- >>> removeUnnecessaryWalls (parseBoard ["#####", "## .#", "#  ##", "#####"])
-- Board { boardSize = (5,4), boardCells = [" ####","## .#","#  ##","#### "] }
removeUnnecessaryWalls :: Board -- ^ Tabuleiro original.
                       -> Board -- ^ Tabuleiro resultante.
removeUnnecessaryWalls board =
    foldr (removeIfIsUnnecessaryWall board) board points
    where
        points = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
        (w,h)  = boardSize board

-- | Substitui uma célula por uma célula vazia se esta for uma parede e estiver
-- rodeada por paredes.
--
-- A verificação do valor da célula e das células à sua volta é feita sobre um
-- tabuleiro, mas a alteração é feita a outro tabuleiro. Assim, a função recebe
-- dois tabuleiros.
removeIfIsUnnecessaryWall :: Board -- ^ Tabuleiro para a verificação.
                          -> Point -- ^ Coordenadas da célula.
                          -> Board -- ^ Tabuleiro a ser modificado.
                          -> Board -- ^ Tabuleiro resultante.
removeIfIsUnnecessaryWall initialBoard (x,y) board =
    if all (cellIsWall initialBoard) cellsAround
        then setCell ' ' (x,y) board
        else board
    where
        cellsAround = [
            (x-1, y-1), (x, y-1), (x+1, y-1),
            (x-1, y  ), (x, y  ), (x+1, y  ),
            (x-1, y+1), (x, y+1), (x+1, y+1)
            ]

--------------------------------------------------------------------------------
-- ** Coordenadas

-- $
-- Tipos e funções relacionados com o processamento de coordenadas.

-- | Representa as coordenadas (posições no tabuleiro) do boneco e de todas as
-- caixas.
data Coords =
    Coords {
        coordsPlayer :: Point,  -- ^ As coordenadas do boneco.
        coordsBoxes  :: [Point] -- ^ As coordenadas de todas as caixas.
    }

-- | Constrói um valor do tipo 'Coords' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseCoords ["11 2", "5 8", "7 7", "5 6"]
-- Coords { coordsPlayer = (11,2), coordsBoxes = [(5,8),(7,7),(5,6)] }
parseCoords :: [String] -> Coords
parseCoords (player:boxes) = Coords (parse player) (map parse boxes)
    where parse l = let [x,y] = words l in (read x, read y) :: Point

-- | Modifica um tabuleiro para que nele fiquem representados o boneco e as
-- caixas nas células em que estes se encontrem.
--
-- A célula na qual o boneco se encontra (a qual se assume estar vazia) passa a
-- ter o valor @\'o\'@.
--
-- As células nas quais se encontram caixas passam a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
applyCoordsToBoard :: Coords -- ^ Coordenadas do boneco e das caixas.
                   -> Board  -- ^ Tabuleiro original.
                   -> Board  -- ^ Tabuleiro resultante.
applyCoordsToBoard coords board =
    applyBoxesToBoard boxes . applyPlayerToBoard player $ board
    where
        player = coordsPlayer coords
        boxes  = coordsBoxes coords

-- | Modifica um tabuleiro para que nele fique representado o boneco na célula
-- em que este se encontre.
--
-- A célula na qual o boneco se encontra (a qual se assume estar vazia) passa a
-- ter o valor @\'o\'@.
applyPlayerToBoard :: Point -- ^ Coordenadas do boneco.
                   -> Board -- ^ Tabuleiro original.
                   -> Board -- ^ Tabuleiro resultante.
applyPlayerToBoard = setCell 'o'

-- | Modifica um tabuleiro para que nele fiquem representadas as caixas nas
-- células em que estas se encontrem.
--
-- As células nas quais se encontram caixas passam a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
applyBoxesToBoard :: [Point] -- ^ Coordenadas das caixas.
                  -> Board   -- ^ Tabuleiro original.
                  -> Board   -- ^ Tabuleiro resultante.
applyBoxesToBoard boxes board = foldr applyBoxToBoard board boxes

-- | Modifica um tabuleiro para que nele fique representada uma caixa na célula
-- em que esta se encontre.
--
-- A célula na qual se encontra a caixa passa a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
applyBoxToBoard :: Point -- ^ Coordenadas da caixa.
                -> Board -- ^ Tabuleiro original.
                -> Board -- ^ Tabuleiro resultante.
applyBoxToBoard box board = setCell val box board
    where val = if getCell box board == '.' then 'I' else 'H'

-- | Modifica um tabuleiro para que nele fiquem representados o boneco e as
-- caixas, substituindo também por célula vazias as paredes desnecessárias.
--
-- A célula na qual o boneco se encontra (a qual se assume estar vazia) passa a
-- ter o valor @\'o\'@.
--
-- As células nas quais se encontram caixas passam a ter o valor:
--
-- * @\'H\'@ se a célula estiver vazia;
-- * @\'I\'@ se a célula for um local de arrumação.
--
-- São consideradas desnecessárias as células que sejam paredes e estejam
-- rodeadas por paredes.
makePrettyBoard :: Board  -- ^ Tabuleiro original.
                -> Coords -- ^ Coordenadas do boneco e das caixas.
                -> Board  -- ^ Tabuleiro resultante.
makePrettyBoard board coords =
    applyCoordsToBoard coords . removeUnnecessaryWalls $ board

--------------------------------------------------------------------------------
