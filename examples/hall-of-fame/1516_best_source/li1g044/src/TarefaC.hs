--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TarefaC
Description : Resolução da Tarefa C.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém uma resolução correspondente à __Tarefa C__ da primeira fase
do projeto de Laboratórios de Informática I.

O objetivo é o de apresentar no @stdout@ a posição do boneco após a execução (ou
tentativa de execução) de um comando, a partir do input fornecido através do
@stdin@.
-}
module TarefaC where

import Data.Char
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
          putStr (outStr (tarefaC (inStr inp)))

--------------------------------------------------------------------------------
-- * Entrada

-- | Função inicial da tarefa. Recebe o input do programa e devolve o output
-- correspondente.
tarefaC :: [String] -> [String]
tarefaC input = [ vectorToString $ runCommand board coords command ]
    where (board, coords, command) = parseInput input

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

-- | Converte um valor do tipo 'Vector' numa representação textual equivalente.
--
-- ==== Exemplos de utilização:
-- >>> vectorToString (1,2)
-- "1 2"
--
-- >>> vectorToString (-3,-4)
-- "-3 -4"
vectorToString :: Vector -> String
vectorToString (x,y) = show x ++ " " ++ show y

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

-- | Constrói um tuplo com valores dos tipos 'Board', 'Coords' e 'Command' a
-- partir de uma representação textual adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseInput ["#####", "#  .#", "#####", "1 1", "2 1", "3 1", "R"]
-- ( Board { boardSize = (5,3), boardCells = ["#####","#  .#","#####"] },
-- Coords { coordsPlayer = (1,1), coordsBoxes = [(2,1),(3,1)] },
-- 'R' )
parseInput :: [String] -> (Board, Coords, Command)
parseInput input1 =
    (parseBoard lnsBoard, parseCoords lnsCoords, parseCommand lnCommand)
    where
        (lnsBoard , input2) = span ((== '#') . head) input1
        (lnsCoords, input3) = span (isDigit  . head) input2
        [lnCommand]         = takeWhile (not . null) input3

--------------------------------------------------------------------------------
-- ** Tabuleiro

-- $
-- Tipos e funções específicos ao processamento de tabuleiros de jogo.

-- | Representa um tabuleiro.
data Board =
    Board {
        boardSize  :: Vector,  -- ^ O tamanho do tabuleiro.
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
getCell :: Vector -- ^ Coordenadas da célula.
        -> Board  -- ^ Tabuleiro.
        -> Cell   -- ^ Célula nas coordenadas especificadas.
getCell (x,y) (Board (_,h) cells) = (cells !! line) !! column
    where (line, column) = (h-y-1, x)

--------------------------------------------------------------------------------
-- ** Coordenadas

-- $
-- Tipos e funções relacionados com o processamento de coordenadas.

-- | Representa as coordenadas (posições no tabuleiro) do boneco e de todas as
-- caixas.
data Coords =
    Coords {
        coordsPlayer :: Vector,  -- ^ As coordenadas do boneco.
        coordsBoxes  :: [Vector] -- ^ As coordenadas de todas as caixas.
    }

-- | Constrói um valor do tipo 'Coords' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseCoords ["11 2", "5 8", "7 7", "5 6"]
-- Coords { coordsPlayer = (11,2), coordsBoxes = [(5,8),(7,7),(5,6)] }
parseCoords :: [String] -> Coords
parseCoords (player:boxes) = Coords (parse player) (map parse boxes)
    where parse l = let [x,y] = words l in (read x, read y) :: Vector

--------------------------------------------------------------------------------
-- ** Comando

-- $
-- Tipos e funções relacionados com o comando que deve ser executado.

-- | Representa um comando.
--
-- Pode tomar os seguintes valores: @\'L\'@, @\'R\'@, @\'U\'@, @\'D\'@.
type Command = Char

-- | Contrói um valor do tipo 'Command' a partir de uma representação textual
-- adequada.
--
-- ==== Exemplos de utilização:
-- >>> parseCommand "L"
-- 'L'
--
-- >>> parseCommand "R"
-- 'R'
--
-- >>> parseCommand "U"
-- 'U'
--
-- >>> parseCommand "D"
-- 'D'
parseCommand :: String -> Command
parseCommand = head

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

-- | Executa (ou tenta executar) um comando, dado um certo tabuleiro e
-- coordenadas. Devolve as coordenadas do boneco após a execução do comando.
--
-- O boneco apenas se pode movimentar se:
--
-- * a célula de destino estiver vazia, __OU__
-- * a célula de destino contiver uma caixa mas a célula imediatamente a seguir
-- estiver vazia.
--
-- Se não for possível mover o boneco na direção representada pelo comando, as
-- suas coordenadas permanecem inalteradas.
runCommand :: Board   -- ^ Tabuleiro.
           -> Coords  -- ^ Coordenadas do boneco e das caixas.
           -> Command -- ^ Comando a executar.
           -> Vector  -- ^ Coordenadas do boneco após a execução do comando.
runCommand board coords command = if canMove then fwd1 else player
    where
        player   = coordsPlayer coords
        boxes    = coordsBoxes coords
        dir      = commandDir command
        fwd1     = player |+| dir
        fwd2     = fwd1   |+| dir
        cellFwd1 = getCell fwd1 board
        cellFwd2 = getCell fwd2 board
        canMove  =
            if (fwd1 `notElem` boxes)
                then (cellFwd1 /= '#')
                else (fwd2 `notElem` boxes) && (cellFwd2 /= '#')

--------------------------------------------------------------------------------
