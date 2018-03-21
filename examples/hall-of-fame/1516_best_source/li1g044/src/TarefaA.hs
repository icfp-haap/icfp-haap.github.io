--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : TarefaA
Description : Resolução da Tarefa A.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo contém uma resolução correspondente à __Tarefa A__ da primeira fase
do projeto de Laboratórios de Informática I.

O objetivo é o de verificar se o input fornecido através do @stdin@ corresponde
ao formato especificado pelo enunciado do projeto, devolvendo através do
@stdout@ uma mensagem de sucesso ou o número da linha onde for encontrada a
primeira divergência.
-}
module TarefaA where

import Data.Char
import Data.List
import Data.Maybe
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
          putStr (outStr (tarefaA (inStr inp)))

--------------------------------------------------------------------------------
-- * Entrada

-- | Função inicial da tarefa. Recebe o input do programa e devolve o output
-- correspondente.
tarefaA :: [String] -> [String]
tarefaA input = [ testResultToString $ testInput (toLineList input) ]

--------------------------------------------------------------------------------
-- * Tipos Auxiliares

-- $
-- Tipos genéricos utilizados pelo programa.

-- | Representa um ponto no plano.
type Point = (Int, Int)

-- | Representa uma dimensão no plano.
type Size = (Int, Int)

--------------------------------------------------------------------------------
-- * Processamento do Input

-- $
-- Tipos e funções destinados ao processamento e manipulação do input do
-- programa.

-- | Representa uma linha de input numerada.
data Line =
    Line {
        lineNum  :: LineNumber, -- ^ O número da linha.
        lineText :: String      -- ^ O conteúdo da linha.
    }

-- | Sinónimo de 'Line', utilizado para distinguir linhas correspondentes à
-- secção do tabuleiro.
type BoardLine = Line

-- | Sinónimo de 'Line', utilizado para distinguir linhas correspondentes à
-- secção das coordenadas.
type CoordsLine = Line

-- | Representa o número de uma linha de input.
type LineNumber = Int

-- | Converte uma lista de @String@s numa lista de linhas numeradas.
--
-- ==== Exemplos de utilização:
-- >>> toLineList []
-- []
--
-- >>> toLineList ["ABC", "123"]
-- [ Line {lineNum = 1, lineText = "ABC"},Line {lineNum = 2, lineText = "123"} ]
--
-- >>> toLineList ["#####", "#  .#", "#####"]
-- [ Line {lineNum = 1, lineText = "#####"},
-- Line {lineNum = 2, lineText = "#  .#"},
-- Line {lineNum = 3, lineText = "#####"} ]
toLineList :: [String] -> [Line]
toLineList = zipWith Line [1..]

-- | Verifica se um valor do tipo 'Line' representa uma linha __não__ vazia.
--
-- ==== Exemplos de utilização:
-- >>> lineIsNotEmpty (Line 42 "")
-- False
--
-- >>> lineIsNotEmpty (Line 42 "Ola!")
-- True
lineIsNotEmpty :: Line -> Bool
lineIsNotEmpty = not . null . lineText

-- | Divide uma lista de linhas de input em duas secções.
--
-- Se as linhas representarem corretamente um tabuleiro de jogo e as coordenadas
-- do boneco e das caixas, a primeira secção corresponderá ao tabuleiro e a
-- segunda às coordenadas.
splitInput :: [Line] -> ([BoardLine], [CoordsLine])
splitInput = break isValidCoordFormat

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
--
-- Pode tomar os seguintes valores:
--
-- * @\'#\'@ - a célula é uma __parede__;
-- * @\' \'@ - a célula é __vazia__;
-- * @\'.\'@ - a célula é um __local de arrumação__.
type Cell = Char

-- | Constrói um valor do tipo 'Board' a partir de uma representação textual
-- válida.
--
-- ==== Exemplos de utilização:
-- >>> parseBoard [Line 1 "#####", Line 2 "#  .#", Line 3 "#####"]
-- Board { boardSize = (5,3), boardCells = ["#####","#  .#","#####"] }
parseBoard :: [Line] -> Board
parseBoard lns@(ln:_) = Board size (map lineText lns)
    where size = (length . lineText $ ln, length lns)

-- | Devolve a célula de um tabuleiro com as coordenadas especificadas.
--
-- ==== Exemplos de utilização:
-- >>> getCell (3,1) (parseBoard [Line 1 "#####",Line 2 "#  .#",Line 3 "#####"])
-- '.'
--
-- >>> getCell (0,1) (parseBoard [Line 1 "#####",Line 2 "#  .#",Line 3 "#####"])
-- '#'
getCell :: Point -- ^ Coordenadas da célula.
        -> Board -- ^ Tabuleiro.
        -> Cell  -- ^ Célula nas coordenadas especificadas.
getCell (x,y) (Board (_,h) cells) = (cells !! line) !! column
    where (line, column) = (h-y-1, x)

-- | Calcula o número de locais de arrumação presentes num tabuleiro, ou seja, o
-- número de células de valor @\'.\'@ existentes no tabuleiro.
--
-- ==== Exemplos de utilização:
-- >>> numTargets (parseBoard [Line 1 "#####", Line 2 "#   #", Line 3 "#####"])
-- 0
--
-- >>> numTargets (parseBoard [Line 1 "#####", Line 2 "#  .#", Line 3 "#####"])
-- 1
--
-- >>> numTargets (parseBoard [Line 1 "#####", Line 2 "#. .#", Line 3 "#####"])
-- 2
numTargets :: Board -> Int
numTargets board = sum $ map (length . filter (== '.')) cells
    where cells = boardCells board

--------------------------------------------------------------------------------
-- ** Coordenadas

-- | Verifica se uma linha de input representa corretamente um par de
-- coordenadas.
--
-- ==== Exemplos de utilização:
-- >>> isValidCoordFormat (Line 42 "")
-- False
--
-- >>> isValidCoordFormat (Line 42 "5 b")
-- False
--
-- >>> isValidCoordFormat (Line 42 "1 3 7")
-- False
--
-- >>> isValidCoordFormat (Line 42 "1 3")
-- True
isValidCoordFormat :: Line -> Bool
isValidCoordFormat ln =
    length split == 2 && all isDigit xStr && all isDigit yStr
    where
        split       = words $ lineText ln
        [xStr,yStr] = split

--------------------------------------------------------------------------------
-- * Testes

-- | Representa o resultado de um teste.
data TestResult
    = Ok               -- ^ O teste passou.
    | Error LineNumber -- ^ O teste falhou na linha de número especificado.

-- | Devolve o primeiro elemento com a forma @'Error' n@ de uma lista do tipo
-- @['TestResult']@.
--
-- Se a lista não possuir elementos com essa forma, é devolvido o valor 'Ok'.
--
-- ==== Exemplos de utilização:
-- >>> firstError []
-- Ok
--
-- >>> firstError [Ok, Ok]
-- Ok
--
-- >>> firstError [Error 7, Error 1]
-- Error 7
--
-- >>> firstError [Error 4, Error 2, Ok, Error 9]
-- Error 4
firstError :: [TestResult] -> TestResult
firstError []        = Ok
firstError (Ok  : t) = firstError t
firstError (err : _) = err

-- | Devolve o elemento com a forma @'Error' n@ de menor @n@, de uma lista do
-- tipo @['TestResult']@.
--
-- Se a lista não possuir elementos com essa forma, é devolvido o valor 'Ok'.
--
-- ==== Exemplos de utilização:
-- >>> lowestError []
-- Ok
--
-- >>> lowestError [Ok, Ok]
-- Ok
--
-- >>> lowestError [Error 7, Error 1]
-- Error 1
--
-- >>> lowestError [Error 4, Error 2, Ok, Error 9]
-- Error 2
lowestError :: [TestResult] -> TestResult
lowestError []            = Ok
lowestError (Ok      : t) = lowestError t
lowestError (Error n : t) =
    case lowestError t of
        Ok          -> Error n
        Error nTail -> Error (min n nTail)

-- | Converte um valor do tipo 'TestResult' numa representação textual
-- equivalente.
--
-- ==== Exemplos de utilização:
-- >>> testResultToString Ok
-- "OK"
--
-- >>> testResultToString (Error 42)
-- "42"
testResultToString :: TestResult -> String
testResultToString Ok        = "OK"
testResultToString (Error n) = show n

-- | Testa se o input do programa segue o formato especificado pelo enunciado do
-- projeto.
--
-- Se o input seguir o formato em questão, é devolvido o valor 'Ok'. Em caso
-- contrário, é devolvido o valor @'Error' n@, onde @n@ corresponde ao número da
-- linha onde for encontrada a primeira divergência.
testInput :: [Line]     -- ^ Input do programa.
          -> TestResult -- ^ Resultado do teste.
testInput lnsInput = firstError [
    testBoard  lnsBoard,
    testCoords board lnsCoords
    ]
    where
        (lnsBoard, lnsCoords) = splitInput lnsInput
        board = parseBoard lnsBoard

--------------------------------------------------------------------------------
-- ** Tabuleiro

-- | Testa se a secção do input do programa correspondente ao tabuleiro segue o
-- formato especificado pelo enunciado do projeto.
--
-- Se o input seguir o formato em questão, é devolvido o valor 'Ok'. Em caso
-- contrário, é devolvido o valor @'Error' n@, onde @n@ corresponde ao número da
-- linha onde for encontrada a primeira divergência.
testBoard :: [BoardLine] -- ^ Linhas de input a testar.
          -> TestResult  -- ^ Resultado do teste.
testBoard lnsBoard = lowestError [
    testBoardChars  lnsBoard,
    testBoardShape  lnsBoard,
    testBoardBounds lnsBoard
    ]

-- | Testa se todas as linhas de uma lista de linhas de input apenas contêm
-- carateres válidos.
--
-- Os únicos carateres válidos são os seguintes: @\'#\'@, @\' \'@, @\'.\'@.
--
-- Se não forem encontrados carateres inválidos, é devolvido o valor 'Ok'. Em
-- caso contrário, é devolvido o valor @'Error' n@, onde @n@ corresponde ao
-- número da primeira linha que contém um carater inválido.
--
-- ==== Exemplos de utilização:
-- >>> testBoardChars [Line 1 "#####", Line 2 "#  .#", Line 3 "#####"]
-- Ok
--
-- >>> testBoardChars [Line 1 "#####", Line 2 "# X.#", Line 3 "#####"]
-- Error 2
--
-- >>> testBoardChars [Line 1 "#####", Line 2 "# X.#", Line 3 "#Ola#"]
-- Error 2
testBoardChars :: [BoardLine] -- ^ Linhas de input a testar.
               -> TestResult  -- ^ Resultado do teste.
testBoardChars lnsBoard = maybe Ok (Error . lineNum) badLine
    where badLine = find (any (`notElem` "# .") . lineText) lnsBoard

-- | Testa se todas as linhas de uma lista de linhas de input são __não__ vazias
-- e possuem o mesmo comprimento.
--
-- Se tal se verificar, é devolvido o valor 'Ok'. Em caso contrário, é devolvido
-- o valor @'Error' n@, onde @n@ corresponde ao número da primeira linha vazia
-- ou de comprimento diferente à anterior.
--
-- ==== Exemplos de utilização:
-- >>> testBoardShape [Line 1 "#####", Line 2 "#  .#", Line 3 "#####"]
-- Ok
--
-- >>> testBoardShape [Line 1 "#####", Line 2 "#  .#", Line 3 ""]
-- Error 3
--
-- >>> testBoardShape [Line 1 "######", Line 2 "#  .#", Line 3 "######"]
-- Error 2
testBoardShape :: [BoardLine] -- ^ Linhas de input a testar.
               -> TestResult  -- ^ Resultado do teste.
testBoardShape [] = Error 1
testBoardShape (ln:lns)
    | lenFirst == 0 = Error 1
    | otherwise     = maybe Ok (Error . lineNum) badLine
    where
        lenFirst = length $ lineText ln
        badLine = find ((/= lenFirst) . length . lineText) lns

-- | Testa se a primeira e última linhas de uma lista de linhas de input apenas
-- contêm o carater @\'#\'@ e as restantes são prefixadas e sufixadas pelo
-- carater @\'#\'@.
--
-- Se tal se verificar, é devolvido o valor 'Ok'. Em caso contrário, é devolvido
-- o valor @'Error' n@, onde @n@ corresponde ao número da primeira linha que não
-- siga o formato esperado.
--
-- ==== Exemplos de utilização:
-- >>> testBoardBounds [Line 1 "#####", Line 2 "#  .#", Line 3 "#####"]
-- Ok
--
-- >>> testBoardBounds [Line 1 "#####", Line 2 "#  . ", Line 3 "#####"]
-- Error 2
--
-- >>> testBoardBounds [Line 1 "#####", Line 2 "#  .#", Line 3 "#   #"]
-- Error 3
testBoardBounds :: [BoardLine] -- ^ Linhas de input a testar.
                -> TestResult  -- ^ Resultado do teste.
testBoardBounds [] = Error 1
testBoardBounds [ln]
    | any (/= '#') (lineText ln) = Error (lineNum ln)
    | otherwise                  = Ok
testBoardBounds lnsBoard@(ln:lns)
    | any (/= '#') (lineText ln)  = Error (lineNum ln)
    | isJust badMidLine           = Error (lineNum . fromJust $ badMidLine)
    | any (/= '#') (lineText btm) = Error (lineNum btm)
    | otherwise                   = Ok
    where
        btm = last lns
        badMidLine = find (not . isValidMidLine) (init lns)
        isValidMidLine (Line _ l) = isPrefixOf "#" l && isSuffixOf "#" l

--------------------------------------------------------------------------------
-- ** Coordenadas

-- | Testa se a secção do input do programa correspondente às coordenadas segue
-- o formato especificado pelo enunciado do projeto, tendo em conta um
-- tabuleiro.
--
-- Se o input seguir o formato em questão, é devolvido o valor 'Ok'. Em caso
-- contrário, é devolvido o valor @'Error' n@, onde @n@ corresponde ao número da
-- linha onde for encontrada a primeira divergência.
testCoords :: Board        -- ^ Tabuleiro.
           -> [CoordsLine] -- ^ Linhas de input a testar.
           -> TestResult   -- ^ Resultado do teste.
testCoords board lnsCoords = lowestError [
    testCoordsEmptyLines lnsCoords,
    testCoordsCount      board nonEmptyLnsCoords,
    testCoordsLines      board nonEmptyLnsCoords
    ]
    where nonEmptyLnsCoords = takeWhile lineIsNotEmpty lnsCoords

-- | Testa se não existem linhas __não__ vazias após linhas vazias, numa lista
-- de linhas de input.
--
-- Se após a primeira a linha vazia apenas existirem linhas vazias, é devolvido
-- o valor 'Ok'. Em caso contrário, é devolvido o valor @'Error' n@, onde @n@
-- corresponde ao número da primeira linha __não__ vazia após a primeira linha
-- vazia.
--
-- ==== Exemplos de utilização:
-- >>> testCoordsEmptyLines []
-- Ok
--
-- >>> testCoordsEmptyLines [Line 13 "A", Line 14 "", Line 15 ""]
-- Ok
--
-- >>> testCoordsEmptyLines [Line 13 "A", Line 14 "", Line 15 "C"]
-- Error 15
testCoordsEmptyLines :: [CoordsLine] -- ^ Linhas de input a testar.
                     -> TestResult   -- ^ Resultado do teste.
testCoordsEmptyLines lnsCoords = maybe Ok (Error . lineNum) badLine
    where badLine = find lineIsNotEmpty (dropWhile lineIsNotEmpty lnsCoords)

-- | Testa se o número de coordenadas descritas por uma lista de linhas é
-- válido, tendo em conta o número de locais de arrumação num tabuleiro.
--
-- As linhas devem conter as coordenadas do boneco e as coordenadas de cada
-- caixa. Devem existir tantas caixas como locais de arrumação. Assim, o número
-- de linhas deve ser dado pela expressão:
--
-- @1 + nº de locais de arrumação no tabuleiro@
--
-- Se o número de linhas for __igual__ ao número esperado, é devolvido o valor
-- 'Ok'.
--
-- Se o número de linhas for __inferior__ ao número esperado, é devolvido o
-- valor @'Error' n@, onde @n@ corresponde ao número da linha que deveria
-- existir após a última linha na lista.
--
-- Se o número de linhas for __superior__ ao número esperado, é devolvido o
-- valor @'Error' n@, onde @n@ corresponde ao número da primeira linha em
-- excesso.
testCoordsCount :: Board        -- ^ Tabuleiro.
                -> [CoordsLine] -- ^ Linhas de input a testar.
                -> TestResult   -- ^ Resultado do teste.
testCoordsCount board [] = Error (h+1)
    where (_,h) = boardSize board
testCoordsCount board lnsCoords
    | nActual == nExpected = Ok
    | nActual <  nExpected = Error (fstLineNum + nActual)
    | nActual >  nExpected = Error (fstLineNum + nExpected)
    where
        nActual    = length lnsCoords
        nExpected  = 1 + numTargets board
        fstLineNum = lineNum $ head lnsCoords

-- | Testa se todas as linhas de uma lista de linhas de input representam
-- corretamente um par de coordenadas válido.
--
-- Para um par de coordenadas ser válido, este deve corresponder a uma posição
-- dentro do tabuleiro especificado e a uma célula que não seja uma parede. Não
-- deve existir também mais de uma linha a representar o mesmo ponto.
--
-- Se todas as linhas forem válidas, é devolvido o valor 'Ok'. Em caso
-- contrário, é devolvido o valor @'Error' n@, onde @n@ corresponde ao número da
-- primeira linha não válida.
testCoordsLines :: Board        -- ^ Tabuleiro.
                -> [CoordsLine] -- ^ Linhas de input a testar.
                -> TestResult   -- ^ Resultado do teste.
testCoordsLines board lnsCoords = test board lnsCoords []
    where
        test _     []       _   = Ok
        test board (ln:lns) pts =
            either (test board lns) Error (testCoordsLine board ln pts)

-- | Testa se uma linha de input representa corretamente um par de coordenadas
-- válido.
--
-- Para o par de coordenadas ser válido, este deve corresponder a uma posição
-- dentro do tabuleiro especificado e a uma célula que não seja uma parede. Não
-- se deve encontrar também na lista de pontos especificada.
--
-- Se a linha __passar__ o teste, é devolvido o valor @'Left' pts@, onde @pts@
-- corresponde à lista de pontos passada como argumento, aumentada com o par de
-- coordenadas que a linha representa.
--
-- Se a linha __falhar__ o teste, é devolvido o valor @'Right' n@, onde @n@
-- corresponde ao número da linha.
testCoordsLine :: Board                     -- ^ Tabuleiro.
               -> CoordsLine                -- ^ Linha de input a testar.
               -> [Point]                   -- ^ Lista de pontos inválidos.
               -> Either [Point] LineNumber -- ^ Resultado do teste.
testCoordsLine board ln pts =
    if passed
        then Left  ((x,y) : pts)
        else Right (lineNum ln)
    where
        split       = words $ lineText ln
        [xStr,yStr] = split
        (x,y)       = (read xStr, read yStr) :: Point
        (w,h)       = boardSize board
        passed      =
            length split == 2 && all isDigit xStr && all isDigit yStr
            && x >= 0 && y >= 0 && x < w && y < h
            && getCell (x,y) board /= '#'
            && (x,y) `notElem` pts

--------------------------------------------------------------------------------
