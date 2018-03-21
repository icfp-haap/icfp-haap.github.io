{- |
Module: Main
Description: Tarefa 3 da 1ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 3 da 1ª Fase do projeto de LI1.
-}


module Main where

import Data.List
import System.Directory
import Data.Char
import qualified Data.Text as T


-- * Funções de Teste


-- | Corre múltiplos testes para a Tarefa 3.

correTestes :: IO ()
correTestes = do
    files3 <- getDirectoryContents "../tests/T3"
    let inputs3 = map ("../tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    mapM_ (correTeste tarefa3) inputs3


-- | Corre um teste para a Tarefa 3.

correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    let nome = reverse $ drop 3 $ reverse input
    inp <- readFile input
    let o = outStr (tarefa (inStr inp))
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp)
    then putStrLn "OK"
    else do
        putStrLn "FALHOU"
        putStr esp
        putStrLn o


-- * Funções da Tarefa 3

-- | Esta função recebe uma string e transforma-a numa lista de strings, dividindo-a quando encontrar um @__'\n'__@.

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]


-- | Esta função recebe uma lista de strings e transforma-a numa única string, adicionando @__'\n'__@ entre as strings originais.

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)


-- | Função geral da Tarefa 3.

main = do inp <- getContents
          putStr (outStr (tarefa3 (inStr inp)))


tarefa3 :: [String] -> [String]
tarefa3 linhas = trans (move (head (last (processa coords))) (head coords2) coords2 (reverse tab))
    where
        (tab,coords) = parteMapa linhas
        coords2 = leCoordenadas (init(processa coords))
        trans (c,d) = [show c ++ " " ++ show d] -- Função que transforma o par de coordenadas numa lista de strings.

{- ^ Função que recebe um tabuleiro, coordenadas e uma letra (L, U, R, D) e devolve a coordenada final do boneco.

Exemplo de utilização:

@
 >>> tarefa3 ["\#\#\#\#\#\#\#\#\#\#\#","\#       ..\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#\#\#\#\#\#\#\#\#\#\#","1 4","4 2","4 3",\"D"]
 [\"1 3"]
@

-}


{-^
 = Funções Auxiliares
-}


processa :: [String] -> [String]
processa [] = []
processa (x:xs) = if and (map isEspaco (x:xs)) then [] else x:processa xs
       where isEspaco [] = True             
             isEspaco (x:xs) = if x == ' ' then isEspaco xs else False -- Verifica se uma linha é vazia.

{- ^ Função que apaga eventuais listas vazias no final do tabuleiro.

@
>>> processa ["\#\#\#\#\#","\#   \#","\#\#. \#","\#\#\#\#\#","     "]
["\#\#\#\#\#","\#   \#","\#\#. \#","\#\#\#\#\#"]
@

-}


{- | Função que divide a parte das coordenadas da parte do tabuleiro.

@
 >>> parteMapa ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#",\"1 1"]
 (["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"],["1 1"])
@

-}

parteMapa :: [String] -> ([String],[String])
parteMapa linhas = splitAt (comprimento linhas) linhas
      where comprimento [] = 0
            comprimento ([]:xs) = comprimento xs
            comprimento ((a:b):xs) = if isDigit a then 0 else 1 + comprimento xs


leCoordenadas :: [String] -> [(Int,Int)]
leCoordenadas xs = map leCoordenada (xs)
      where leCoordenada s = (read x,read y) -- Função que transforma uma coordenada para o formato (x,y).
             where [x,y] = words s 

{- ^ Função que recebe a parte das coordenadas e as transforma para o formato (x,y).

@
 >>> leCoordenadas ["1 1",\"3 4"]
[(1,1),(3,4)]
@

-}


{-^
 = Funções Principais
-}


move :: Char -> (Int,Int) -> [(Int,Int)] -> [String] -> (Int,Int)
move x (a,b) l2 l1 | x == 'L' &&  moveL (a,b) l2 l1 = (a-1,b)
                   | x == 'R' &&  moveR (a,b) l2 l1 = (a+1,b)
                   | x == 'U' &&  moveU (a,b) l2 l1 = (a,b+1)
                   | x == 'D' &&  moveD (a,b) l2 l1 = (a,b-1)
                   | otherwise = (a,b)

{- ^ Ao receber L, R , U ou D, modifica a coordenada para a Esquerda, Direita, Cima ou Baixo, respetivamente, caso possível.

@
>>> move \'R'\ (1,1) [(1,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
\(2,1)
@

-}


{-^
 == Funções específicas para cada movimento 
-}


moveL :: (Int,Int) -> [(Int,Int)] -> [String] -> Bool
moveL (a,b) (x:xs) l1 | isParede (a-1,b) l1  = False
                      | isCaixa (a-1,b) xs && isParede (a-2,b) l1 = False
                      | isCaixa (a-1,b) xs && isCaixa (a-2,b) xs = False
                      | otherwise = True

{- ^  Esta função verifica se é possível a movimentação do boneco para esquerda. Caso seja possível, ela devolve /True/, e no outro caso devolve /False/.

@
>>> moveL (2,1) [(2,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
True
@

@
>>> moveL (1,1) [(1,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
False
@

-}

{- | Esta função verifica se é possível a movimentação do boneco para a direita. Caso seja possível, ela devolve /True/, e no outro caso devolve /False/.

@
>>> moveR (1,1) [(1,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
True
@

@
>>> moveR (3,1) [(3,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
False
@


-}

moveR :: (Int,Int) -> [(Int,Int)] -> [String] -> Bool
moveR (a,b) (x:xs) l1 | isParede (a+1,b) l1  = False
                      | isCaixa (a+1,b) xs && isParede (a+2,b) l1 = False
                      | isCaixa (a+1,b) xs && isCaixa (a+2,b) xs = False
                      | otherwise = True

{- | Esta função verifica se é possível a movimentação do boneco para cima. Caso seja possível, ela devolve /True/, e no outro caso devolve /False/.

@
>>> moveU (1,1) [(1,1),(2,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
True
@

@
>>> moveU (1,1) [(1,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
False
@

-}

moveU :: (Int,Int) -> [(Int,Int)] -> [String] -> Bool
moveU (a,b) (x:xs) l1 | isParede (a,b+1) l1  = False
                      | isCaixa (a,b+1) xs && isParede (a,b+2) l1  = False
                      | isCaixa (a,b+1) xs && isCaixa (a,b+2) xs = False
                      | otherwise = True


moveD :: (Int,Int) -> [(Int,Int)] -> [String] -> Bool
moveD (a,b) (x:xs) l1 | isParede (a,b-1) l1 = False
                      | isCaixa (a,b-1) xs && isParede (a,b-2) l1 = False
                      | isCaixa (a,b-1) xs && isCaixa (a,b-2) xs = False
                      | otherwise = True

{- ^ Esta função verifica se é possível a movimentação do boneco para baixo. Caso seja possível, ela devolve /True/, e no outro caso devolve /False/.

@
>>> moveD (1,2) [(1,2),(2,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
True
@

@
>>> moveD (1,1) [(1,1),(1,2)] ["\#\#\#\#\#","\#   \#","\#   \#","\#\#\#\#\#"]
False
@

-}

{-^
 == Funções que verificam se tem uma caixa ou é parede na coordenada pedida
-}


isCaixa :: (Int,Int) -> [(Int,Int)] -> Bool
isCaixa (a,b) l2 = or (map (isSame (a,b)) l2) 
                     where isSame (a,b) (c,d) = (a==c) && (b==d) -- Verifica se duas coordenadas são iguais.

{- ^ Verifica se numa determinada coordenada está uma caixa.

@
>>> isCaixa (2,3) [(2,3),(3,7),(4,5)]
True
@

@
>>> isCaixa (0,0) [(2,3),(3,7),(4,5)]
False
@

-}


{- | Recebendo um par de coordenadas e um tabuleiro, verifica se o elemento nessa coordenada é parede (__@'#'@__).

@
>>> isParede (0,0) ["\#\#\#\#","\#  \#","\#\#\#\#"]
True
@

@
>>> isParede (1,1) ["\#\#\#\#","\#  \#","\#\#\#\#"]
False
@

-}

isParede :: (Int,Int) -> [String] -> Bool
isParede (a,b) l1 = (l1!!b!!a) == '#' 
