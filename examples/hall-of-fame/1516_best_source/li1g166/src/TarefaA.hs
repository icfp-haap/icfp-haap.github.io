{-|
Module: Main
Description: Tarefa 1 da 1ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 1 da 1ª Fase do projeto de LI1.
-}


module Main where

import Data.List
import System.Directory
import Data.Char
import qualified Data.Text as T


-- * Funções de teste

-- | Corre múltiplos testes para a tarefa 1.

correTestes :: IO ()
correTestes = do
    files1 <- getDirectoryContents "../tests/T1"
    let inputs1 = map ("../tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    mapM_ (correTeste tarefa1) inputs1


-- | Corre um teste para uma tarefa.

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


-- * Funções da Tarefa 1

-- | Esta função recebe uma string e transforma-a numa lista de strings, dividindo-a quando encontrar um __@'\n'@__.

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]


-- | Esta função recebe uma lista de strings e transforma-a numa única string, adicionando __@'\n'@__ entre as strings originais.

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)


-- | Função geral da tarefa 1.

main = do inp <- getContents
          putStr (outStr (tarefa1 (inStr inp)))



{- |Esta função recebe um tabuleiro e verifica se este está no formato correto, devolvendo __@[\"OK"]@__, em caso afirmativo, ou a linha onde está o primeiro erro, em caso contrário. Seguem-se dois exemplos:

@
 >>> tarefa1 ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#",\"1 1"]
 [\"OK"]
@

@
 >>> tarefa1 ["\#\#\#\#\#","\# ..\#","\#   \#","\#\#\#\#\#",\"0 0",\"2 2"]
 ["5"]
@

-}


tarefa1 :: [String] -> [String]
tarefa1 linhas = if erro <0 then ["OK"] else [show erro]
     where erro = tarefa1Erro linhas


tarefa1Erro :: [String] -> Int
tarefa1Erro linhas = juntaErros okTab okCoords
  where
    (tab,coords) = parteMapa linhas
    okTab = validaTab tab
    okCoords = juntaErros (validaCoords (lerCoordenadas (processa coords)) (reverse tab)) (pro (length tab + 1)  (processa coords) tab)
          where pro a [] l1 = a   -- Verifica se o mapa tem coordenadas e, se não tiver, dá a linha de erro que é o comprimento do tabuleiro + 1. Se tiver tudo bem, verifica se estão no formato pedido.
                pro a l2 l1 = processaCoordenadas a l2 


{- ^ Esta função recebe um tabuleiro e devolve a linha com erro, se houver e caso contrário, devolve __@-1@__.

@
 >>> tarefa1Erro ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#",\"1 1"]
 -1
@

@
 >>> tarefa1Erro ["\#\#\#\#\#","\# ..\#","\#   \#","\#\#\#\#\#",\"0 0",\"2 2"]
 5
@

-}



{- ^
 = Funções auxiliares
-}



juntaErros :: Int -> Int -> Int
juntaErros i j | i<0 = j
               | j<0 = i
               |otherwise = min i j


{- ^ Função que devolve a linha menor dos erros encontrados. Se não foram encontrados erros devolve -1.

@
 >>> juntaErros (-1) 3
 3
@

@
 >>> juntaErros (-1) (-1)
 -1
@

@
 >>> juntaErros 1 4
 1  
@

-}



{- | Verifica que se as coordenadas estão no formato pedido.

@
 >>> processaCoordenadas 7 ["1 1",\"3 4"]
-1
@

@
 >>> processaCoordenadas 7 ["1 a"]
7
@

@
 >>> processaCoordenadas 7 ["1 1",\"3  "]
8
@

-}

processaCoordenadas :: Int -> [String] -> Int
processaCoordenadas l [] = -1
processaCoordenadas l (x:xs) = juntaErros erro1 erro2
  where
    erro1 = processaCoordenada l x
    erro2 = processaCoordenadas (l+1) xs
    processaCoordenada l s = processaCoord l coords  -- Separa os elementos de uma linha de coordenadas.
           where
              coords = words s 
              processaCoord l [x,y] = if isCoord x && isCoord y then -1 else l  --  Verifica se o par de coordenadas tem somente dois elementos.
                         where isCoord s = and (map isDigit s) --  Verifica se as coordenadas são todas números, devolvendo /True/ em caso afirmativo, e /False/ em caso negativo.
              processaCoord l xs = l

{- | Função que apaga eventuais listas vazias no final do tabuleiro.

@
>>> processa ["\#\#\#\#\#","\#   \#","\#\#. \#","\#\#\#\#\#","     "]
["\#\#\#\#\#","\#   \#","\#\#. \#","\#\#\#\#\#"]
@

-}

processa :: [String] -> [String]
processa [] = []
processa (x:xs) = if  isEspaco x then [] else x:processa xs
       where isEspaco [] = True                   
             isEspaco x = and (map (\a -> a==' ' ) x) -- Verifica se uma linha é vazia.


{- | Função que divide a parte das coordenadas da parte do tabuleiro.


@
 >>> parteMapa ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#",\"1 1"]
 (["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"],["1 1"])
@

-}


parteMapa :: [String] -> ([String],[String])
parteMapa linhas = splitAt (comprimento linhas) linhas
      where comprimento [] = 0 -- Função que calcula o comprimento do tabuleiro (sem a parte das coordenadas).
            comprimento ([]:xs) = comprimento xs
            comprimento ((a:b):xs) = if isDigit a then 0 else 1 + comprimento xs



lerCoordenadas :: [String] -> [(Int,Int)]
lerCoordenadas x =if  (processaCoordenadas 1 x) == (-1) then  leCoordenadas x else []
                     where leCoordenadas xs = map leCoordenada xs  --  Função que recebe a parte das coordenadas e as transforma para o formato (x,y).
                             where leCoordenada s = (read x,read y) --  Função que transforma uma coordenada para o formato (x,y).
                                     where [x,y] = words s

{- ^ Caso o 'processaCoordenadas' verifique que as coordenadas estão formato pedido, esta função transforma as coordenadas no formato (x,y).

@
 >>> lerCoordenadas ["1 1",\"3 4"]
[(1,1),(3,4)]
@

-}

{- ^
 = Funções que verificam o tabuleiro
-}

validaTab :: [String] -> Int
validaTab [] = 1
validaTab ([]:xs) = 1
validaTab linhas = juntaErros erroLinhas erroOutro 
          where
              erroLinhas = validaTab1 1 linhas
              erroOutro = juntaErros (validaTab2 linhas) (validaTab3 2 linhas)




{- ^Verifica se o tabuleiro está bem construído.

@
 >>> validaTab ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaTab ["\# \#\#\#","\# \#\#\#","\#\#\#\#\#"]
 1
@

-}

{- | Verifica se o ficheiro é apenas composto pelos caracteres @__'#'__@, @__\' '__@ ou @__\'.'__@ e se cada linha acaba e começa em @__'#'__@.

@
 >>> validaTab1 1 ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaTab1 1 ["\#\#\#\#\#","\# \#\#\#","\#\#\2\#\#"]
 3
@

@
 >>> validaTab1 1 ["\#\#\#\#\#","\  \#\#\#","\#\#\#\#\#"]
 2
@

-}


validaTab1 :: Int -> [String] -> Int
validaTab1 pos [] = -1
validaTab1 pos (h:t) = juntaErros taberroLinha taberroLinhas
        where
          taberroLinha = validaLinhaTab pos h
            where validaLinhaTab x l = if ((and (map isCorrectChar l)) &&  isOctophorp l) then (-1) else x --  Verifica se o ficheiro é apenas composto pelos caracteres '#', ' ' ou '.' e se cada linha acaba e começa em '#', através das funções 'isCorrectChar' e 'isOctophorp'.
                                 where isCorrectChar a =  (a == '#') || (a == ' ') || (a == '.') -- Verifica se o tabuleiro é apenas composto pelos caracteres '#', ' ' ou '.'.
                                       isOctophorp x = (head x == '#') && (last x == '#') -- Verifica se o primeiro e o último elemento de uma linha são '#'.
          taberroLinhas = validaTab1 (pos+1) t

{- | Verifica se a primeira e a última linha são constituídas apenas por @__\'#'__@.

@
 >>> validaTab2 ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaTab2 ["\#\#\#\#\#","\# \#\#\#","\#\#\ \#\#"]
 3
@

-}


validaTab2 :: [String] -> Int
validaTab2 [] = 1
validaTab2 (l:ls) | and (map isOnlyOcto l)  == False = 1 -- Verifica se uma linha é constituída apenas por cardinais.
                  | and (map isOnlyOcto (last (l:ls))) == False = length (l:ls) -- Verifica se uma linha é constituída apenas por cardinais.
                  | otherwise = (-1) 
                      where isOnlyOcto a = a == '#'



validaTab3 :: Int -> [String] -> Int
validaTab3 c [] = -1
validaTab3 c [a] = -1
validaTab3 c (l:la:ls) = if length l == length la then validaTab3 (c+1) (la:ls) else c


{- ^ Verifica se as linhas têm todas o mesmo comprimento.

@
 >>> validaTab3 2 ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaTab3 2 ["\#\#\#\#\#","\# \#\#","\#\#\ \#\#"]
 2
@

-}

{- ^
 = Funções que verificam as coordenadas
-}


validaCoords :: [(Int,Int)] -> [String] -> Int
validaCoords [] l1 = -1
validaCoords l2 [] = 1
validaCoords lista2 lista1 = juntaErros errolista2 errolista2'
            where 
                 errolista2 = juntaErros (validaCoords1 (1 + length lista1) lista2 lista1) (validaCoords4 (1 + length lista1) lista2)
                 errolista2' = juntaErros (validaCoords2 (1 + length lista1) lista2 lista1) (validaCoords3 lista2 lista1)

{- ^ Verifica se as coordenadas estão em locais possíveis do tabuleiro:

* não estão fora do tabuleiro;

* não estão em cima de paredes, etc...

@
 >>> validaCoords  [(1,1)] ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaCoords [(3,4)] ["\# \#\#\#","\# \#\#\#","\#\#\#\#\#"] 
 4
@

-}

{- | Verifica se todos os pares de coordenadas se encontram dentro do tabuleiro.

@
 >>> validaCoords1 7 [(1,1)] ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaCoords1 7 [(3,4)] ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 7
@

-}


validaCoords1 :: Int -> [(Int,Int)] -> [String] -> Int
validaCoords1 pos [] _ = (-1)
validaCoords1 pos ((x,y):ls) l1 = juntaErros erroLinha2 erroLinhas2
          where
            erroLinha2 = validaCoords1' pos (x,y) l1
                where validaCoords1' x (a,b) (c:d) = if (a >= 0 && a < length c) && (b >= 0 && b < length (c:d)) then (-1) else x --  Verifica se um par de coordenadas se encontra dentro do tabuleiro.
            erroLinhas2 = validaCoords1 (pos+1) ls l1

{- | Verifica se todas as coordenadas estão num @__\'.'__@ ou num @__\' '__@.

@
 >>> validaCoords2 7 [(1,1)] ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaCoords2 7 [(2,2)] ["\#\#\#\#\#","\# \#\#\#","\#\#\#\#\#"]
 7
@

-}

validaCoords2 :: Int -> [(Int,Int)] -> [String] -> Int
validaCoords2 pos [] l1 = (-1)
validaCoords2 pos ((x,y):ls) l1 = if (validaCoords1 (1 + length l1) ((x,y):ls) l1) == (-1) then juntaErros erroLinha3 erroLinhas3 else (-1)
          where
            erroLinha3 = validaCoords2' pos (x,y) l1
                where validaCoords2' x (a,b) l1 = if elemento (a,b) l1 == ' ' || elemento (a,b) l1 == '.'  then (-1) else x  --  Verifica se um par de coordenadas está num '.' ou num ' '.
                        where elemento (a,b) l1 = l1 !! b!! a --  Esta função recebe umas coordenadas e devolve o elemento que está nessa posição.
            erroLinhas3 = validaCoords2 (pos+1) ls l1
             
{- | Verifica se o tabuleiro tem tantos locais de arrumações (__@\'.'@__) quanto o número de caixas.

@
 >>> validaCoords3 [(1,1),(1,2)] ["\#\#\#\#\#","\# \#\#\#","\#.\#\#\#","\#\#\#\#\#"]
 -1
@

@
 >>> validaCoords3 [(1,1)] ["\#\#\#\#\#","\# \#\#\#","\#.\#\#\#","\#\#\#\#\#"]
 6
@

-}

validaCoords3 :: [(Int,Int)] -> [String] -> Int
validaCoords3 l2 l1 = if (length l2-1) > narrumacao l1 then (length l1 + (narrumacao l1) + 2) else if (length l2 - 1) < narrumacao l1 then ((length l2 + length l1) + 1) else (-1)
                                   where narrumacao l1 = sum ( map narrumacao1 l1) --  Conta os locais de arrumação num tabuleiro.
                                                             where narrumacao1 [] = 0    -- Conta os locais de arrumação numa linha.
                                                                   narrumacao1 (a:b) | a == '.' = 1 + narrumacao1 b
                                                                                     | otherwise = narrumacao1 b

{- | Verifica se a parte das coordenadas não tem coordenadas repetidas.

@
 >>> validaCoords4 7 [(2,3),(3,7),(4,5)]
 -1
@

@
 >>> validaCoords4 7 [(2,3),(3,7),(2,3)]
 9
@

-}

validaCoords4 :: Int -> [(Int,Int)] -> Int
validaCoords4 x [] = (-1)
validaCoords4 x ((a,b):ls) = juntaErros erroLinha5 erroLinhas5
          where
            erroLinha5 = validaCoords4' x (a,b) ls
             where validaCoords4' x (a,b) [] = -1  --  Compara uma coordenada com as restantes, verificando se são iguais.
                   validaCoords4' x (a,b) ((c,d):e) = if a==c && b==d then x+1 else validaCoords4' (x+1) (a,b) e
            erroLinhas5 = validaCoords4 (x+1) ls