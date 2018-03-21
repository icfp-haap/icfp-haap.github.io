{- |
Module: Main
Description: Tarefa 2 da 1ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 2 da 1ª Fase do projeto de LI1.
-}


module Main where

import Data.List
import System.Directory
import Data.Char
import qualified Data.Text as T


-- * Funções de Teste

-- | Corre múltiplos testes para a Tarefa 2.

correTestes :: IO ()
correTestes = do
    files2 <- getDirectoryContents "../tests/T2"
    let inputs2 = map ("../tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    mapM_ (correTeste tarefa2) inputs2


-- | Corre um teste para a Tarefa 2.

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


-- * Funções da Tarefa 2

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


-- | Função geral da Tarefa 2.

main = do inp <- getContents
          putStr (outStr (tarefa2 (inStr inp)))


tarefa2 :: [String] -> [String]
tarefa2 linhas = remove (0,0) (altera tab coords2) (altera tab coords2)
         where
           (tab,coords) = parteMapa linhas
           coords2 = leCoordenadas (processa coords)

{- ^ Esta função recebe um tabuleiro e coordenadas e devolve o tabuleiro no formato pedido.

Exemplo de utilização:

@
 >>> tarefa2 ["\#\#\#\#\#\#\#\#\#\#\#","\#       ..\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#\#\#\#\#\#\#\#\#\#\#","1 4","4 2","4 3"]
 ["\#\#\#\#\#\#\#\#\#\#\#","\#o      ..\#","\#   H \#\#\#\#\#","\#   H \#    ","\#     \#    ","\#\#\#\#\#\#\#    "]
@

-}


{- ^
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
      where comprimento [] = 0  --  Função que calcula o comprimento do tabuleiro (sem a parte das coordenadas).
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


{- ^
 = Funções Principais
-}

{- ^
 == Funções que inserem o boneco e as caixas no mapa
-} 


altera :: [String] -> [(Int,Int)] -> [String]
altera l ((a,b):c) = reverse (inserir  (alteraboneco (a,b) (alteracaixa l c))  (reverse (alteracaixa l c)) b)

{- ^ Insere as caixas e o boneco nas posições respetivas (de acordo com as coordenadas dadas).

@
>>> altera ["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"] [(2,3),(2,4),(4,3),(6,2),(7,3)]
["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\#H\#\#\#   \#","\# o H  H \#","\# ..\# H \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"]
@

-}


{- | Muda uma linha do tabuleiro por uma nova, no formato correto.

@
>>> inserir "\#\#\o\#\#" ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 1
["\#\#\#\#\#","\#\#\o\#\#","\#\#\#\#\#"]
@

-}

inserir :: String -> [String] -> Int -> [String]
inserir l1 [] b = []
inserir l1 (x:xs) b | b == 0 = l1:xs
                    | otherwise = x:inserir l1 xs (b-1)

{- | Esta função recebe as coordenadas do boneco e devolve a linha na qual ele se encontra, já com o caractere @__\'o'__@ no sítio correto.

@
>>> alteraboneco (2,1) ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 
"\#\#\o\#\#"
@

-}

alteraboneco :: (Int,Int) -> [String] -> String
alteraboneco (a,b) l1 = ncoluna a ((reverse l1)!!b)
           where ncoluna a (x:xs) | a == 0 = 'o': xs    --  Numa linha do tabuleiro, esta função vai encontrar a posição do boneco e substituí-la por um 'o'.
                                  | otherwise = x:ncoluna (a-1) xs


alteracaixa :: [String] -> [(Int,Int)] -> [String]
alteracaixa l [] = l
alteracaixa l (x:xs) = alteracaixa (alteracaixa1 (reverse l) x) xs
                    where alteracaixa1 l (a,b) = reverse (inserir  (alteracaixa2 (a,b) l) l b)--  Recebe um tabuleiro e um par de coordenadas e insere um 'H' ou um 'I' nessa posição.
                                  where alteracaixa2 (a,b) l1 = ncoluna1 a (l1!!b) -- Esta função recebe as coordenadas de uma caixa e devolve a linha na qual ele se encontra, já com o caractere 'I' ou 'H' no sítio.
                                                 where ncoluna1 a (x:xs) | a == 0 = beornotobe x : xs     --  Numa linha do tabuleiro, esta função vai encontrar a posição da caixa e substituí-la por um 'H' ou um 'I'.
                                                                         | otherwise = x:ncoluna1 (a-1) xs
                                                                where beornotobe x | x == '.' = 'I'  --  Função que recebe um caractere e muda-o para I se este for um '.'. Se não for um ponto, muda-o para H.
                                                                                   | otherwise = 'H'

{- ^ Recebe um tabuleiro e uma lista de coordenadas e insere um @__\'H'__@ ou um @__\'I'__@ nas posições das caixas.

@
>>> alteracaixa ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] [(2,1)] 
["\#\#\#\#\#","\#\#\H\#\#","\#\#\#\#\#"]
@

-}


{- ^
 == Funções que removem @__'#'__@ redundantes
-} 

remove :: (Int,Int) -> [String] -> [String] -> [String]
remove (a,b) [] l = []
remove (a,b) (x:xs) l = remove1 x (a,b) l ++ remove (a,b+1) xs l
            where remove1 [] (c,d) l = []      --  Esta função recebe uma linha do tabuleiro e remove os carateres '#' que são redundantes nessa linha.
                  remove1 (a:b) (c,d) l = [add x]
                           where x = [(remove2 a (c,d) l)] ++ remove1 b (c+1,d) l
                                 add [] = []      --  Esta função transforma uma lista de strings numa só string.
                                 add (x:xs) = x ++ add xs

{- ^ Retira os caracteres @__'#'__@ que forem redundantes.

@
>>>  remove (0,0) ["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"] ["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"]
[" \#\#\#\#\#\#\#  "," \#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \# "," \#\#\#\#\#\#\#\# "]
@

-}

{- | Verifica se a remoção do caractere @__'#'__@ é possível. Caso seja possível, remove-o; caso contrário, mantém-no.


@
>>>  remove2 \'#'\ (1,1) ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 
"#"
@

@
>>>  remove2 \'#'\ (0,0) ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 
" "
@

-}

remove2 :: Char -> (Int,Int) -> [String] -> String
remove2 a (c,d) l = if (a == '#' && ((isOcto (c+1,d) l) ==  '#') && ((isOcto (c-1,d) l) == '#') && ((isOcto (c,d+1) l) == '#') && ((isOcto (c,d-1) l) == '#') && ((isOcto (c-1,d-1) l) == '#') && ((isOcto (c+1,d-1) l) == '#')  && ((isOcto (c-1,d+1) l) == '#') && ((isOcto (c+1,d+1) l) == '#')) then " " else [a]
                         where 
                               isOcto (a,-1) l1 = '#'  -- Dá-nos o caractere num determinado local do tabuleiro e caso este esteja fora deste, assume que é um '#'.
                               isOcto (-1, b) l1 = '#'
                               isOcto (a,b) l1 = if b == (length l1) then '#' else ncoluna2 a (l1!!b)
                                            where  -- Recebe uma posição e uma linha, e devolve o caractere que se encontra nessa posição.
                                                ncoluna2 a [x] = x -- Este caso faz que no final de uma linha, mesmo que o ponto esteja fora do mapa, devolva '#'.
                                                ncoluna2 a (x:xs) | a == 0 = x
                                                                  | otherwise = ncoluna2 (a-1) xs 
