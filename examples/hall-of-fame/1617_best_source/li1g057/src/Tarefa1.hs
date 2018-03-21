module Tarefa1 where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.List

{-|===Função:
@A função 'mapa' devolve uma lista de Strings que representam o mapa completo do jogo com os power-ups@

===Variáveis:
@ __:: Int:__ Tamanho do mapa 
__ -> Int:__ Semente@

===Observação:
@No caso em que o mapa tem o tamanho 5x5 tivemos que colocar exatamente o mapa pois estava a dar erro no SVN@
 -}
mapa :: Int -> Int -> [String]
mapa 5 seed = ["#####","#   #","# # #","#   #","#####"]
mapa size seed = (mapAllTogether size seed (size-1)) ++ (bombDetector size seed (size-1)) ++ (flameDetector size seed (size-1))

main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"

{-|===Função:
@A função 'mapAllTogether' devolve uma lista de Strings que representam o mapa completo do jogo@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente
__-> Int:__ Linha do mapa@ -}
mapAllTogether :: Int -> Int -> Int -> [String]
mapAllTogether size seed 0 = [randomMap size seed 0 True]
mapAllTogether size seed l = mapAllTogether size seed (l-1) ++ [randomMap size seed l True]

{-|===Função:
@A função randomList devolve uma lista de Int's com números geridos aleatoriamente de 0 a 99 na qual a seed vai ser o número pelo qual esses 
números vão ser geridos@ 

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente@ -}
randomList :: Int -> Int -> [Int]
randomList size seed = take (numOpenSpace size) $ randomRs (0,99) (mkStdGen seed)

{-|===Função: 
@A função 'tradutor' vai receber uma lista de números aleatórios e vai transformar os números aleatórios em tijolos, bombas, flames ou espaços vazios. 
__Bool == True:__ A função trabalha com os tijolos dentro do mapa
__Bool == False:__ A função trabalha com as bombas e os flames@ 

===Variáveis:
@__::[Int]:__ Lista de Int's 
__-> Bool:__ Vai servir de switch@

===Observação:
@__(?):__ Tijolos
__(+):__ Power-up Bombas
__(!):__ Power-Up flames
__(' '):__ Espaços vazios@ -}
tradutor :: [Int] -> Bool -> String
tradutor [] _ = []
tradutor (h:t) n | h < 40 && n = '?' : tradutor t n
                 | h < 2 = '+' : tradutor t n
                 | h < 4 = '!' : tradutor t n
                 | otherwise = ' ' : tradutor t n

{-|===Função: 
@A função 'randomMap' devolve uma String que representa o mapa do jogo@
 
===Variáveis:
@__:: Int:__ Tamanho do mapa 
__-> Int:__ Semente
__-> Int:__ Linha do mapa 
__-> Bool:__ Bool para ser usado na função auxiliar 'tradutor'@
    
===Observação:
@Na função 'randomMap' vamos usar a função auxiliar 'fillOpenSpace' para preencher os espaços vazios disponíveis com os números aleatórios gerados@ -}
randomMap :: Int -> Int -> Int -> Bool -> String
randomMap size seed l b | l == size = []
                        | l == 0 || l == (size - 1) = replicate size '#'
                        | l == 1 || l == (size - 2) = "#  " ++ fillOpenSpace v l size ++ "  #"
                        | l == 2 || l == (size - 3) = "# #" ++ intersperse '#' (fillOpenSpace v l size) ++ "# #"
                        | odd l = "#" ++ fillOpenSpace v l size ++ "#"
                        | even l = "#" ++ intersperse '#' (fillOpenSpace v l size) ++ "#"
                        where v = tradutor (randomList size seed) b

{-|===Função: 
@A função 'bombDetector' vai percorrer o mapa e vai detetar onde existem bombas. De seguida, vai colocar abaixo do mapa, a posição das bombas@

===Variáveis:
@__:: Int:__ Tamanho do mapa 
__-> Int:__ Semente
__-> Int:__ Linha do mapa@ -}
bombDetector :: Int -> Int -> Int -> [String]
bombDetector size seed 0 = []
bombDetector size seed l = bombDetector size seed (l-1) ++ map (\h -> "+ " ++ show h ++ " " ++ show l) (elemIndices '+' $ randomMap size seed l False)
                         where v = tradutor (randomList size seed)

{-|===Função:
@A função 'flameDetector' vai percorrer o mapa e vai detetar onde existem flames. De seguida, vai colocar abaixo do mapa, as posições das flames@

===Variáveis:
@__:: Int:__ Tamanho do mapa
__-> Int:__ Semente 
__-> Int:__ Linha do mapa@ -}
flameDetector :: Int -> Int -> Int -> [String]
flameDetector size seed 0 = []
flameDetector size seed l = flameDetector size seed (l-1) ++ map (\h -> "! " ++ show h ++ " " ++ show l) (elemIndices '!' $ randomMap size seed l False)
                         where v = tradutor (randomList size seed)

{-|===Função: 
@A função 'numOpenSpaceLine' vai-nos devolver um Int que representa o número de espaços vazios de uma linha sabendo que há espaços vazios já predefinidos@

===Variáveis:
@__:: Int:__ Linha do mapa 
__-> Int:__ Tamanho do mapa@ -}
numOpenSpaceLine :: Int -> Int -> Int
numOpenSpaceLine _ 5 = 0
numOpenSpaceLine l size | l == 0 || l == (size - 1) = 0
                        | l == 1 || l == (size - 2) = size - 6
                        | l == 2 || l == (size - 3) = size - 4 - ((div size 2) - 1)
                        | odd l = size - 2
                        | even l = size - 2 - ((div size 2) - 1)

{-|===Função:
@A função 'gathered' é uma função auxiliar para a função fillOpenSpace, devolve o número de elementos que a função drop tem de dropar. A função vai verificar às 
linhas anteriores quantos elementos tem para depois a 'fillOpenSpace' dropar esses elementos todos@

===Variáveis:
@__:: Int:__ Linha do mapa
__-> Int:__ Tamanho do mapa@ -}
gathered :: Int -> Int -> Int
gathered _ 5 = 0
gathered l size | l == 0 = 0
                | l == 1 || l == (size - 1) = gathered (l-1) size
                | l == 2 || l == (size - 1) = numOpenSpaceLine 1 size + gathered (l-1) size
                | l == 3 || l == (size - 2) = numOpenSpaceLine 2 size + gathered (l-1) size
                | odd l = numOpenSpaceLine (l-1) size + gathered (l-1) size
                | even l = numOpenSpaceLine (l-1) size + gathered (l-1) size

{-|===Função: 
@ A função 'fillOpenSpace' preenche os espaços vazios com os números aleatórios da função 'tradutor'. A função 'fillOpenSpace', através da função 'numOpenSpaceLine',
vai contar quantos espaços vazios tem o mapa disponíveis para preencher com números aleatórios. A função take vai tirar os elementos necessários 
para preencher corretamente a lista e a função drop vai dropar o número de elementos já saídos@

===Variáveis:
@__:: String:__ Lista dos elementos aleatórios do mapa
__-> Int:__ Número da linha do mapa
__-> Int (size):__ Tamanho do mapa@

===Observação:
@Na função 'fillOpenSpace' vamos usar as funções auxiliares 'numOpenSpaceLine' ,que vai devolver-nos um Int que vai ser o número de espaços vazios
que tem uma linha do mapa, e a função 'gathered' que devolve um Int que vai ser o número de elementos necessários dropar, visto que o mapa tem que ser
preenchido aleatoriamente com os números da 'randomList' e todos os números dessa lista devem ser usados@

===Exemplo:
@Num mapa 7x7, o número de espaços disponíveis para colocar números da 'randomList' são 9@

@Seja l a lista de Int's formada pela função 'randomList'. Então, neste caso, o que queremos fazer é o seguinte:@

@take 1 (drop 0 l)
take 1 (drop 1 l)
take 5 (drop 2 l)
take 1 (drop 7 l)
take 1 (drop 8 l)@

@Desta maneira garantimos que todos os número da função 'randomList' vão ser usados e nenhum vai ser repetido@ -}
fillOpenSpace :: String -> Int -> Int -> String
fillOpenSpace l 0 size = ""
fillOpenSpace l n size = take (numOpenSpaceLine n size) $ drop (gathered n size ) l

{-|===Função:
@ A função 'numOpenSpace' devolve um Intque corresponde ao número de espaços vazios que tem o mapa, no total@ 

===Variáveis: 
@__::Int:__ Tamanho do mapa@ -}
numOpenSpace :: Int -> Int
numOpenSpace 5 = 0
numOpenSpace n  = (n^2) - (n * 4 - 4) - 12 - ((div n 2) - 1)^2
