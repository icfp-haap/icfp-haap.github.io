{- |
Module: Main
Description: Tarefa 4 da 2ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 4 da 2ª Fase do projeto de LI1.
-}


module Main where
import qualified Data.Text as T
import Data.Char
import Graphics.Gloss
{-import System.FilePath.Find
import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit.Adapter


-- * Testes
-- | Função de testes.


tests :: IO Test
tests = do
    let hunit = TestLabel "HUnit" $ TestList [testsTD]
    mooshak <- testesMooshak
    return $ TestList [hunit,mooshak]

-- | Função geral e função de testes.

main = do
    tt <- tests
    defaultMain $ testGroup "Tests" $ hUnitTestToTestTree tt


-- ** Testes unitários
-- | Testes unitários para a tarefa 4.

testsTD = TestLabel "Tarefa D" $ TestList [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19]
   where t1 = TestCase (assertEqual "t1" ["INCOMPLETO 2"] (tarefa4 ["###################","#####   ###########","#####   ###########","#####   ###########","###      ##########","### # ## ##########","#   # ## #####  ..#","#               ..#","##### ### # ##  ..#","#####     #########","###################","11 2","5 8","7 7","5 6","7 6","2 3","5 3","LURD"]))
         t2 = TestCase (assertEqual "t2" (["###################","#####   ###########","#####   ###########","#####   ###########","###      ##########","### # ## ##########","#   # ## #####  ..#","#               ..#","##### ### # ##  ..#","#####     #########","###################"],["11 2","5 8","7 7","5 6","7 6","2 3","5 3","LURD"]) (parteMapa ["###################","#####   ###########","#####   ###########","#####   ###########","###      ##########","### # ## ##########","#   # ## #####  ..#","#               ..#","##### ### # ##  ..#","#####     #########","###################","11 2","5 8","7 7","5 6","7 6","2 3","5 3","LURD"]))
         t3 = TestCase (assertEqual "t3" ["FIM 32"] (tarefa4 ["###########","#       ..#","#     #####","#     #####","#     #####","###########","1 4","4 2","4 3","DDRRRDRUUDLLUURRRRRLLLDDLULURRRR"]))
         t4 = TestCase (assertEqual "t4" True (isCaixa (2,3) [(2,3),(3,7),(4,5)]))
         t5 = TestCase (assertEqual "t5" False (isParede (1,1) ["####","#  #","####"]))
         t6 = TestCase (assertEqual "t6" ["INCOMPLETO 5"] (tarefa4 ["##########","##     ###","## ###   #","#        #","# ..#   ##","##..#   ##","##########","2 3","2 4","2 1","6 2","7 3","ULRLUUDDDRRL"]))
         t7 = TestCase (assertEqual "t7" ["INCOMPLETO 8"] (tarefa4 ["##################","##########   #####","########## # ##  #","##########       #","#....   ##       #","#....          ###","#....   ##     ###","##########      ##","##########     ###","############    ##","############ ## ##","############    ##","##################","15 5","12 10","15 9","11 8","14 8","10 7","12 7","13 7","10 6","13 6","12 5","11 4","13 4","URLDDDUULRR"]))
         t8 = TestCase (assertEqual "t8" ["FIM 9"] (tarefa4 ["#########","#  ...  #","#       #","##     ##","###   ###","#########","4 1","3 3","4 3","5 3","ULUDRUDRULR"]))
         t9 = TestCase (assertEqual "t9" ["FIM 7"] (tarefa4 ["#######","#.    #","#     #","#     #","#     #","#######","4 1","4 2","UURULLLL"]))
         t10 = TestCase (assertEqual "t10" ["INCOMPLETO 13"] (tarefa4 ["#######","#.    #","#.    #","#     #","#     #","#######","4 1","4 2","4 3","UURULLLLRLRLRLRL"]))
         t11 = TestCase (assertEqual "t11" ["FIM 6"] (tarefa4 ["#############","##         ##","## .   # ####","##     ######","#############","8 2","4 2","ULLLDL"]))
         t12 = TestCase (assertEqual "t12" ["INCOMPLETO 4"] (tarefa4 ["######################","##############    ####","#####            .####","#####     # ##   .####","#####     ############","######################","11 2","7  2","12 3","ULRUUD"]))
         t13 = TestCase (assertEqual "t13" ["INCOMPLETO 6"] (tarefa4 ["########","###   ##","#   # ##","# #  . #","#    # #","## #   #","##   ###","########","2 1","2 5","UDLRDLRR"]))
         t14 = TestCase (assertEqual "t14" ["INCOMPLETO 1"] (tarefa4 ["########","#.     #","#.   # #","#.# #  #","### # ##","##    ##","##  ####","########","2 1","3 2","2 5","5 6","ULUU"]))
         t15 = TestCase (assertEqual "t15" ["INCOMPLETO 6"] (tarefa4 ["##########","#.....  ##","###   # ##","###  ##  #","###      #","###   #  #","####  #  #","##########","8 1","3 3","5 3","7 3","4 4","4 5","UULUUUU"]))
         t16 = TestCase (assertEqual "t16" (6,[(4,5),(4,4),(6,3),(5,3),(3,3)]) (movegeral ["##########","#.....  ##","###   # ##","###  ##  #","###      #","###   #  #","####  #  #","##########","8 1","3 3","5 3","7 3","4 4","4 5","UULUUUU"]))
         t17 = TestCase (assertEqual "t17" ["##########","#.....  ##","###   # ##","###  ##  #","###      #","###   #  #","####  #  #","##########","8 1","3 3","5 3","7 3","4 4","4 5","UULUUUU"] (processa ["##########","#.....  ##","###   # ##","###  ##  #","###      #","###   #  #","####  #  #","##########","8 1","3 3","5 3","7 3","4 4","4 5","UULUUUU","      ",""]))
         t18 = TestCase (assertEqual "t18" [(4,2),(4,6),(1,5)] (leCoordenadas ["4 2","4 6","1 5"]))
         t19 = TestCase (assertEqual "t19" False (equiv [(4,2),(4,6),(1,5)] [(4,2),(4,6),(2,5)]))

-- * Testes Mooshak
-- | Testes mooshak para a tarefa 4.

testesMooshak :: IO Test
testesMooshak = do
    inputs4 <- find (depth ==? 0) (extension ==? ".in") "../tests/T4"
    let t4 = TestLabel "Tarefa D" $ TestList $ map (testesTarefa tarefa4) inputs4
    return $ TestLabel "Mooshak" $ TestList [t4]

-- | Testes para a tarefa 4.

testesTarefa :: ([String] -> [String]) -> String -> Test
testesTarefa tarefa input = TestLabel nome $ test $ do
    -- texto do mapa
    inp <- readFile input
    let out = outStr (tarefa (inStr inp))
    -- resultado da tarefa
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    return (out == esp)
  where
    -- nome do ficheiro
    nome = reverse $ drop 3 $ reverse input

-}

-- * Funções da Tarefa 4


-- | Esta função recebe uma string e transforma-a numa lista de strings, dividindo-a quando encontrar um @__'\n'__@.

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


-- | Função geral da tarefa 4.

main = do
   inp <- getContents
   putStr (outStr (tarefa4 (inStr inp)))


tarefa4 :: [String] -> [String]
tarefa4 linhas = if equiv (coordsarrumacao tab) (snd (movegeral linhas)) then ["FIM" ++ " " ++ show (fst (movegeral linhas))] else ["INCOMPLETO" ++ " " ++ show (fst (movegeral linhas))]
            where
                 (tab,coords) = parteMapa linhas

{- ^ Função que recebe um tabuleiro, coordenadas e uma sequência de comandos (L, U, R, D) e devolve "INCOMPLETO \<tick_count\>" se o puzzle não ficar resolvido ou "FIM \<tick_count\>" se o puzzle for completado. \<tick_count\> é o número de movimentos efetuados.

Exemplo de utilização:

@
 >>> tarefa4 ["\#\#\#\#\#\#\#\#\#\#\#","\#       ..\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#     \#\#\#\#\#","\#\#\#\#\#\#\#\#\#\#\#","1 4","4 2","4 3",\"DDRRRDRUUDLLUURRRRRLLLDDLULURRRR"]
 [\"FIM 32"]
@

-}
{- ^
 = Funções Principais
-}

movegeral :: [String] -> (Int,[(Int,Int)])
movegeral linhas = move (last (processa coords)) (head coords2) (init (reverse coords2)) (reverse tab) (coordsarrumacao  tab)
            where
                 (tab,coords) = parteMapa linhas
                 coords2 = leCoordenadas (init(processa coords))

-- ^ A função movegeral recebe uma lista de strings e processa-a de modo a que esta possa ser utilizada pela função move.


-- | A função move recebe os comandos, as coordenadas do boneco, as coordenadas das caixas, o tabuleiro e as coordenadas dos lugares de arrumação e devolve um tuplo, cujo primeiro elemento diz respeito ao número de movimentos efetuados e o segundo é uma lista com as coordenadas finais das caixas.

    -- comandos  -- boneco    --coords     -- tabuleiro -- coorde de arrum -- (nº de movimentos, coords das caixas) 
move :: String -> (Int,Int) -> [(Int,Int)] -> [String] -> [(Int,Int)] -> (Int,[(Int,Int)])
move [] (a,b) l2 l1 l3 = (0,l2)
move (h:t) (a,b) l2 l1 l3 | equiv l3 l2= (0,l2) 
                          | h == 'L' &&  moveM (a-1,b) (a-2,b) l2 l1 = ( 1 + ( fst (mov (a-1,b))), snd (mov (a-1,b)))
                          | h == 'R' &&  moveM (a+1,b) (a+2,b) l2 l1 = ( 1 + ( fst (mov (a+1,b))), snd (mov (a+1,b)))
                          | h == 'U' &&  moveM (a,b+1) (a,b+2) l2 l1 = ( 1 + ( fst (mov (a,b+1))), snd (mov (a,b+1)))
                          | h == 'D' &&  moveM (a,b-1) (a,b-2) l2 l1 = ( 1 + ( fst (mov (a,b-1))), snd (mov (a,b-1))) 
                          | otherwise = (fst (move t (a,b) l2 l1 l3), snd (move t (a,b) l2 l1 l3) )
                                         where mov p = move t p (moveCaixa (a,b) p l2) l1 l3


-- | Verifica se um determinado movimento é possível.

moveM :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [String] -> Bool
moveM a b l2 l1 | isParede a l1  = False
                | isCaixa a l2 && isParede b l1 = False
                | isCaixa a l2 && isCaixa b l2 = False
                | otherwise = True


-- | Conforme a posição anterior e a atual do boneco verifica se houve movimento de uma caixa e devolve uma lista com as coordenadas novas das caixas.

            -- inicial   -- onde está -- as coordenadas -- coordendas das caixas 
moveCaixa :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
moveCaixa (f,e) (a,b) [] = []
moveCaixa (f,e) (a,b) ((h,i):t) | (a,b) == (h,i)  && (f,e) == (h-1,i) = ((h+1,i):t)  
                                | (a,b) == (h,i)  && (f,e) == (h+1,i) = ((h-1,i):t) 
                                | (a,b) == (h,i)  && (f,e) == (h,i+1) = ((h,i-1):t)
                                | (a,b) == (h,i)  && (f,e) == (h,i-1) = ((h,i+1):t)
                                |otherwise = (h,i): moveCaixa (f,e) (a,b) (t)


-- | Recebe o tabuleiro e devolve as coordenadas dos lugares de arrumação.

coordsarrumacao :: [String] -> [(Int,Int)]
coordsarrumacao l1 = lugar 0 0  (reverse l1)
                     where lugar a b [] = []
                           lugar a b ([]:xs) = lugar 0 (b+1) xs
                           lugar a b ((c:d):xs) | c=='.' = (a,b): lugar (a+1) b ((d):xs) 
                                                | otherwise = lugar (a+1) b ((d):xs) 


-- | Função que apaga eventuais listas vazias no final do tabuleiro.

processa :: [String] -> [String]
processa [] = []
processa (x:xs) = if and (map isEspaco (x:xs)) then [] else x:processa xs
       where isEspaco [] = True             
             isEspaco (x:xs) = if x == ' ' then isEspaco xs else False -- Verifica se uma linha é vazia.


-- | Função que divide a parte das coordenadas da parte do tabuleiro.

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


-- | Verifica se duas listas de coordenadas são iguais.

equiv :: [(Int,Int)] -> [(Int,Int)] -> Bool
equiv poli1 poli2 = and (map (\(a,b) -> elem (a,b) poli1) poli2)


isCaixa :: (Int,Int) -> [(Int,Int)] -> Bool
isCaixa (a,b) l2 = or (map (\(x,y) -> (a==x) && (b==y)) l2)

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
isParede (a,b) l1 = ((l1!!b)!!a) == '#' 