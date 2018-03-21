{-|
Module      : Bomberman
Description : Módulo Haskell que agrega a maior parte do código base do jogo 'Bomberman'
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Módulo Haskell que contém o código as Tarefas 1, 2 e 4, de modo a facilitar a importação de funções usadas nestas tarefas para outros módulos.
-}
module Bomberman where

import Data.Char
import Data.List
import System.Environment
import Text.Read
import Data.Maybe
import System.Random

{-|
Usado para representar as bombas como ( 'Coordenadas' , jogador que a colocou , raio , tick )

  === Exemplo:

>>> "* 1 1 0 2 5"
((1,1),0,2,5)
-}
type Bombas = (Coordenadas,Int,Int,Float) 
-- | Usado para representar apenas o mapa.
type Mapa = [String]
{-|
Usado para representar os jogadores como (jogador , 'Coordenadas' , 'PowerUpsJ' )

  === Exemplo:

>>> "0 5 9 ++!!!"
(0,(5,9),(2,3))
-}
type Jogador = (Int,Coordenadas,PowerUpsJ)
-- | Usado para representar um par de coordenadas (x,y)
type Coordenadas = (Float,Float)
{-|
Usado para representar os powerups que um jogador tem, da forma (nº de bombs , nº de flames).

  === Exemplo:

>>> "0 5 9 ++!!!"
(2,3)
-}
type PowerUpsJ = (Int,Int)
{-|
Usado para representar os powerups como ( powerup , 'Coordenadas' )

  === Exemplo:

>>> "+ 5 3"
(+,(5,3))
-}
type PowerUps = (Char,Coordenadas)
{-|
Usado para representar o __estado de jogo__

  === Exemplo:

>>> ["#######","#     #","# # # #","#   ? #","# # # #","#  ?  #","#######","+ 3 3","* 3 2 0 3 1","* 2 5 0 3 1","0 1 1 +!!"]
(["#######","#     #","# # # #","#   ? #","# # # #","#  ?  #","#######"],[(+,(3,3))],[((3,2),0,3,1),((2,5),0,3,1)],[(0,(1,1),(1,2))])
-}
type InformacaoMapa = (Mapa,[PowerUps],[Bombas],[Jogador])


-- TAREFA 1
-- | Função que converte um mapa representado por números inteiros @'mapaComInts'@ devolvendo um mapa representado por caracteres
conversao :: [[Int]] -> [String]
conversao [] = []
conversao (l:ls) = auxconv l : conversao ls
    where auxconv :: [Int] -> String
          auxconv (h:t)| h == 101 = '#' : auxconv t
                       | h >= 40 = ' ' : auxconv t
                       | otherwise = '?' : auxconv t
          auxconv [] = []

-- | Função que devolve uma lista de strings com as posições dos power ups de bomba
bPower :: [[Int]] -> [String]
bPower l = baux l (0,0)
    where baux [] _ = []
          baux (l:ls) (x,y) = baux1 l (x,y) ++ baux ls (x,(y+1))
                where baux1 (h:t) (x,y) | h==1 || h==0 = ("+ " ++ show x ++ " " ++ show y) : baux1 t ((x+1),y)
                                        | otherwise = baux1 t ((x+1),y)
                      baux1 [] _ = []

-- | Função que devolve uma lista de strings com as posições dos power ups de flame
fPower :: [[Int]] -> [String]
fPower l = faux l (0,0)
    where faux [] _ = []
          faux (l:ls) (x,y) = faux1 l (x,y) ++ faux ls (x,(y+1))
                where faux1 (h:t) (x,y) | h==2 || h==3 = ("! " ++ show x ++ " " ++ show y) : faux1 t ((x+1),y)
                                        | otherwise = faux1 t ((x+1),y)
                      faux1 [] _ = []



-- | Função que recebe uma lista base para o mapa e uma lista de elementos aleatórios e irá incorporar os elementos aleatórios nas suas posições
aleatorio :: [Int] -> [Int] -> [Int]
aleatorio (x:xs) (y:ys) | x==1 = y: aleatorio xs ys
                        | otherwise = x: aleatorio xs (y:ys)
aleatorio l [] = l



-- | Função que irá criar um mapa com números inteiros 
mapaComInts :: Int -> Int -> [[Int]]
mapaComInts n s = divisao n (aleatorio (concat (listaBase n)) (listaAleatoria n s))
      where divisao :: Int -> [Int] -> [[Int]]
            divisao n [] = []
            divisao n l = take n l : divisao n (drop n l)
            listaAleatoria :: Int -> Int -> [Int]
            listaAleatoria n s = take (n^2-4*(n-1)-12-((div n 2)-1)^2) $ randomRs (0,99) (mkStdGen s)







-- | A função @'listaBase'@ cria um mapa geral para um dado tamanho de mapa.
listaBase :: Int -> [[Int]]
listaBase n = replicate n 101 : (aux 2 n) ++ [replicate n 101]
    where aux ::  Int -> Int -> [[Int]]
          aux k n | k==2 = (101:100:100 : (replicate (n - 6) 1) ++ [100,100,101]) : aux (k+1) n
                  | k==3 || k == (n-2) = (101:100: intersperse 1 (replicate ((div n 2)-1) 101) ++ [100,101]) : aux (k+1) n
                  | k==(n-1) = [101:100:100: replicate (n-6) 1 ++ [100,100,101]]
                  | even k = (101 : replicate (n-2) 1 ++ [101]) : aux (k+1) n
                  | otherwise = (101 : intersperse 101 (replicate (div n 2) 1) ++ [101]) : aux (k+1) n 


-- | Função que cria uma lista com os elementos aleatórios relativos ao mapa
listaAleatoria :: Int -> Int -> [Int]
listaAleatoria n s = take (n^2-4*(n-1)-12-((div n 2)-1)^2) $ randomRs (0,99) (mkStdGen s)


-- | Esta função que recebe m tamanho @n@ e uma seed @s@ e devolve um mapa com tamanho @n@
mapaaux :: Int -- ^ tamanho do mapa
        -> Int -- ^ número da semente
        -> [String] -- ^ mapa que será gerado
mapaaux n s |n==5 = ["#####","#   #","# # #","#   #","#####"]
            |otherwise = conversao (mapaComInts n s) ++ bPower (mapaComInts n s) ++ fPower (mapaComInts n s)






-- MOVIMENTO

{- |
'checkPos2' é uma __função auxiliar__ de 'checkPos'. Recebe o mapa de jogo e as coordenadas (x,y) para as quais o jogador se quer mover.

Determina que caratér se encontra nas coordenadas (x,y) e devolve-o.
-}
checkPos2 :: [String] -> (Int,Int) -> Char
checkPos2 l (x,y) = (l!!y)!!x

{- |
'conta' é uma __função auxiliar__ de 'raio' e de 'checkAllowedBombs'. Recebe a lista do jogador que colocou a bomba.

Conta o número de /power ups/ __x__ (@+@ ou @!@, depende do caso) que o jogador tem.
-}
conta :: String -> Char -> Int
conta [] x = 0
conta (h:t) x | h==x = 1+conta t x
              | otherwise = conta t x

{- |
'checkActiveBombs' é uma __função auxiliar__ de 'canPlaceBomb'. Recebe o mapa de jogo e o identificador do jogador que colocou a bomba.

Devolve o número de bombas que o jogador tem ativas (por explodir).
-}
checkActiveBombs :: [String] -> Int -> Int
checkActiveBombs [] j = 0
checkActiveBombs ((h:t):ts) j | h=='*' && take 1 ((drop 3 (words (h:t)))) == [[intToDigit j]] = 1 + checkActiveBombs ts j
                              | otherwise = checkActiveBombs ts j


-- TAREFA 4

{- |
'stringToBombs' é uma __função auxiliar__ de 'stringToEstado' que converte as bombas de um estado de jogo do tipo @[String]@ para o tipo @[Bombas]@.
-}
stringToBombs :: [String] -> [Bombas]
stringToBombs [] = []
stringToBombs ((h:t):ts) = if h=='*' then aux (words (h:t)):stringToBombs ts else stringToBombs ts
                     where aux ["*",x,y,j,r,t]=((read x :: Float,read y :: Float),read j :: Int,read r :: Int,read t :: Float)
{- |
'bombasToString' é uma __função auxiliar__ de 'estadoToString' e é a função inversa de 'stringToBombs', ou seja, 
 recebe a lista das bombas do estado de jogo do tipo @[Bombas]@ e converte-a para o tipo @[String]@
-}
bombasToString :: [Bombas] -> [String]
bombasToString [] = []
bombasToString (((x,y),a,b,c):t) = ("* "++show(round x)++" "++show(round y)++" "++show a++" "++show b++" "++show(round c)):bombasToString t

{- |
'stringToPU' é uma __função auxiliar__ de 'stringToEstado' que converte os /PowerUps/ de um estado de jogo do tipo @[String]@ para o tipo @[PowerUps]@.
-}
stringToPU :: [String] -> [PowerUps]
stringToPU [] = []
stringToPU ((h:t):ts) | h=='!' || h=='+' = [(h,(a,b))]++stringToPU ts
                 | otherwise = stringToPU ts 
                    where [a,b] = map read (words t) :: [Float]
{- |
'puToString' é uma __função auxiliar__ de 'estadoToString' e é a função inversa de 'stringToPU', ou seja, 
 recebe a lista dos /PowerUps/ do estado de jogo do tipo @[PowerUps]@ e converte-a para o tipo @[String]@
-}
puToString :: [PowerUps] -> [String]
puToString [] = []
puToString ((c,(x,y)):t) | c=='+' = ("+ "++show(round x)++" "++show(round y)):puToString t
                         | c=='!' = ("! "++show(round x)++" "++show(round y)):puToString t

{- |
'stringToJogadores' é uma __função auxiliar__ de 'stringToEstado' que converte os jogadores de um estado de jogo do tipo @[String]@ para o tipo @[Jogador]@.
-}
stringToJogadores :: [String] -> [Jogador]
stringToJogadores [] = []
stringToJogadores ((h:t):ts) | isDigit h = [(read [h],(a,b),(c,d))]++stringToJogadores ts
                      | otherwise = stringToJogadores ts
                          where [a,b] = take 2 (map read (words t) :: [Float])
                                c = 1+conta t '+'
                                d = 1+conta t '!'
{- |
'jogadoresToString' é uma __função auxiliar__ de 'estadoToString' e é a função inversa de 'stringToJogadores', ou seja, 
 recebe a lista dos jogadores do estado de jogo do tipo @[Jogador]@ e converte-a para o tipo @[String]@
-}
jogadoresToString :: [Jogador] -> [String]
jogadoresToString [] = []
jogadoresToString ((j,(x,y),(a,b)):t) = if a/=1 && b/=1 
                                          then (show j++" "++show(round x)++" "++show(round y)++" "++replicate (a-1) '+'++replicate (b-1) '!'):jogadoresToString t
                                          else (show j++" "++show(round x)++" "++show(round y)):jogadoresToString t
{- |
'stringToMap' é uma __função auxiliar__ de 'stringToEstado' que converte os jogadores de um estado de jogo do tipo @[String]@ para o tipo @Mapa@.
-}
stringToMap :: [String] -> Mapa
stringToMap l = takeWhile (\h -> head h == '#') l

-- I M P O R T A N T E S  (para o Vítor) ###################################################################################################
{- |
É a função que converte um estado de jogo do tipo @[String]@ para o tipo @InformacaoMapa@ (utlizado na Tarefa 5)
-}
stringToEstado :: [String] -> InformacaoMapa
stringToEstado l = (stringToMap l,stringToPU l,stringToBombs l,stringToJogadores l)
{- |
É a função inversa de 'stringToEstado', ou seja, converte um estado de jogo do tipo @InformacaoMapa@ para o tipo @[String]@
-}
estadoToString :: InformacaoMapa -> [String]
estadoToString (m,pu,b,j) = m++puToString pu++bombasToString b++jogadoresToString j

-- I M P O R T A N T E S  (para o Vítor) ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- instance para debug

{- |
Instance criada para debug
-}
instance Eq DataEstado where
  (Mapa a b)==(Mapa c d) = a==c && b==d
  (Bomba x y j r t)==(Bomba x1 y1 j1 r1 t1) = x==x1 && y==y1 

{- |
@data type@ criado para facilitar a manipulação do estado de jogo. Consiste em:
-}
data DataEstado = Mapa Int String -- ^ __@Mapa@ (nº de linha/coordenada y) (linha)__ - Exemplo: @Mapa 0 "#########"@
                | PowerUp Char Int Int -- ^ __@PowerUp@ (+/!) x y__ - Exemplo: @PowerUp + 1 2@
                | Bomba Int Int Int Int Int -- ^ __@Bomba@ x y jogador raio ticks__ - Exemplo: @Bomba 2 2 0 1 8@
                | Jogador Int Int Int Int Int -- ^ __@Jogador@ jogador x y (nº de powerups "+") (nº de powerups "!")__ - Exemplo: @Jogador 0 2 3 1 0@
          deriving Show

-- I M P O R T A N T E S ######################################################################################

-- acumulador: default -> 0
{- |
Converte um estado de jogo do tipo @[String]@ num estado de jogo do tipo @[DataEstado]@

Recebe o estado de jogo do tipo @[String]@ e um @Int@, que serve como acumulador e deve ser igual a __@0@__ (zero)
-}
stringToDataEstado :: [String] -> Int -> [DataEstado]
stringToDataEstado [] _ = []
stringToDataEstado (z@(h:t):ts) acumulador | h=='#' = (Mapa acumulador z):stringToDataEstado ts (acumulador+1)
                                           | h=='+' || h=='!' = (PowerUp h (getIntFromString 2 z) (getIntFromString 3 z)):stringToDataEstado ts acumulador
                                           | h=='*' = (Bomba (getIntFromString 2 z) (getIntFromString 3 z) (getIntFromString 4 z) (getIntFromString 5 z) (getIntFromString 6 z)):stringToDataEstado ts acumulador
                                           | isDigit h = (Jogador (read [h]::Int) (getIntFromString 2 z) (getIntFromString 3 z) (conta z '+') (conta z '!')):stringToDataEstado ts acumulador

{- |
Converte um estado de jogo do tipo @[DataEstado]@ num do tipo @[String]@. È a função inversa de 'stringToDataEstado'
-}
dataEstadoToString :: [DataEstado] -> [String]
dataEstadoToString [] = []
dataEstadoToString ((Mapa n l):t) = [l] ++ dataEstadoToString t
dataEstadoToString ((PowerUp c x y):t) = ([c]++" "++show x++" "++show y):dataEstadoToString t
dataEstadoToString ((Bomba x y j r ti):t) = ("* "++show x++" "++show y++" "++show j++" "++show r++" "++show ti):dataEstadoToString t
dataEstadoToString ((Jogador j x y b f):t) = if b==0 && f==0
                                             then (show j++" "++show x++" "++show y):dataEstadoToString t
                                             else (show j++" "++show x++" "++show y++" "++replicate b '+'++replicate f '!'):dataEstadoToString t

-- I M P O R T A N T E S ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- | Vai buscar o elemento __n__ de uma String e converte-o para Int (__função auxiliar__ de 'stringToDataEstado')
getIntFromString :: Int -> String -> Int
getIntFromString n l = read$head(drop (n-1) (words l))::Int

{-
-- Recebe as coordenadas (x,y) de uma bomba e o estado de jogo e devolve um par com o tick da bombas e as suas coordenadas
checkBombTick :: (Int,Int) -> [DataEstado] -> (Int,(Int,Int))
checkBombTick (x,y) ((Bomba a b j r t):ts) = if x==a && y==b then (t,(a,b)) else checkBombTick (x,y) ts
checkBombTick (x,y) (h:t) = checkBombTick (x,y) t

explodingBombCoords :: (Int,(Int,Int)) -> (Int,Int)
explodingBombCoords (t,(x,y)) = (x,y)
-}

{- |
Recebe as coordenadas de uma bomba e um @[DataEstado]@ e devolve o raio da bomba
-}
checkFlamesRadius :: (Int,Int) -> [DataEstado] -> Int
checkFlamesRadius (x,y) ((Bomba a b j r t):ts) = if x==a && y==b then r else checkFlamesRadius (x,y) ts
checkFlamesRadius (x,y) (h:t) = checkFlamesRadius (x,y) t

{-
-- | Devolve o jogador que é colocou a bomba que está em (x,y)
checkBombOwner :: (Int,Int) -> [DataEstado] -> Int
checkBombOwner (x,y) ((Bomba a b j r t):ts) = if x==a && y==b then j else checkBombOwner (x,y) ts
checkBombOwner (x,y) (h:t) = checkBombOwner (x,y) t
-}

-- | Devolve uma lista com as coordenadas das bombas que vão explodir
bombExplosion :: [DataEstado] -> [(Int,Int)]
bombExplosion [] = []
bombExplosion ((Bomba a b j r t):ts) = if t==1 then (a,b):bombExplosion ts else bombExplosion ts
bombExplosion (_:t) = bombExplosion t

-- | Verifica se há um powerup em (x,y) 
checkPosPuDT :: [DataEstado] -> (Int,Int) -> Bool
checkPosPuDT [] _ = False
checkPosPuDT ((PowerUp c n m):t) (x,y) | n==x && m==y = True
                                       | otherwise = checkPosPuDT t (x,y)
checkPosPuDT (_:t) (x,y) = checkPosPuDT t (x,y)



{- |
É uma __função auxiliar__ de 'celulasAfetadasXRFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve as coordenadas das células que essa
bomba afeta à sua direita, sem considerar as existência de powerups.
-}
celulasAfetadasXR :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
--celulasAfetadasXR =
celulasAfetadasXR ((Mapa n m):t) rt r (x,y) | y==n && m!!(x+1)=='#' && rt<=r = []
                                            | y==n && m!!(x+1)=='?' && rt<=r = [(x+1,y)]
                                            | y/=n = celulasAfetadasXR t rt r (x,y)
                                            | rt>r = []
                                            | otherwise = (x+1,y):celulasAfetadasXR ((Mapa n m):t) (rt+1) r (x+1,y)
celulasAfetadasXR (_:t) rt r (x,y) = celulasAfetadasXR t rt r (x,y)

{- |
É uma __função auxiliar__ de 'celulasAfetadasXRFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
o raio. Serve para bloquear as chamas do lado direito da bomba caso elas encontrem um powerup.
-}
celulasAfetadasPuXR :: [DataEstado] -> Int -> Int -> (Int,Int) -> Int
celulasAfetadasPuXR [] rt r (x,y) = r
celulasAfetadasPuXR l@((PowerUp c x1 y1):t) rt r (x,y) | checkPosPuDT l (x+1,y) && rt<=r = rt
                                                       | rt>r = r
                                                       | otherwise = celulasAfetadasPuXR l (rt+1) r (x+1,y)
celulasAfetadasPuXR (_:t) rt r (x,y) = celulasAfetadasPuXR t rt r (x,y) 

{- |
É uma __função auxiliar__ de 'celulasAfetadas'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
a lista das células afetadas pela bomba à sua direita, parando se encontrar um Powerup.
-}
celulasAfetadasXRFinal :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
celulasAfetadasXRFinal l rt r (x,y) = celulasAfetadasXR l rt (celulasAfetadasPuXR l rt r (x,y)) (x,y)

-- #########################################################################################################################
{- |
É uma __função auxiliar__ de 'celulasAfetadasXLFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve as coordenadas das células que essa
bomba afeta à sua esquerda sem considerar as existência de powerups.
-}
celulasAfetadasXL :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
--celulasAfetadasXL =
celulasAfetadasXL ((Mapa n m):t) rt r (x,y) | y==n && m!!(x-1)=='#' && rt<=r = []
                                            | y==n && m!!(x-1)=='?' && rt<=r = [(x-1,y)]
                                            | y/=n = celulasAfetadasXL t rt r (x,y)
                                            | rt>r = []
                                            | otherwise = (x-1,y):celulasAfetadasXL ((Mapa n m):t) (rt+1) r (x-1,y)
celulasAfetadasXL (_:t) rt r (x,y) = celulasAfetadasXL t rt r (x,y)

{- |
É uma __função auxiliar__ de 'celulasAfetadasXLFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
o raio. Serve para bloquear as chamas do lado esquerdo da bomba caso elas encontrem um powerup.
-}
celulasAfetadasPuXL :: [DataEstado] -> Int -> Int -> (Int,Int) -> Int
celulasAfetadasPuXL [] rt r (x,y) = r
celulasAfetadasPuXL l@((PowerUp c x1 y1):t) rt r (x,y) | checkPosPuDT l (x-1,y) && rt<=r = rt
                                                       | rt>r = r
                                                       | otherwise = celulasAfetadasPuXL l (rt+1) r (x-1,y)
celulasAfetadasPuXL (_:t) rt r (x,y) = celulasAfetadasPuXL t rt r (x,y) 

{- |
É uma __função auxiliar__ de 'celulasAfetadas'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
a lista das células afetadas pela bomba à sua esquerda, parando se encontrar um Powerup.
-}
celulasAfetadasXLFinal :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
celulasAfetadasXLFinal l rt r (x,y) = celulasAfetadasXL l rt (celulasAfetadasPuXL l rt r (x,y)) (x,y)

-- #########################################################################################################################
{- |
É uma __função auxiliar__ de 'celulasAfetadasYUFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve as coordenadas das células que essa
bomba afeta a cima de si, sem considerar as existência de powerups.
-}
celulasAfetadasYU :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
--celulasAfetadasYU =
celulasAfetadasYU ((Mapa n m):t) rt r (x,y) | (y-1)==n && m!!x=='#' && rt<=r = []
                                            | (y-1)==n && m!!x=='?' && rt<=r = [(x,y-1)]
                                            | (y-1)/=n = celulasAfetadasYU t rt r (x,y)
                                            | rt>r = []
                                            | otherwise = (x,y-1):celulasAfetadasYU ((Mapa n m):t) (rt+1) r (x,y-1)
celulasAfetadasYU (_:t) rt r (x,y) = celulasAfetadasYU t rt r (x,y)

{- |
É uma __função auxiliar__ de 'celulasAfetadasYUFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
o raio. Serve para bloquear as chamas do cimo da bomba caso elas encontrem um powerup.
-}
celulasAfetadasPuYU :: [DataEstado] -> Int -> Int -> (Int,Int) -> Int
celulasAfetadasPuYU [] rt r (x,y) = r
celulasAfetadasPuYU l@((PowerUp c x1 y1):t) rt r (x,y) | checkPosPuDT l (x,y-1) && rt<=r = rt
                                                       | rt>r = r
                                                       | otherwise = celulasAfetadasPuYU l (rt+1) r (x,y-1)
celulasAfetadasPuYU (_:t) rt r (x,y) = celulasAfetadasPuYU t rt r (x,y) 

{- |
É uma __função auxiliar__ de 'celulasAfetadas'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
a lista das células afetadas pela bomba acima de si, parando se encontrar um Powerup.
-}
celulasAfetadasYUFinal :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
celulasAfetadasYUFinal l rt r (x,y) = celulasAfetadasYU l rt (celulasAfetadasPuYU l rt r (x,y)) (x,y)

-- #########################################################################################################################
{- |
É uma __função auxiliar__ de 'celulasAfetadasYDFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve as coordenadas das células que essa
bomba afeta abaixo de si, sem considerar as existência de powerups.
-}
celulasAfetadasYD :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
--celulasAfetadasYD =
celulasAfetadasYD ((Mapa n m):t) rt r (x,y)   | (y+1)==n && m!!x=='#' && rt<=r = []
                                              | (y+1)==n && m!!x=='?' && rt<=r = [(x,y+1)]
                                              | (y+1)/=n = celulasAfetadasYD t rt r (x,y)
                                              | rt>r = [] 
                                              | otherwise = (x,y+1):celulasAfetadasYD ((Mapa n m):t) (rt+1) r (x,y+1)
celulasAfetadasYD (_:t) rt r (x,y) = []

{- |
É uma __função auxiliar__ de 'celulasAfetadasYDFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
o raio. Serve para bloquear as chamas abaixo da bomba caso elas encontrem um powerup.
-}
celulasAfetadasPuYD :: [DataEstado] -> Int -> Int -> (Int,Int) -> Int
celulasAfetadasPuYD [] rt r (x,y) = r
celulasAfetadasPuYD l@((PowerUp c x1 y1):t) rt r (x,y) | checkPosPuDT l (x,y+1) && rt<=r = rt
                                                       | rt>r = r
                                                       | otherwise = celulasAfetadasPuYD l (rt+1) r (x,y+1)
celulasAfetadasPuYD (_:t) rt r (x,y) = celulasAfetadasPuYD t rt r (x,y) 

{- |
É uma __função auxiliar__ de 'celulasAfetadas'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
a lista das células afetadas pela bomba abaixo de si, parando se encontrar um Powerup.
-}
celulasAfetadasYDFinal :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
celulasAfetadasYDFinal l rt r (x,y) = celulasAfetadasYD l rt (celulasAfetadasPuYD l rt r (x,y)) (x,y)
-- gera a lista das céluas afetadas pela bomba colocada em (x,y); rt: default -> 1


{- |
É uma __função auxiliar__ de 'celulasAfetadasFinal'. 

Recebe o estado de jogo, um Int (rt) que deve ser @1@, um Int (r) que é o raio da bomba e as coordenadas da bomba e devolve 
a lista das células afetadas pela bomba.
-}
celulasAfetadas :: [DataEstado] -> Int -> Int -> (Int,Int) -> [(Int,Int)]
celulasAfetadas l rt r (x,y) = [(x,y)] ++ (celulasAfetadasXRFinal l rt r (x,y)) ++ (celulasAfetadasXLFinal l rt r (x,y)) ++ (celulasAfetadasYUFinal (reverse l) rt r (x,y)) ++ (celulasAfetadasYDFinal l rt r (x,y))

{- |
Recebe o estado de jogo e o resultado de 'bombExplosion' e devolve a lista com as células que são afetadas pela explosão de todas as bombas do 
mapa com um tick de 1. 
-}
celulasAfetadasFinal :: [DataEstado] -> [(Int,Int)] -> [(Int,Int)]
celulasAfetadasFinal d [] = []
celulasAfetadasFinal d (h:t) = (celulasAfetadas d 1 (checkFlamesRadius h d) h)++(celulasAfetadasFinal d t)

{- |
Recebe o estado de jogo e o resultado de 'celulasAfetadasFinal' e elimina do estado de jogo os jogadores que sofreram a explosão de uma bomba, ou seja, 
que se encontravam nas células afetadas pela explosão de uma bomba.
-}
eliminaJogador :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
eliminaJogador [] _ = []
eliminaJogador ((Jogador j x1 y1 b f):ts) l | (x1,y1)`elem`l = eliminaJogador ts l
                                            | otherwise = (Jogador j x1 y1 b f):(eliminaJogador ts l)
eliminaJogador (h:t) l = h:(eliminaJogador t l)


-- | Verifica se um powerup (nas coordenadas (x,y)) está destapado ou não.
ePowerUpDestapado :: [DataEstado] -> (Int,Int) -> Bool
ePowerUpDestapado [] (x,y) = False
ePowerUpDestapado a@((Mapa n m):t) (x,y) | (checkPosPuDT a (x,y)) && n==y && m!!x=='?'=False
                                         | (checkPosPuDT a (x,y)) && n==y && m!!x==' '=True
                                         | otherwise = ePowerUpDestapado t (x,y)
ePowerUpDestapado (_:t) (x,y) = ePowerUpDestapado t (x,y)

{- |
Recebe o estado de jogo (duas vezes) e o resultado de 'celulasAfetadasFinal' e elimina do estado de jogo os powerups destapados 
 que sofreram a explosão de uma bomba, ou seja, se o powerup está despado e as suas coordenadas estão na lista resultado de 'celulasAfetadasFinal', ele
 é eliminda do estado de jogo.
-}
eliminaPowerUpDestapado :: [DataEstado] -> [DataEstado] -> [(Int,Int)] -> [DataEstado]
eliminaPowerUpDestapado dt [] _ = []
eliminaPowerUpDestapado dt a@((PowerUp c x1 y1):ts) l | (ePowerUpDestapado dt (x1,y1)) && ((x1,y1)`elem`l) = eliminaPowerUpDestapado dt ts l
                                                      | otherwise = (PowerUp c x1 y1):(eliminaPowerUpDestapado dt ts l)
eliminaPowerUpDestapado dt (h:t) l = h:(eliminaPowerUpDestapado dt t l)

{-
--função para debug
eliminaPowerUpDebug :: [DataEstado] -> Int -> Int -> (Int,Int) -> [DataEstado]
eliminaPowerUpDebug a rt r (x,y) = eliminaPowerUpDestapado a a (celulasAfetadas a rt r (x,y))
-}
{-
-- descobre os powerups que estão na lista das celuas afetadas mas que não estão destapados
powerupNaoDestapado :: [DataEstado] -> [DataEstado] -> [(Int,Int)] -> [(Int,Int)]
powerupNaoDestapado _ [] _ = []
powerupNaoDestapado a ((PowerUp c x y):ts) l | (ePowerUpDestapado a (x,y))==False && (x,y)`elem`l = (x,y):powerupNaoDestapado a ts l
                                             | otherwise = powerupNaoDestapado a ts l
powerupNaoDestapado a ((Mapa n m):ts) l = powerupNaoDestapado a ts l
powerupNaoDestapado a ((Jogador j x y b f):ts) l = powerupNaoDestapado a ts l
powerupNaoDestapado a ((Bomba x y j r t):ts) l = powerupNaoDestapado a ts l

-- recebe o DataEstado e a lista gerada pela função powerupNaoDestapado e devolve uma lista com as linhas a serem alteradas
destapaPowerupsAux :: [DataEstado] -> [DataEstado] -> [(Int,Int)] -> [DataEstado]
destapaPowerupsAux _ _ [] = []
destapaPowerupsAux a [] ((x,y):ts) = destapaPowerupsAux a a ts
destapaPowerupsAux a ((Mapa n m):t) ((x,y):ts) | n==y = let (b,c)=splitAt x m
                                                        in (Mapa n (b++" "++(tail c))):destapaPowerupsAux a t ts
                                               | otherwise = destapaPowerupsAux a t ((x,y):ts)
destapaPowerupsAux a ((Jogador j x y b f):ts) l = destapaPowerupsAux a ts l
destapaPowerupsAux a ((Bomba x y j r t):ts) l = destapaPowerupsAux a ts l
destapaPowerupsAux a ((PowerUp c x y):ts) l = destapaPowerupsAux a ts l

--substitui as linhas modificadas (dadas pela função destapaPowerupsAux) no estado do jogo
destapaPowerups :: [DataEstado] -> [DataEstado] -> [DataEstado] -> [DataEstado]
destapaPowerups a [] [] = []
destapaPowerups a b@((Mapa n m):t) [] = b
destapaPowerups a [] b@((Mapa n1 m1):ts) = destapaPowerups a a b
destapaPowerups a ((Mapa n m):t) ((Mapa n1 m1):ts) | n==n1 = (Mapa n1 m1):destapaPowerups a t ts
                                                   | otherwise = (Mapa n m):destapaPowerups a t ((Mapa n1 m1):ts)
destapaPowerups a ((Jogador j x y b f):ts) l = (Jogador j x y b f):destapaPowerups a ts l
destapaPowerups a ((Bomba x y j r t):ts) l = (Bomba x y j r t):destapaPowerups a ts l
destapaPowerups a ((PowerUp c x y):ts) l = (PowerUp c x y):destapaPowerups a ts l
-}

{- |
Recebe o estado de jogo e as coordenadas (x,y) e, se houver um tijolo ("?") nessas coordenadas, destrói-o.
-}
destroiTijolo :: [DataEstado] -> (Int,Int) -> [DataEstado]
destroiTijolo [] _ = []
destroiTijolo ((Mapa n m):t) (x,y) | y==n && m!!x=='?' = let (a,b)=splitAt x m in (Mapa n (a++" "++(tail b))):destroiTijolo t (x,y)
                                   | otherwise = (Mapa n m):destroiTijolo t (x,y)
destroiTijolo (h:t) (x,y) = h:destroiTijolo t (x,y)
{- |
Recebe o estado de jogo e o resultado de 'celulasAfetadasFinal' e elimina do estado de jogo os tijolos ("?") que sofreram a explosão de uma bomba.
-}
destroiTijoloFinal :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
destroiTijoloFinal d [] = d
destroiTijoloFinal d [(x,y)] = destroiTijolo d (x,y)
destroiTijoloFinal d (h:t) = destroiTijoloFinal (destroiTijolo d h) t
{- |
Recebe o estado de jogo e o resultado de 'celulasAfetadasFinal' e elimina do resultado de 'celulasAfetadasFinal' as coordenadas dos tijolos que vão ser destruídos.
-}
celulasAfetadasSemTijolosDestruidos :: [DataEstado] -> [(Int,Int)] -> [(Int,Int)]
celulasAfetadasSemTijolosDestruidos d [] = []
celulasAfetadasSemTijolosDestruidos d (h:t) | eTijoloDestruido d (h:t) h = celulasAfetadasSemTijolosDestruidos d t
                                            | otherwise = h:celulasAfetadasSemTijolosDestruidos d t
{- |
__Função auxiliar__ de 'celulasAfetadasSemTijolosDestruidos'.

Recebe o estado de jogo, o resultado de 'celulasAfetadasFinal' e um par de coordenadas e determina se esse par de coordenadas é um tijolo que vai ser
 destruído.
-}
eTijoloDestruido :: [DataEstado] -> [(Int,Int)] -> (Int,Int) -> Bool
eTijoloDestruido [] _ _ = False
eTijoloDestruido ((Mapa n m):t) l (x,y) | (x,y)`elem`l && y==n && m!!x=='?' = True
                                        | otherwise = eTijoloDestruido t l (x,y)
eTijoloDestruido (_:t) l (x,y) = eTijoloDestruido t l (x,y)


{- |
È uma __função auxiliar__ de 'mudaTickBombaFinal'.

Recebe o estado de jogo (duas vezes) e o resultado de 'celulasAfetadas' e as coordenadas da bomba que explodiu e muda o tick das bombas
que sofreram uma explosão para 2, devolvendo uma lista apenas com o DataEstado das bombas cujo tick foi alterado.
-}
mudaTickBombaAux :: [DataEstado] -> [DataEstado] -> [(Int,Int)] -> (Int,Int) -> [DataEstado]
mudaTickBombaAux a [] [] _ = []
mudaTickBombaAux a b@((Bomba x y j r t):ts) [] _ = []
mudaTickBombaAux a [] ((x1,y1):t1) c = mudaTickBombaAux a a t1 c
mudaTickBombaAux a ((Bomba x y j r t):ts) ((x1,y1):t1) (x2,y2) | x==x2 && y==y2 = mudaTickBombaAux a ts ((x1,y1):t1) (x2,y2)
                                                               | x==x1 && y==y1 = (Bomba x y j r 2):mudaTickBombaAux a a t1 (x2,y2)
                                                               | otherwise = mudaTickBombaAux a ts ((x1,y1):t1) (x2,y2)
mudaTickBombaAux a ((Jogador j x y b f):ts) l c = mudaTickBombaAux a ts l c
mudaTickBombaAux a ((Mapa n m):ts) l c = mudaTickBombaAux a ts l c
mudaTickBombaAux a ((PowerUp c x y):ts) l coords = mudaTickBombaAux a ts l coords



-- | È uma __função auxiliar__ de 'mudaTickBombaFinal'. Recebe o estado de jogo e devolve uma lista só com as bombas
retiraBombas :: [DataEstado] -> [DataEstado]
retiraBombas [] = []
retiraBombas ((Mapa n m):t) = retiraBombas t
retiraBombas ((PowerUp c x y):t) = retiraBombas t
retiraBombas ((Bomba x y j r t):t1) = (Bomba x y j r t):retiraBombas t1
retiraBombas ((Jogador j x y b f):t) = retiraBombas t

{- |
È uma __função auxiliar__ de 'mudaTickBombaFinal'.

Recebe a lista resultante de 'retiraBombas' e a lista resultante de mudaTickBombaAux (duas vezes) e, substitui as bombas da lista resultante de 
'retiraBombas' pelas bombas resultantes de 'mudaTickBombaAux', caso estas tenham coordenadas iguais.
-}
mudaTickBomba :: [DataEstado] -> [DataEstado] -> [DataEstado] -> [DataEstado]
mudaTickBomba [] _ _ = []
mudaTickBomba (a@(Bomba x y j r t):t1) [] c = a:mudaTickBomba t1 c c
mudaTickBomba (a@(Bomba x y j r t):ts) (b@(Bomba x1 y1 j1 r1 t1):ts1) c | x==x1 && y==y1 = b:mudaTickBomba ts ts1 c
                                                                        | otherwise = mudaTickBomba (a:ts) ts1 c

{- |
È uma __função auxiliar__ de 'mudaTickBombaFinal'.

Recebe o estado do jogo e a lista resultante de 'mudaTickBomba' e substitui todas as bombas do estado de jogo pelas que estão na lista resultante de
 'mudaTickBomba', devolvendo o novo estado de jogo. 
-}
juntaNovasBombas :: [DataEstado] -> [DataEstado] -> [DataEstado]
juntaNovasBombas [] _ = []
juntaNovasBombas (a@(Mapa n m):t) b = a:juntaNovasBombas t b
juntaNovasBombas (a@(PowerUp c1 x y):t) b = a:juntaNovasBombas t b
juntaNovasBombas (a@(Bomba x y j r t):ts) (b@(Bomba x1 y1 j1 r1 t1):tss) = b:juntaNovasBombas ts tss
juntaNovasBombas (a@(Jogador j x y b1 f):t) b = a:juntaNovasBombas t b 


-- | __Função auxiliar__ de 'removeBombaExplodiuFinal'. Recebe o estado de jogo e elimina a bomba que está nas coordenadas (x1,y1).
removeBombaExplodiu :: [DataEstado] -> (Int,Int) -> [DataEstado]
removeBombaExplodiu [] _ = []
removeBombaExplodiu (a@(Mapa n m):t) b = a:removeBombaExplodiu t b
removeBombaExplodiu (a@(PowerUp c1 x y):t) b= a:removeBombaExplodiu t b
removeBombaExplodiu (a@(Bomba x y j r t):ts) (x1,y1) | x==x1 && y==y1 = removeBombaExplodiu ts (x1,y1)
                                                     | otherwise = a:removeBombaExplodiu ts (x1,y1)
removeBombaExplodiu (a@(Jogador j x y b1 f):t) b = a:removeBombaExplodiu t b 

{- |
Recebe o estado do jogo e a lista resultante de 'bombExplosion' e elimina as bombas cujas coordenadas estão nessa lista.
-}
removeBombaExplodiuFinal :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
removeBombaExplodiuFinal d [] = d
removeBombaExplodiuFinal d [(x,y)] = removeBombaExplodiu d (x,y)
removeBombaExplodiuFinal d (h:t) = removeBombaExplodiuFinal (removeBombaExplodiu d h) t

-- | Recebe o resultado de 'celulasAfetadas' e o resultado de 'bombExplosion' e devolve o resultado de primeiro argumento sem os elementos do segundo.
celulasAfetadasSemBombExplosion :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
celulasAfetadasSemBombExplosion [] _ = []
celulasAfetadasSemBombExplosion (h:t) be | h`elem`be = celulasAfetadasSemBombExplosion t be
                                              | otherwise = h:celulasAfetadasSemBombExplosion t be

{- |
__Função auxiliar__ de 'mudaTickBombaFinalFinal'.

Recebe o estado do jogo e as coordenadas da bomba que explodiu e, se alguma bomba tiver sido apanhada na explosão, muda o tick dessa bomba para 2.
-}
mudaTickBombaFinal :: [DataEstado] -> (Int,Int) -> [DataEstado]
mudaTickBombaFinal a (x,y) = juntaNovasBombas a (mudaTickBomba (retiraBombas a) (mudaTickBombaAux a a (celulasAfetadasSemBombExplosion (celulasAfetadas a 1 (checkFlamesRadius (x,y) a) (x,y)) (bombExplosion a)) (x,y)) (mudaTickBombaAux a a (celulasAfetadasSemBombExplosion (celulasAfetadas a 1 (checkFlamesRadius (x,y) a) (x,y)) (bombExplosion a)) (x,y)))

{- |
Recebe o estado do jogo e a lista resultado de 'bombExplosion' e, verifica se quando uma bomba explode, se afeta outra e, se afetar, muda o tick da
 bomba afetada para 2.
-}
mudaTickBombaFinalFinal :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
mudaTickBombaFinalFinal d [] = d
mudaTickBombaFinalFinal d (h:t) = if eBomba d h
                                  then mudaTickBombaFinalFinal (mudaTickBombaFinal d h) t
                                  else mudaTickBombaFinalFinal d t


{-
mudaTickBombaFinalFinalFinal :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
mudaTickBombaFinalFinalFinal d l = if bombExplosion (mudaTickBombaFinalFinal d l) == []
                                   then mudaTickBombaFinalFinal d l
                                   else mudaTickBombaFinalFinal (mudaTickBombaFinalFinal d l) l

removeBomba0Ticks :: [DataEstado] -> [DataEstado]
removeBomba0Ticks [] = []
removeBomba0Ticks (a@(Bomba x y j r t):ts) | t==0 = removeBomba0Ticks ts
                                           | otherwise = a:removeBomba0Ticks ts
removeBomba0Ticks (h:t) = h:removeBomba0Ticks t
-}
-- | Recebe o estado de jogo e um par de coordenadas e verifica se nesse par de coordenadas há uma bomba.
eBomba :: [DataEstado] -> (Int,Int) -> Bool
eBomba [] _ = False
eBomba ((Bomba x1 y1 j r t):ts) (x,y) | x==x1 && y==y1 = True
                                      | otherwise = eBomba ts (x,y)
eBomba (_:t) (x,y) = eBomba t (x,y)

-- ############################################ C A R A C O L ######################################################################

{-
-- | Recebe o nº de ticks que faltam para o jogo acabar e a dimensão do mapa e devolve o numero de celulas que tem de estar bloqueado
celulasBloqueadas :: Int -> Int -> Int
celulasBloqueadas t n = if t >= (n-2)^2
                        then 0
                        else (n-2)^2 - t
-}

-- | Recebe o estado de jogo e devolve a dimensão do mapa
dimensaoMapa :: [DataEstado] -> Int
dimensaoMapa ((Mapa n m):t) = length m
dimensaoMapa (h:t) = dimensaoMapa t


{-|
Função que trabalha em conjunto com 'geraCelulasABloquearD', 'geraCelulasABloquearR' e 'geraCelulasABloquearU' de modo a
 gerarem uma lista de coordenadas das células que devem estar bloqueadas.

Recebe a dimensão do mapa, um @i@ (um acumulador, deve ser @1@), as coordenadas a partir das quais o efeito "caracol" começa e um @s@ que informa a funação de quando
 o efeito "caracol" tem de acabar (deve ser @((dimensão do mapa)`div`2)@).
-}
geraCelulasABloquearL :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
-- geraCelulasABloquearL d [] = geraCelulasABloquearL d [(1,1)]
geraCelulasABloquearL d i (x,y) s | x==y && x==s = []
                                  | x<d-2 = (x+1,y):geraCelulasABloquearL d i (x+1,y) s
                                  | x==d-2 = (x,y+1):geraCelulasABloquearD d i (x,y+1) s -- >>bloqueia neste fazer uma função para cada

{-|
Função que trabalha em conjunto com 'geraCelulasABloquearL', 'geraCelulasABloquearR' e 'geraCelulasABloquearU' de modo a
 gerarem uma lista de coordenadas das células que devem estar bloqueadas.

Recebe a dimensão do mapa, um @i@ (um acumulador, deve ser @1@), as coordenadas a partir das quais o efeito "caracol" começa e um @s@ que informa a funação de quando
 o efeito "caracol" tem de acabar (deve ser @((dimensão do mapa)`div`2)@).
-}
geraCelulasABloquearD :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
-- geraCelulasABloquearD d [] = geraCelulasABloquearD d [(1,1)]
geraCelulasABloquearD d i (x,y) s | x==y && x==s = []
                                  | y<d-2 = (x,y+1):geraCelulasABloquearD d i (x,y+1) s
                                  | y==d-2 = (x-1,y):geraCelulasABloquearR d i (x-1,y) s


{-|
Função que trabalha em conjunto com 'geraCelulasABloquearL', 'geraCelulasABloquearD' e 'geraCelulasABloquearU' de modo a
 gerarem uma lista de coordenadas das células que devem estar bloqueadas.

Recebe a dimensão do mapa, um @i@ (um acumulador, deve ser @1@), as coordenadas a partir das quais o efeito "caracol" começa e um @s@ que informa a funação de quando
 o efeito "caracol" tem de acabar (deve ser @((dimensão do mapa)`div`2)@).
-}
geraCelulasABloquearR :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
-- geraCelulasABloquearR d [] = geraCelulasABloquearR d [(1,1)]
geraCelulasABloquearR d i (x,y) s | x==y && x==s = []
                                  | x>i = (x-1,y):geraCelulasABloquearR d i (x-1,y) s
                                  | x==i = (x,y-1):geraCelulasABloquearU d (i+1) (x,y-1) s

{-|
Função que trabalha em conjunto com 'geraCelulasABloquearL', 'geraCelulasABloquearD' e 'geraCelulasABloquearR' de modo a
 gerarem uma lista de coordenadas das células que devem estar bloqueadas.

Recebe a dimensão do mapa, um @i@ (um acumulador, deve ser @1@), as coordenadas a partir das quais o efeito "caracol" começa e um @s@ que informa a funação de quando
 o efeito "caracol" tem de acabar (deve ser @((dimensão do mapa)`div`2)@).
-}
geraCelulasABloquearU :: Int -> Int -> (Int,Int) -> Int -> [(Int,Int)]
-- geraCelulasABloquearU d [] = geraCelulasABloquearU d [(1,1)]
geraCelulasABloquearU d i (x,y) s | x==y && x==s = []
                                  | y>i = (x,y-1):geraCelulasABloquearU d i (x,y-1) s
                                  | y==i = (x+1,y):geraCelulasABloquearL (d-1) i (x+1,y) s

-- | Função que recebe a dimensão do mapa e gera uma lista com as células que podem ser bloqueadas pela ordem de bloqueio correta a partir da função 'geraCelulasABloquearL'
geraCelulasABloquear :: Int -> [(Int,Int)]
geraCelulasABloquear d = (1,1):geraCelulasABloquearL d 1 (1,1) (d`div`2)


{-|
Recebe a dimensão do mapa, o número de ticks que falta para o jogo acabar e a lista resultante de 'geraCelulasABloquear' 
e devolve uma lista com as células que têm de ser bloqueadas de acordo com o nº de ticks que falta para o jogo acabar.
-}
tiraCelulaABloquear :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
tiraCelulaABloquear d t l = if t==(d-2)^2 then [] else (l!!((d-2)^2-t-1)):tiraCelulaABloquear d (t+1) l

-- | Recebe a dimensão do mapa e o nº de ticks que falta para o jogo terminar e inverte a lista resultante de 'tiraCelulaABloquear'
celulasABloquear :: Int -> Int -> [(Int,Int)]
celulasABloquear d t = reverse$tiraCelulaABloquear d t (geraCelulasABloquear d)

-- | Recebe o estado de jogo e as coordenadas da célula a bloquear e bloqueia-a (ou seja, substitui o que estiver nessas coordenadas por um "#").
bloqueiaCelula :: [DataEstado] -> (Int,Int) -> [DataEstado]
bloqueiaCelula [] _ = []
bloqueiaCelula ((Mapa n m):t) (x,y) | n==y = let (a,b)=splitAt x m in (Mapa n (a++"#"++(tail b))):t
                                    | otherwise = (Mapa n m):bloqueiaCelula t (x,y)
bloqueiaCelula (a@(PowerUp c x1 y1):t) (x,y) = a:bloqueiaCelula t (x,y)
bloqueiaCelula (a@(Bomba x1 y1 j r t1):t) (x,y) = a:bloqueiaCelula t (x,y)
bloqueiaCelula (a@(Jogador j x1 y1 b f):t) (x,y) = a:bloqueiaCelula t (x,y)

-- | Recebe o estado de jogo e a lista resultante de 'celulasABloquear' e devolve o estado de jogo com as células que estão na lista bloqueadas.
bloqueiaCelulaFinal :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
bloqueiaCelulaFinal d [] = d
bloqueiaCelulaFinal d (h:t) = bloqueiaCelulaFinal (bloqueiaCelula d h) t

-- | Recebe o estado de jogo e a lista resultante de 'celulasABloquear' e devolve o estado de jogo sem as bombas cujas coordenadas estavam na lista.
eliminaBomba :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
eliminaBomba [] _ = []
eliminaBomba ((Bomba x y j r t):ts) l | (x,y)`elem`l = eliminaBomba ts l
                                      | otherwise = (Bomba x y j r t):(eliminaBomba ts l)
eliminaBomba (h:t) l = h:(eliminaBomba t l)

-- | Recebe o estado de jogo e a lista resultante de 'celulasABloquear' e devolve o estado de jogo sem os powerups cujas coordenadas estavam na lista.
eliminaPowerUp :: [DataEstado] -> [(Int,Int)] -> [DataEstado]
eliminaPowerUp [] _ = []
eliminaPowerUp ((PowerUp c x y):ts) l | (x,y)`elem`l = eliminaPowerUp ts l
                                      | otherwise = (PowerUp c x y):(eliminaPowerUp ts l)
eliminaPowerUp (h:t) l = h:(eliminaPowerUp t l)

-- | Gere o que acontece com os powerups e os tijolos quando explode uma bomba.
explosaoBombasPowerups :: [DataEstado] -> [DataEstado]
-- explosaoBombasPowerups d = destroiTijoloFinal (eliminaPowerUpDestapado d d (celulasAfetadasFinal d (bombExplosion d))) (celulasAfetadasFinal (eliminaPowerUpDestapado d d (celulasAfetadasFinal d (bombExplosion d))) (bombExplosion d))
explosaoBombasPowerups d = eliminaPowerUpDestapado (destroiTijoloFinal d (celulasAfetadasFinal d (bombExplosion d))) (destroiTijoloFinal d (celulasAfetadasFinal d (bombExplosion d))) (celulasAfetadasSemTijolosDestruidos d (celulasAfetadasFinal d (bombExplosion d)))

-- | Recebe o estado de jogo e reduz o tick de todas as bombas em 1.
diminuiTickBomba :: [DataEstado] -> [DataEstado]
diminuiTickBomba [] = []
diminuiTickBomba ((Bomba x y j r t):ts) = (Bomba x y j r (t-1)):diminuiTickBomba ts
diminuiTickBomba (h:t) = h:diminuiTickBomba t
{-
-- | __Função final__ que recebe o estado do jogo e o nº de ticks que falta para o jogo acabar e gere a explosão das bombas, a diminuição do tick das bombas e o "efeito caracol".
avancaAux :: [DataEstado] -> Int -> [DataEstado]
avancaAux d t = if t>=((dimensaoMapa d)-2)^2
                then diminuiTickBomba$removeBombaExplodiuFinal (eliminaJogador (mudaTickBombaFinalFinal (explosaoBombasPowerups d) (celulasAfetadasFinal d (bombExplosion d))) (celulasAfetadasFinal d (bombExplosion d))) (bombExplosion d)
                else diminuiTickBomba$removeBombaExplodiuFinal (eliminaJogador (mudaTickBombaFinalFinal (explosaoBombasPowerups (eliminaPowerUp (eliminaBomba (eliminaJogador (bloqueiaCelulaFinal d (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t))) (celulasAfetadasFinal d (bombExplosion d))) (celulasAfetadasFinal d (bombExplosion d))) (bombExplosion d)
-}
-- | __Função final__ que recebe o estado do jogo e o nº de ticks que falta para o jogo acabar e gere a explosão das bombas, a diminuição do tick das bombas e o "efeito caracol".
avancaAux :: [DataEstado] -> Int -> [DataEstado]
avancaAux d t = if t>=((dimensaoMapa d)-2)^2
                then diminuiTickBomba$removeBombaExplodiuFinal (eliminaJogador (explosaoBombasPowerups (mudaTickBombaFinalFinal d (celulasAfetadasFinal d (bombExplosion d)))) (celulasAfetadasFinal d (bombExplosion d))) (bombExplosion d)
                else diminuiTickBomba$removeBombaExplodiuFinal (eliminaJogador (explosaoBombasPowerups (mudaTickBombaFinalFinal (eliminaPowerUp (eliminaBomba (eliminaJogador (bloqueiaCelulaFinal d (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t)) (celulasABloquear (dimensaoMapa d) t)) (celulasAfetadasFinal d (bombExplosion d)))) (celulasAfetadasFinal d (bombExplosion d))) (bombExplosion d)

{-
-- os dois dataEstado são iguais
avancaAux2 :: [DataEstado] -> [DataEstado] -> Int -> [DataEstado]
avancaAux2 d [] t = diminuiTickBomba (avancaAux d t)
avancaAux2 d (a@(Bomba x y j r t1):ts) t | t1==1 = avancaAux d t
                                         | otherwise = avancaAux2 d ts t
avancaAux2 d (h:ts) t = avancaAux2 d ts t

avancaAux3 :: [DataEstado] -> Int -> [DataEstado]
avancaAux3 d t = avancaAux2 (avancaAux d t) (avancaAux d t) t
-}