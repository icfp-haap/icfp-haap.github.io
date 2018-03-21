{-|
Module : Main
Description : Reagir à passagem do tempo
Copyright : Pedro Pinto <a80741@alunos.uminho.pt>;
            Pedro Lima <a80328@alunos.uminho.pt>
Neste módulo procura-se que o jogo em criação passe a ser capaz de alterar o seu estado com base num acontecimento. 
O acontecimento nesta tarefa será especificamente a passagem do tempo onde o jogo terá de se adaptar com base nos eventos que ocorrem no mapa:
Exemplos disto serão os timers das bombas, que com a passagem do tempo serão reduzidos assim como a alteração física do mapa a partir do instante (d-2)² onde d representa a dimensão do mesmo e instante onde é invocada uma espiral que vai fechando o mapa.

-}


module Tarefa4 where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

{- |
ex14 
* 7 6 1 1 1
* 7 7 0 1 10
0 7 5 
1 7 5
-}

type Mapa      = [String]                                                 -- ^ Representação do estad do jogo em lista de strings.
type Linha     = String                                                   -- ^ Uma linha da representação doe stado do jogo no tipo @Mapa@
type PowerType = Char                                                     -- ^ @Char@ distintivo de um tipo de powerup.
type Range     = Int                                                      -- ^ Raio de uma bomba.
type Abcissa   = Int                                                      -- ^ Abcissa de um espaço do mapa.
type Ordenada  = Int                                                      -- ^ Ordenada de um espaço do mapa.
type Tempo     = Int                                                      -- ^ Instantes de tempo que restam para o jogo acabar.
type Cords     = (Abcissa,Ordenada)                                       -- ^ Coordenadas de um espaço no mapa.
type Dim       = Int                                                       -- ^ Dimensão do mapa.
type Jogador   = Int                                                       -- ^ Número que representa um dado jogador.


ln x = mapM_ putStrLn x

avanca :: Mapa -> Tempo -> Mapa
avanca mapa n = if n > time then          mergeMaps (explosionsMap (reduce mapa) (reduce mapa)) []
                            else espiral (mergeMaps (explosionsMap (reduce mapa) (reduce mapa)) []) n
              where time = (d-2)^2
                    d = length (mapa!!0)


mapa1 =  ["#########",
          "##      #",
          "# #?# # #",
          "#     ? #",
          "#?# # #?#",
          "# ?  ?  #",
          "# #?#?# #",
          "#  ??   #",
          "#########",
          "+ 3 3",
          "! 5 5",
          "* 7 7 1 1 1",
          "0 4 3 +",
          "1 7 7"]

  --      ,-0      
  --    __+__
  --   /|_|_|_\
  --  |_|_|_|_|
  --  |_|_|_|_|
  --   \_|_|_/




main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"





-----------------------------------------DIMINUIÇÃO DOS TIMERS E EXPLOSÃO DE BOMBAS-----------------------------------------------
{- |
Exemplo:
reduce ex14
* 7 6 1 1 1                        * 7 6 1 1 0
* 7 7 0 1 10                       * 7 7 0 1 9
0 7 5                              0 7 5
1 7 5                              1 7 5

-}
reduce :: Mapa -> Mapa
-- ^ Função que percorre o mapa e, a cada bomba, reduz o tempo do contador em 1.
reduce []    = []
reduce (h:t) = if     (h!!0) == '*'        then (tickBomb h):reduce t
                                           else           h :reduce t




tickBomb :: Linha -> Linha
-- ^ Função que reduz o contador na string da bomba.
tickBomb h = if last h == '0' then (take (length h-2) h) ++ "9"
                              else (take (length h-1) h) ++ (show (read (drop (length h-1) h)-1))





explosionsMap :: Mapa -> Mapa -> [Mapa]
-- ^ Função que percorre o mapa para encontrar bombas que têm de explodir, levando outro argumento com o mapa, que vai sendo alterado à medida que vão havendo explosões.
explosionsMap mapa []    =                                                                              [mapa]
explosionsMap mapa (h:t) = if   (h!!0) == '*' && last h == '0'       then (explode mapa h):explosionsMap mapa t
                                                                     else                  explosionsMap mapa t


mergeMaps :: [Mapa] -> Mapa -> Mapa
-- ^ Função que funde mapas resultantes de explosões que ocorreram em simultâneo.
mergeMaps []  m                                                                        = m
mergeMaps [a] _                                                                        = a
mergeMaps ([]:[]:t) m                                                                  =                                       mergeMaps     (m:t) []
mergeMaps (a:[]:t)  m                                                                  =                                       mergeMaps     (m:t) []
mergeMaps ([]:b:t)  m                                                                  =                                       mergeMaps     (m:t) []
mergeMaps ((a:as):(b:bs):t) m | a == b                                                 =                                       mergeMaps (as:bs:t)     (m ++ [a])
                              | head a == '#'                                          = if howMany ' ' a > howMany ' ' b then mergeMaps (as:bs:t)     (m ++ [a]) else mergeMaps (as:bs:t) (m ++ [b])
                              | head a == '*' && head b == '*' && cordsT a == cordsT b = if last a == '1'                 then mergeMaps (as:bs:t)     (m ++ [a]) else mergeMaps (as:bs:t) (m ++ [b])
                              | head a == '*' && last a == '1'                         =                                       mergeMaps (as:(b:bs):t) (m ++ [a])
                              | head b == '*' && last b == '1'                         =                                       mergeMaps ((a:as):bs:t) (m ++ [b])
                              | elem a (b:bs)                                          =                                       mergeMaps (as:bs:t)     (m ++ [a])
                              | elem b (a:as)                                          =                                       mergeMaps (as:bs:t)     (m ++ [b])
                              | otherwise                                              =                                       mergeMaps (as:bs:t)      m







howMany :: PowerType -> Linha -> Int
howMany _ []    = 0
howMany c (h:t) = if h == c then 1 + howMany c t else howMany c t



{- |
Função que, a partir do estado do mapa e de uma string de uma bomba prestes a explodir, gera o mapa depois da explosão.

reduce ex14                        explode ex14        
* 7 6 1 1 1                        * 7 6 1 1 0                * 7 7 0 1 9
* 7 7 0 1 10                       * 7 7 0 1 9                0 7 5
0 7 5                              0 7 5                      1 7 5
1 7 5                              1 7 5




-}
explode :: Mapa -> Linha -> Mapa
explode mapa string = removeBomb (flameUp   (flameDown  (flameRight   (    flameLeft mapa    x y r  )   x y r)     x y r)    x y r) x y
                   where x = pAbcis string
                         y = pOrden string
                         r = range  string







flameUp :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
-- ^ Função que explode os espaços do mapa acima da bomba e chama as funções para efetuar as alterações necessárias no estado do jogo.
flameUp mapa _ _ 0  =                                                                                                                        mapa
flameUp mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                 mapa
                    | ((mapa !! y) !! x) == '?'            =         (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                    | isTherePower  mapa x y               =                                                           removePower           mapa x  y
                    | isTherePlayer mapa x y               =                                                          (flameUp (removePlayer mapa x y) x y n)
                    | isThereBomb   mapa x y               =                                                          (flameUp (blowTimer    mapa x y) x (y-1) (n-1))
                    | otherwise                            =                                                                         flameUp mapa      x (y-1) (n-1)



flameDown :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
-- ^ Função que explode os espaços do mapa abaixo da bomba e chama as funções para efetuar as alterações necessárias no estado do jogo.
flameDown mapa _ _ 0  =                                                                                                                          mapa
flameDown mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                   mapa
                      | ((mapa !! y) !! x) == '?'            =           (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                      | isTherePower  mapa x y               =                                                           removePower             mapa x  y
                      | isTherePlayer mapa x y               =                                                          (flameDown (removePlayer mapa x y) x y n)
                      | isThereBomb   mapa x y               =                                                          (flameDown (blowTimer    mapa x y) x (y+1) (n-1))
                      | otherwise                            =                                                                         flameDown mapa      x (y+1) (n-1)


flameRight :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
-- ^ Função que explode os espaços do mapa à direita da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameRight mapa _ _ 0  =                                                                                                                           mapa
flameRight mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                    mapa
                       | ((mapa !! y) !! x) == '?'            =            (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                       | isTherePower  mapa x y               =                                                           removePower              mapa x  y
                       | isTherePlayer mapa x y               =                                                          (flameRight (removePlayer mapa x y) x y n)
                       | isThereBomb   mapa x y               =                                                          (flameRight (blowTimer    mapa x y) (x+1) y (n-1))
                       | otherwise                            =                                                                         flameRight mapa      (x+1) y (n-1)


flameLeft :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
-- ^ Função que explode os espaços do mapa à esquerda da bomba e chama as funções para efetuar as alterações necessárias no etsado do jogo.
flameLeft mapa _ _ 0  =                                                                                                                          mapa
flameLeft mapa x y n  | ((mapa !! y) !! x) == '#'            =                                                                                   mapa
                      | ((mapa !! y) !! x) == '?'            =           (take y mapa)    ++    [(take x (mapa !! y))   ++ " " ++   (drop (x+1) (mapa !! y ))] ++ (drop (y+1) mapa)
                      | isTherePower  mapa x y               =                                                           removePower             mapa x  y
                      | isTherePlayer mapa x y               =                                                          (flameLeft (removePlayer mapa x y) x y n)
                      | isThereBomb   mapa x y               =                                                          (flameLeft (blowTimer    mapa x y) (x-1) y (n-1))
                      | otherwise                            =                                                                         flameLeft mapa      (x-1) y (n-1)







----------------------------------------------------ESPIRAL-------------------------------------------------------------------------------------------------







espiral :: Mapa -> Tempo -> Mapa
-- ^ Função que coloca as pedras e esmaga o que estiver nessas coordenadas.
espiral mapa n = eraseObj (placeRock mapa n) n



cordLeft  :: Cords -> Abcissa -> [Cords]
-- ^ Função auxiliar que cria a lista de coordenadas da espiral da direita para a esquerda.
cordLeft c 0      = [c]
cordLeft (a,b) n  =  (a,b):cordLeft (a-1,b) (n-1)

cordRight :: Cords -> Abcissa -> [Cords]
-- ^ Função com a mesma funcionalidade que a @cordLeft@ mas com diferente direção.
cordRight c 0     = [c]
cordRight (a,b) n = (a,b):cordRight (a+1,b) (n-1)

cordUp    :: Cords -> Ordenada -> [Cords]
-- ^ Atua como as outras @cord@ mas preenche a lista de baixo para cima.
cordUp c 0        = []
cordUp (a,b) n    =    (a,b):cordUp (a,b-1) (n-1)

cordDown  :: Cords -> Ordenada -> [Cords]
-- ^ Preenche a lista de cima para baixo.
cordDown c 0      = [c]
cordDown (a,b) n  =  (a,b):cordDown (a,b+1) (n-1)

quadrado  :: Cords -> Int -> [Cords]
-- ^ Função que engloba todas as funções cord e que gera ordenadamente a lista de coordenadas que serão destruidas pela espiral.
quadrado (a,b) d  = l1 ++ l2 ++ l3 ++ l4
                where l1 = (cordRight (a,b)        (d-1))
                      l2 = tail (cordDown  (last l1)    (d-1))
                      l3 = tail (cordLeft  (last l2)    (d-1))
                      l4 = tail (cordUp    (last l3)    (d-1))




listaSpiral :: Cords -> Dim -> [Cords]
-- ^ Função que gera uma lista de pares de coordenadas que respresentam a ordem pela qual o mapa será preenchido por pedras.
listaSpiral (a,b) 1 = [(a,b)]
listaSpiral (a,b) d = q ++ listaSpiral (a+1,b+1) (d-2)
                    where q = (quadrado (a,b) d)

placeRock :: Mapa -> Tempo -> Mapa
-- ^ Função que coloca a pedra relativamente a um instante do jogo.
placeRock mapa n = (take y mapa) ++  [(take x (mapa !! y)) ++ "#" ++ (drop (x+1) (mapa !! y))] ++ (drop (y+1) mapa)
                 where (x,y) = (listaSpiral (1,1) d) !! (d*d - n)
                       d = length (mapa !! 0) - 2


eraseObj :: Mapa -> Tempo -> Mapa
-- ^ Função que "esmaga" o que estiver numas dadas coordenadas (onde será colocad auma pedra)
eraseObj mapa n = removePower (removePlayer (removeBomb mapa x y) x y) x y
                where (x,y) = (listaSpiral (1,1) d) !! (d*d - n)
                      d = length (mapa !! 0) - 2









---------------------------------FUNÇÕES QUE EFETUAM ALTERAÇÕES NO MAPA AQUANDO UMA EXPLOSÃO---------------------------------

removePower :: Mapa -> Abcissa -> Ordenada -> Mapa
-- ^ Função que remove o powerup numas dadas coordenadas.
removePower [] _ _ = []
removePower (h:t) x y   = if     elem (h!!0) "+!"     &&    pAbcis h == x    &&     pOrden h == y        then               t
                                                                                                         else h:removePower t x y


removePlayer :: Mapa -> Abcissa -> Ordenada -> Mapa
-- ^ Função que remove o jogador numas dadas coordenadas.
removePlayer [] _ _ = []
removePlayer (h:t) x y  = if     elem (h!!0) "0123"  &&    pAbcis h == x     &&     pOrden h == y        then                t
                                                                                                         else h:removePlayer t x y


blowTimer :: Mapa -> Abcissa -> Ordenada -> Mapa
-- ^ Função que coloca o timer da bomba numas coordenadas a 1.
blowTimer [] _ _ = []
blowTimer (h:t) x y     = if          (h!!0) == '*'   &&    pAbcis h == x    &&     pOrden h == y        then                  (stringTimer h):          t
                                                                                                         else                                h:blowTimer t x y

stringTimer :: Linha -> Linha
-- ^ Função que coloca o timer da string de uma bomba a 1 (a menos que esteja para explodir).
stringTimer st = if last st == '0' then st else (take (length st - 1) st) ++ "1"

removeBomb :: Mapa -> Abcissa -> Ordenada -> Mapa
-- ^ Função que remove a bomba de umas dadas coordenadas (retira-a do Mapa, [String]).
removeBomb [] _ _ = []
removeBomb (h:t) x y   = if    (h!!0)    ==   '*'     &&    pAbcis h == x    &&     pOrden h == y        then              t
                                                                                                         else h:removeBomb t x y








--------------------------FUNÇÕES QUE AVERIGUAM SE HÁ ALTERAÇÕES A FAZER, INTERAÇÃO COM AS CHAMAS DA BOMBA--------------


isTherePower :: Mapa -> Abcissa -> Ordenada -> Bool
-- ^ Função que testa se há um powerup num dado espaço do mapa.
isTherePower [] _ _    = False
isTherePower (h:t) x y = if      elem (h!!0) "+!"     &&       pAbcis h == x    &&     pOrden h == y              then  True
                                                                                                                  else  isTherePower t x y

isTherePlayer :: Mapa -> Abcissa -> Ordenada -> Bool
-- ^ Função que testa se há um jogador num dado espaço do mapa.
isTherePlayer [] _ _ = False
isTherePlayer (h:t) x y = if     elem (h!!0) "0123"   &&       pAbcis h == x    &&     pOrden h == y              then  True
                                                                                                                  else  isTherePlayer t x y


isThereBomb :: Mapa -> Abcissa -> Ordenada -> Bool
-- ^ Função que testa se há um powerup num dado espaço do mapa.
isThereBomb [] _ _    = False
isThereBomb (h:t) x y = if            (h!!0) == '*'   &&       pAbcis h == x    &&     pOrden h == y              then  True
                                                                                                                  else   isThereBomb t x y





















--------------------------------------------FUNÇÕES AUXILIARES QUE TRABALHAM COM COORDENADAS EXPRESSAS EM STRINGS---------------------------------------------


{- | Esta pequena função traduz a função @pOrdenSt@ num @Int@, de forma a podermos trabalhar com esta informação em índices, por exemplo.-}
pOrden :: Linha -> Ordenada
pOrden st = read (pOrdenSt st)


{- | O papel desempenhado pela @pAbcis@ é análogo ao da @pOrden@, mas diz respeito à abcissa de um dado objeto (e.g. jogador, bomba, power up..)-}
pAbcis :: Linha -> Abcissa
pAbcis st = read (pAbcisSt st)


{- | Esta função obtem a parte da string correspondente a um jogador, ou outro objeto com coordenadas, em que se encontra a informação sobre a abcissa

     das coordenadas desse mesmo objeto. Como esta informação começa sempre no 3º caracter de cada string, apenas temos de considerar o quão grande é o

     número relativo a esta mesma coordenada.-}
pAbcisSt :: Linha -> String
pAbcisSt st
      |st !! 3 == ' '     =          (st !! 2):""
      |st !! 4 == ' '     = take 2 (drop 2 st)
      |otherwise          = take 3 (drop 2 st)






{- | Apesar de esta função ter exatamente o mesmo papel que a @pAbcisSt@, a ordenada dos objetos vem em segundo lugar, e, por isso, os caracteres da string

    que correspondem a esta informação também dependem da quantidade de caracteres utilizados para descrever a abcissa.-}
pOrdenSt :: Linha -> String
pOrdenSt st
      |last st /= ' '  = pOrdenSt (st ++ " ")
      |pAbcis st >= 100           = if st !! 7 == ' '  then (st !! 6):"" else if st !! 8 == ' ' then take 2 (drop 6 st) else take 3 (drop 6 st)
      |pAbcis st >= 10            = if st !! 6 == ' '  then (st !! 5):"" else if st !! 7 == ' ' then take 2 (drop 5 st) else take 3 (drop 5 st)
      |otherwise                  = if st !! 5 == ' '  then (st !! 4):"" else if st !! 6 == ' ' then take 2 (drop 4 st) else take 3 (drop 4 st)



range :: Linha -> Int
-- ^ Função que retorna o range da bomba descrita numa dada string.
range string = 1 + read (fst         (span (isDigit) (drop (6 + (length (pAbcisSt string)) + (length (pOrdenSt string))) string))              )






cordsT :: Linha -> (Int,Int)
cordsT st = (pAbcis st ,pOrden st)
