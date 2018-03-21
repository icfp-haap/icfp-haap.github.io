---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------     TAREFA 1 - VALIDATION     ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Validation
Description : Módulo que valida tabuleiros de /Sokoban./
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo de validação dos tabuleiros de __/Sokoban/__. Verifica
o cumprimento de todas as regras e os padrões previamente 
definidos para os tabuleiros e as respetivas coordenadas.

De modo a optimizar o código, adpenas são verificadas as coordenadas
depois de verificado o tabuleiro, evitando assim tarefas desnecessárias,
pois um erro no tabuleiro estará sempre antes de um erro nas coordenadas.

Além disso, existe em ambas as situações (tabuleiro e coordenadas)
verificações que ocorrem linha a linha sempre que possível, ou seja, 
apenas é feito um conjunto de testes, uma linha de cada vez. Caso essa 
linha não passe nos testes não se realizam os testes para as linhas 
seguintes evitando assim o uso de recursos de forma desnecessária. No entanto, 
existem também testes que necessitam de ser feitos com o conjunto total,
tanto das linhas como das coordenadas, nesta situação, caso alguma linha não
passe, essa função não continua para as linhas seguintes.

__Nota__: É usado o __codNotOk__ (@minBound :: Int@) para representar um __erro__ 
sendo que, quando isto acontece, é __retornada__ a linha onde foi detetado o 
erro. Quando o teste é passado é usado o __codOk__ (@maxBound :: Int@).
-}

module Main where

import Data.Char
import Data.Maybe

-- | Lista com o tabuleiro.
type Tabuleiro  = [String]

-- | Lista com as coordenadas.
type Coords     = [String]

-- | Posições (x,y).
type Posicao    = (Int,Int) 

-- | /Input/ constituído pelo tabuleiro e pelas coordenadas.
type Input      = [String]

-- | /String/ com coordenadas ou com o tabuleiro.
type Linha      = String 

{-
Legenda das váriáveis usadas:
i   <- input dividido em tabuleiro e coordenadas
ht  <- head tabuleiro
tt  <- tail tabuleiro
lt  <- length tabuleiro
hc  <- head coordenadas
tc  <- tail coordenadas
lc  <- length coordenadas
lht <- length head tabuleiro
lT  <- length total
cl  <- current line
-}

---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Recebe tabuleiro.
inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

-- | Divide linhas do output.
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

-- | Função /Main/.
main = do inp <- getContents
          putStr (outStr (tarefa1 (inStr inp)))

-- | Função inicial.
{-|
* Nesta parte da tarefa, o objetivo será confirmar se o /input/ está 
correto ou se este apresenta erros. No caso de não existirem erros 
será apresentada a string __OK__. Caso existam, será apresentada a 
linha onde se verifica o __erro__.

    >         Input                    Output
    >
    >  ###################     |         OK
    >  #####   ###########     |         
    >  #####   ###########     |         
    >  #####   ###########     | 
    >  ###      ##########     |    
    >  ### # ## ##########     |     
    >  #   # ## #####  ..#     |     
    >  #               ..#     |     
    >  ##### ### # ##  ..#     |     
    >  #####     #########     |         
    >  ###################     |         
    >  11 2
    >  5 8
    >  7 7
    >  5 6
    >  7 6
    >  2 3
    >  5 3
    >
    >         Input                    Output
    >
    >  ###################     |         7 
    >  ##                #     |         
    >  #####      ##   ###     |         
    >  ###### #   ##   ###     |       
    >  ###################     |
    >  5 3
    >  7 3
    >  7 3
    >
-}

tarefa1 :: Input -> [String]
tarefa1 p = [validatab p]


---------------------------------------------------------------------
-- * __Divisão do /Input/ em tabuleiro e coordenadas__
---------------------------------------------------------------------

-- | Divide o /input/ em tabuleiro e coordenadas.
--
-- ==== __/Exemplo de utilização/__
--
-- >>> divideTab ["####","#  #","12 2","1 15"]
-- (["####","#  #"],["12 2","1 15"])
divideTab :: Input -> (Tabuleiro, Coords)
divideTab [] = ( [] , [])
divideTab l | last l `elem` ["L","U","R","D",""] = divideTab (init l)
divideTab ([]:ts) = let (tab,coor) = divideTab ts
                    in ( []:tab , coor )
divideTab l@((h:t):ts)
    | isDigit h = ( [] , l )
    | otherwise = let (tab,coor) = divideTab ts
                  in ( (h:t):tab , coor )

---------------------------------------------------------------------
-- * __Verificação geral__
---------------------------------------------------------------------

-- | Validação geral (tabuleiro e coordenadas).
validatab :: [String] -> String
validatab t
    | validalinhastab t /= codOk = show (validalinhastab t)
    | validacoordtab t  /= codOk = show (validacoordtab t)
    | otherwise                  = "OK"

-- | Código a retornar caso __"OK"__.
codOk :: Int
codOk = maxBound
-- | Código a retornar em caso de __erro__.
codNotOk :: Int
codNotOk = minBound

---------------------------------------------------------------------
-- ** Validação linha a linha do tabuleiro
---------------------------------------------------------------------

-- | Validação das __linhas__.
validalinhastab :: Input -> Int
validalinhastab l
    | null tab  = 1  -- verificacao se existe tabuleiro
    | otherwise = validaLinha tab 1 (length (head tab))
    where (tab,coor) = divideTab l


-- | Validação individual de cada linha.
validaLinha :: Tabuleiro -> Int -> Int -> Int
validaLinha [] _ _ = codOk
validaLinha (ht:tt) cl lht
    | null ht                                                    = cl
    | verificaContent ht                                /= codOk = cl
    | (cl == 1 || null tt) && verificaParedesTopBot ht  /= codOk = cl
    | verificaParedesSide ht                            /= codOk = cl
    | verificaComprimento ht lht                        /= codOk = cl
    | otherwise = validaLinha tt (cl+1) lht

---------------------------------------------------------------------
-- ** Validação linha a linha das coordenadas
---------------------------------------------------------------------

-- | Validação geral das __coordenadas__.
validacoordtab :: Input -> Int
validacoordtab l
    | null coor = length tab + 1 -- verificacao se existem coordenadas
    | otherwise = minimum [validCoord,confNCoord,confCRepet]
    where (tab,coor) = divideTab l
          validCoord = validaCoordenadas coor tab (length tab + 1) (length tab) (length (head tab))
          confNCoord = confirmNumberOfCoor coor tab (length tab + length coor)
          confCRepet = confRepCoor coor [] (length tab)

-- | Validação linha a linha das coordenadas.
validaCoordenadas :: Coords -> Tabuleiro -> Int -> Int -> Int -> Int
validaCoordenadas [] _ _ _ _ = codOk
validaCoordenadas (hc:tc) tab cl lt lht
    | null hc                                             = cl
    | verificaValorCoor hc                       /= codOk = cl
    | confValuesCoor (a,b) lt lht                /= codOk = cl
    | confColocC tab (a,b)                       /= codOk = cl
    | otherwise = validaCoordenadas tc tab (cl+1) lt lht
    where (a,b) = (converterEmDuplos hc)

-------------------------------------------------------------------
-- * __Funções de verificação do tabuleiro__
---------------------------------------------------------------------

-- ** Verifica o comprimento do tabuleiro
-- | Verifica se o comprimento das linhas do tabuleiro é igual ao 
--   comprimento da /head/ do tabuleiro.
--
-- ==== __/Exemplo de utilização/__
--
-- >>> verificaComprimento "####" 4
-- 9223372036854775807 
-- >>> verificaComprimento "#" 3
-- -9223372036854775808
verificaComprimento :: Linha -> Int -> Int
verificaComprimento [] _ = codNotOk
verificaComprimento l lht 
    | length l == lht = codOk
    | otherwise       = codNotOk 

-- ** Verifica paredes (topo e base)
-- | Verifica a existência de "paredes" na parte superior e
-- inferior do tabuleiro, ou seja, confirma se todos os
-- elementos da primeira e última linha do tabuleiro são
-- um "#".
--
-- ==== __/Exemplos de utilização/__
--
-- >>> verificaParedesTopBot "####"
-- 9223372036854775807
-- >>> verificaParedesTopBot "## .#"
-- -9223372036854775808
verificaParedesTopBot :: Linha -> Int
verificaParedesTopBot [a] = codOk
verificaParedesTopBot (h:t)
    | h == '#'  = verificaParedesTopBot t
    | otherwise = codNotOk

-- ** Verifica paredes laterais
-- | Verifica a existência de "paredes" laterais, ou seja, 
-- confirma se o primeiro e último elemento de cada linha
-- do tabuleiro é um "#".
--
-- ==== __/Exemplos de utilização/__
--
-- >>> verificaParedesSide "#  #"
-- 9223372036854775807
-- >>> verificaParedesSide "# #."
-- -9223372036854775808
verificaParedesSide :: Linha -> Int
verificaParedesSide l
    | head l == last l && head l == '#' = codOk
    | otherwise                         = codNotOk


-- ** Verifica conteúdo
-- | Testa se o conteúdo das linhas do tabuleiro é diferente do
-- esperado, isto significa que as linhas __apenas__ podem ser
-- constituídas pelos caractéres: __''#''__, __'' ''__ e __''.''__
--
--  ==== __/Exemplos de utilização/__
--  >>> verificaContent "# #"
-- 9223372036854775807
--  >>> verificaContent "#as#"
-- -9223372036854775808
verificaContent :: Linha -> Int
verificaContent [] = codOk
verificaContent (h:t)
    | h `elem` "# ." = verificaContent t
    | otherwise      = codNotOk

---------------------------------------------------------------------
-- * __Funções de verificação das coordenadas__
---------------------------------------------------------------------

-- ** Contagem de pontos
-- | Contagem do número total de pontos no tabuleiro (__locais de arrumação__)
-- que são posteriormente comparados com o número total de coordenadas.
--
-- ==== __/Exemplos de utilização/__
--
-- >>> contagemPontos ["##.","#..#"]
-- 3
contagemPontos :: Tabuleiro -> Int
contagemPontos = foldr ((+) . contagemPontosAux) 0

-- | Função auxiliar da @contagemPontos@.
contagemPontosAux :: Linha -> Int
contagemPontosAux [] = 0
contagemPontosAux (h:t)
    | h == '.'  = 1 + contagemPontosAux t
    | otherwise = contagemPontosAux t

-- ** Compara coordenadas com pontos
-- | Confirma o número de coordenadas inseridas e compara com
-- o número de locais de arrumação.
--
-- ==== __/Exemplos de utilização/__
--
-- >>> confirmNumberOfCoor ["12 2","13 3"] ["##.","##"] 4
-- 9223372036854775807
-- >>> confirmNumberOfCoor ["12 2","13 3"] ["#..","##"] 4
-- 5
-- >>> confirmNumberOfCoor ["12 2","13 3"] ["#  ","##"] 4
-- 4
confirmNumberOfCoor :: Coords -> Tabuleiro -> Int -> Int
confirmNumberOfCoor coor tab p
    | length coor == (contagemPontos tab + 1) = codOk
    | length coor > (contagemPontos tab + 1)  = (p - (length coor - contagemPontos tab - 2))
    | otherwise                               = (p + 1)

-- ** Separa /string/ com coordenadas em duas strings
-- | Recebe uma lista e retorna um par. __Nota__: de forma a
-- evitar erros de tipo, quando a @/length/@  da lista é
-- diferente de __2__, retorna o duplo @(-1,-1)@ que seguidamente retorna
-- erro quando é verificada na função __@confValuesCoor@__.
--
-- ==== __/Exemplos de utilização/__
--
-- >>> listaParaDuplo ["12","2"]
-- ("12","2")
-- >>> listaParaDuplo ["12","1","3"]
-- ("-1","-1")
listaParaDuplo :: [String] -> (String,String)
listaParaDuplo [x,y] = (x,y)
listaParaDuplo list | length list /= 2 = ("-1","-1") 

-- ** Verifica se recebe números
-- | Verifica se as coordenadas são números.
--
--   ==== __/Exemplos de utilização/__
-- >>> verificaValorCoor "12 2"
-- 9223372036854775807
-- >>> verificaValorCoor "12 a"
-- -9223372036854775808
verificaValorCoor :: Linha -> Int
verificaValorCoor [] = codOk
verificaValorCoor (h:t)
    | isDigit h || h == ' ' = verificaValorCoor t
    | otherwise             = codNotOk

-- ** Converter coordenadas em duplos
-- | Converte coordenadas em duplos de inteiros. __Nota__: As coordenadas apenas 
-- serão verificadas no caso destas
-- serem números para evitar erros de tipo.
--
-- ==== __/Exemplos de utilização/__
--
-- >>> converterEmDuplos "3 4"
-- (3,4)
converterEmDuplos :: Linha -> Posicao
converterEmDuplos hc = let (a,b) = (listaParaDuplo $ words hc)
                        in (read a, read b)

-- ** Verifica intervalo de localização no tabuleiro
-- | Verifica se as coordenadas estão no interior do tabuleiro
-- e se são positivas.
--
-- ==== __/Exemplos de utilização/__
-- >>> confValuesCoor (3,3) 10 12
-- 9223372036854775808
-- >>> confValuesCoor (3,3) 2 3
-- -9223372036854775808
-- >>> confValuesCoor (-3,0) 1 2
-- -9223372036854775808
confValuesCoor :: Posicao -> Int -> Int -> Int
confValuesCoor (a,b) lt lht
    | a<lht && a>0 && b>0 && b<lt = codOk
    | otherwise                   = codNotOk

--  ** Coordenadas repetidas
-- | Confirma se existem coordenadas repetidas.
--
-- ==== __/Exemplos de utilização/__
-- >>> confRepCoor ["12 2","2 1","12 2"] [] 17
-- 20
-- >>> confRepCoor ["1 2","3 2","1 5"] [] 7
-- 9223372036854775807
confRepCoor :: Coords -> [String] -> Int -> Int
confRepCoor [] _ _ = codOk
confRepCoor (hc:tc) list lt 
    | hc `elem` list = lt+1
    | otherwise      = confRepCoor tc (hc:list) (lt+1)

-- ** Confirma local
-- | Confirma se é possível colocar as coordenadas no
-- tabuleiro e que estas não se sobrepõem num muro.
--
-- ==== __/Exemplos de utilização/__
-- >>> confColocC ["####","####","## #","####"] (2,1)
-- 9223372036854775807
-- >>> confColocC ["####","## #","####","####"] (1,1)
-- -9223372036854775808
confColocC :: Tabuleiro -> Posicao -> Int
confColocC tab (a,b)
    | ((tab !! (length tab - b - 1)) !! a) `elem` "#" = codNotOk
    | otherwise                                       = codOk


