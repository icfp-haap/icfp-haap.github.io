{-|
Module      : Tarefa6_2017li1g12
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g12 where


import Tarefa4_2017li1g12
import LI11718
import Tarefa3_2017li1g12
import Tarefa2_2017li1g12
import Data.List


{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}

bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick e j = Acao acel trav esq dir nitro
                   where (dir,esq) = (acaoDirEsq e j)
                         (acel,trav,nitro) = aceleraOuTrava e j tick                      


{- | A função ’pecaCurva’ verifica se a peça em questão é uma curva.


== Exemplos de utilização:

>>> pecaCurva (Peca Recta 0)
False

>>> pecaCurva (Peca (Curva Norte) 0)
True


-}


pecaCurva :: Peca -> Bool
pecaCurva (Peca (Curva Norte) _) = True
pecaCurva (Peca (Curva Sul) _) = True
pecaCurva (Peca (Curva Oeste) _) = True
pecaCurva (Peca (Curva Este) _) = True
pecaCurva (Peca _ _) = False



{- | Partindo estado atual do jogo, do identificador do jogador dentro do estado e do tempo decorrido desde a ultima decisão, a função ’aceleraOuTrava’ irá devolver o resultado da função ´aceleraOuTravaGAT´. 
Os argumentos terreno, velMaxCurva1, velMaxCurva2, travRectas e nitroRetas utilizados na função ´aceleraOuTravaGAT´ irão depender das ´Propriedades´ utilizada no estado atual do jogo.

-}


aceleraOuTrava :: Jogo 
               -> Int 
               -> Tempo 
               -> (Bool,Bool,Maybe Int) -- os primeiros dois valores Bool dão a acção para acelerar e travar, enquanto o ultimo diz o carro em que se deve aplicar o nitro caso o valor seja diferente de Nothing.
aceleraOuTrava jogo jogador tempo | (gelo == pista) = aceleraOuTravaGAT jogo jogador gelo 2 2 8 8
                                  | (asfalto == pista) = aceleraOuTravaGAT jogo jogador asfalto 0.8 1.65 0 2 
                                  | (terra == pista) = aceleraOuTravaGAT jogo jogador terra 1 2 1 2    
                                   where Jogo mapa pista carros nitros historico = jogo
                                         Mapa (posinicial,oinicial) tab = mapa
                                         Carro pos dir vel = carros !! jogador
                                         terra   = Propriedades 2 3 4 2 15 180
                                         gelo    = Propriedades 0.3 0.4 2 1.5 15 270
                                         asfalto = Propriedades 4 5 8 4 10 120



{- | Partindo do estado atual do jogo, do identificador do jogador dentro do estado, das ´Propriedades´ do estado atual do jogo, da velocidade máxima do carro caso as proximas peças sejam uma curva e não curva,
da velocidade máxima do carro caso as proximas peças sejam duas curvas, do numero de peças reta a partir do qual é necessário começar a travar, e do numero de peças retas que é necessário haver em frente para acionar o nitro, a função ’aceleraOuTravaGAT’ vai retornar os valores das acções referentes a acelerar, travar e nitro. Os argumentos terreno, velMaxCurva1, velMaxCurva2, travRectas e nitroRetas irão depender da função ’aceleraOuTrava’. 


-}


aceleraOuTravaGAT :: Jogo -> Int -> Propriedades -> Double -> Double -> Int -> Int -> (Bool,Bool,Maybe Int)
aceleraOuTravaGAT jogo jogador terreno velMaxCurva1 velMaxCurva2 travRectas nitroRetas | (terreno == pista) && normaVel < velMaxCurva1 && pecasCurva > 1                                                                            = (True,False,Nothing)
                                                                                       | (terreno == pista) && normaVel < velMaxCurva2 && (pecasCurva == 1)                                                                         = (True,False,Nothing)
                                                                                       | (terreno == pista) && ((pecasReta <= travRectas) || (retaDeCurvas <= nitroRetas)) && (normaVel < velMaxCurva2) && (pecasCurva == 0)        = (True,False,Nothing)
                                                                                       | (not (gelo == pista)) && ((pecasReta > nitroRetas) || (retaDeCurvas > nitroRetas))                                                         = (True,False,Just jogador)
                                                                                       | (terreno == pista) && ((pecasReta > travRectas) || (retaDeCurvas > travRectas))                                                            = (True,False,Nothing)                            
                                                                                       | otherwise                                                                                                                                  = (False,True,Nothing)
                                                                                       where Jogo mapa pista carros nitros historico = jogo
                                                                                             Mapa (posinicial,oinicial) tab = mapa
                                                                                             Carro pos dir vel = carros !! jogador
                                                                                             (normaVel,anguloVel) = cartesianoToPolar (vel)
                                                                                             posPeca = (floor (fst pos),floor (snd pos)) 
                                                                                             (pecas,orientacoes,posicoes) = separatePecaOrientacaoPosicao (listaposicoes posinicial oinicial tab 0 oinicial posinicial)
                                                                                             historicoGamer = historico !! jogador
                                                                                             indexespos = (elemIndices posPeca posicoes)
                                                                                             indexeshistory = (elemIndices posPeca (historico !! jogador))
                                                                                             indexAtualiz = findindexpos indexeshistory indexespos historicoGamer posicoes
                                                                                             pecasReta = length $ takeWhile (retaOuRampa) (drop (indexAtualiz) pecas)
                                                                                             pecasCurva = length $ takeWhile (pecaCurva) (drop (indexAtualiz) pecas)
                                                                                             retaDeCurvas = curvaformareta jogo jogador 0 posPeca posicoes indexAtualiz
                                                                                             gelo = Propriedades 0.3 0.4 2 1.5 15 270


{- | A função ’retaOuRampa’ verifica se a peça em questão é uma reta ou uma rampa.


== Exemplos de utilização:

>>> retaOuRampa (Peca Recta 0)
True

>>> retaOuRampa (Peca (Curva Norte) 0)
False

>>> retaOuRampa (Peca (Rampa Este) 0)
True

-}


retaOuRampa :: Peca -> Bool
retaOuRampa (Peca Recta _)            = True
retaOuRampa (Peca (Rampa Norte) _)    = True
retaOuRampa (Peca (Rampa Oeste) _)    = True
retaOuRampa (Peca (Rampa Sul) _)      = True
retaOuRampa (Peca (Rampa Este) _)     = True
retaOuRampa (Peca _ _ )               = False



{-| A função 'curvaformareta' vai verificar se existem duas curvas com peças alternadase vai devolver um inteiro caso existam, com o número dessas peças que formam uma reta. -}


curvaformareta :: Jogo -> Int -> Int -> Posicao -> [Posicao] -> Int -> Int
curvaformareta jogo jogador int posPeca posicoes indexAtualiz| tipo == Curva Norte && tipon == Curva Sul = 1 + funcao
                                                             | tipo == Curva Sul && tipon == Curva Norte = 1 + funcao
                                                             | tipo == Curva Este && tipon == Curva Oeste = 1 + funcao
                                                             | tipo == Curva Oeste && tipon == Curva Este = 1 + funcao
                                                             | otherwise = 0
       where Peca tipon a                           = pegaPecaTab (posicoes !! (indexAtualiz + 1 + int)) tab
             Peca tipo al                           = pegaPecaTab (posicoes !! (indexAtualiz + int)) tab
             ori                                    = (listaOrientacoes !! (indexOa + int))
             orin                                   = (listaOrientacoes !! (indexOa + 2 + int))
             oriatualizada = atualizarorientacao ori posPeca tipo tab
             indexOa = indexori' listaOrientacoes oriatualizada 0 
             Jogo mapa pista carros nitros historico = jogo
             Mapa (posinicial,oinicial) tab = mapa
             funcao  = curvaformareta jogo jogador (int +1) posPeca posicoes indexAtualiz


{-| A função 'indexori' serve para percorrer a função @listaOrientacoes@, e para isso recebe como argumento a listaOrientacoes, uma Orientação, e um Inteiro.

== Exemplos de utilização:

>>> indexori' orientacoesproximapeca  Norte 0
0

>>> indexori' orientacoesproximapeca  Oeste  0
3

-}

indexori' :: [Orientacao] -> Orientacao -> Int -> Int
indexori' (a:b) o i | a == o        = i
                    | otherwise     = indexori' b o (i+1)



{- | Partindo estado atual do jogo e do identificador do jogador dentro do estado, a função ’acaoDirEsq’ retorna os valores booleanos referentes à acção direita e esquerda a ser tomada pelo bot.



-}


acaoDirEsq :: Jogo -> Int -> (Bool,Bool)
acaoDirEsq jogo jogador | normalizaAng (angDirMinusIdeal) >= 0 && normalizaAng (angDirMinusIdeal) < 180       = (True,False)
                        | normalizaAng (angDirMinusIdeal) >= 180 && normalizaAng (angDirMinusIdeal) <= 360    = (False,True)
                        | otherwise                                                                           = (False,False)
                        where Jogo mapa pista carros nitros historico = jogo
                              Carro pos dir vel = carros !! jogador
                              posIdeal = posicaoideal jogo jogador
                              vetorIdeal = ((fst posIdeal) - (fst pos),(snd posIdeal) - (snd pos))
                              (normaIdeal,anguloIdeal) = cartesianoToPolar (vetorIdeal)
                              (normaVel,anguloVel) = cartesianoToPolar (vel)
                              anguloIdealNormz = normalizaAng anguloIdeal
                              anguloDirNormz = normalizaAng dir
                              angDirMinusIdeal = anguloDirNormz - anguloIdealNormz

{- | Partindo estado atual do jogo e do identificador do jogador dentro do estado, a função ’posicaoideal’ retorna a ponto ideal da proxima peça para onde o bot deverá ir. 
 

-}

posicaoideal :: Jogo -> Int -> Ponto
posicaoideal jogo jogador | tipo == (Curva Norte)    = (fi xp + 0.75,fi yp + 0.75)
                          | tipo == (Curva Sul)      = (fi xp + 0.25,fi yp + 0.25)
                          | tipo == (Curva Oeste)    = (fi xp + 0.75,fi yp + 0.25)
                          | tipo == (Curva Este)     = (fi xp + 0.25,fi yp + 0.75)
                          | otherwise                = (fi xp + 0.5,fi yp + 0.5)
                          where fi = fromIntegral
                                (Peca tipo altura,pos) = positionGoal jogo jogador
                                (xp,yp) = pos


{- | Partindo estado atual do jogo e do identificador do jogador dentro do estado, a função ’positionGoal’ devolve um tuplo com a próxima posição do caminho para onde o bot deverá ir e a peça que correponde a essa posição. 

-}

positionGoal :: Jogo -> Int -> (Peca,Posicao)
positionGoal jogo jogador | posPeca == posinicial             = (pecas !! 0, posicoes !! 0)
                          | otherwise                         = (pecas !! ((findindexpos indexeshistory indexespos historicoGamer posicoes) + 1), posicoes !! ((findindexpos indexeshistory indexespos historicoGamer posicoes) + 1))
                          where Jogo mapa pista carros nitros historico = jogo
                                Mapa (posinicial,oinicial) tab = mapa
                                Carro pos dir vel = carros !! jogador
                                historicoGamer = historico !! jogador
                                posPeca = (floor (fst pos),floor (snd pos)) 
                                (pecas,orientacoes,posicoes) = separatePecaOrientacaoPosicao (listaposicoes posinicial oinicial tab 0 oinicial posinicial)
                                indexespos = (elemIndices posPeca posicoes)
                                indexeshistory = (elemIndices posPeca (historico !! jogador))


{- | Partindo dos indices da posição da peça em que se situa o bot na lista do historico do bot, dos indices da posição da peça em se situa o bot na lista de posições do caminho (´indexespos´), 
do historico do bot e da lista de posições do caminho a função ´findindexpos´ vai devolver o resultado da função ´verificar index´ no caso da lista ´indexespos´ ter comprimento maior que 1 e devolve o unico valor da lista ´indexespos´ caso contrario.   



-}

findindexpos :: [Int] -> [Int] -> [Posicao] -> [Posicao] -> Int
findindexpos indexeshistory indexespos historicoGamer posicoes | (length indexespos) > 1                    = verificarindex indexeshistory indexespos historicoGamer posicoes
                                                               | otherwise                                  = indexespos !! 0


{- | Partindo dos indices da posição da peça em que se situa o bot na lista do historico do bot, dos indices da posição da peça em se situa o bot na lista de posições do caminho, 
do historico do bot e da lista de posições do caminho a função ´verificarindex´ retorna o index da lista de posições, que se refere à proxima peça para onde o bot deve seguir. 

-}


verificarindex :: [Int] -> [Int] -> [Posicao] -> [Posicao] -> Int   
verificarindex indexeshistory indexespos historicoGamer posicoes | (elem (posicoes !! (indexposZero + 1)) historicoGamer)   = indexposOne
                                                                 | otherwise                                                = indexposZero                     
                                                                  where indexposZero = indexespos !! 0
                                                                        indexposOne = indexespos !! 1



