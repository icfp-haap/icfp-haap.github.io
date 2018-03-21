{-|
Module      : Tarefa2_2017li1g118
Description : Módulo que testa se a pista criada é válida.
Copyright : Diogo Rio <diogorio53@hotmail.com>;
            Jorge Cerqueira <jorgejg575926@gmail.com>
Um Módulo para testar a validade de um mapa no ambito da UC de LI1
-}

module Tarefa2_2017li1g118
(
-- * Função Valida
testesT2,valida,
-- * Função Check3
check3,compativel,getPeca,
-- * Função Check5
check5,
-- * Função Check6
check6,checkIfLava,checkDim,
-- * Função CheckPassos
checkPassos,getPecaAlt,reversePassos,pecaToPasso,compativelPecaOriAlt,countNotLava,checkPartida
)

where

import LI11718
import Tarefa1_2017li1g118

testesT2 :: [Tabuleiro]
testesT2 =[ [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Sul) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 1]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
              [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
          ]

{- | Esta função testa se o mapa é ou não válido -}
valida :: Mapa -> Bool
valida m = check3 m && check5 m && check6 m && checkPassos m && checkPartida m && checkDim m

{- | Esta função utiliza as funções compativel e getPeca para testar se a orientação inicial é compatível com a peça de partida.
-}

{- | Testa se um mapa tem dimensão válida.-}
checkDim :: Mapa -> Bool
checkDim (Mapa _ (x:xs)) = checkDimH (length x) xs
                        where
                        checkDimH len [] = True
                        checkDimH len (x:xs)|len == length x = checkDimH len xs
                                            |otherwise = False

check3 :: Mapa -> Bool
check3 (Mapa (pos,ori) tab) = compativel (getPeca tab pos) ori

{- | Esta função testa se o mapa é valido para outra partida-}
checkPartida :: Mapa -> Bool
checkPartida (Mapa (pos,ori) tab) |mp == Nothing = False
                                  |otherwise     = checkPassos (Mapa (nPos,nOri) tab)
                                  where
                                  (mp,nOri,_) = pecaToPasso peca ori alt
                                  peca = getPeca tab pos
                                  alt  = getPecaAlt peca ori
                                  nPos = move pos nOri

{- | Esta função testa se a peça é compatível com a orientação qualquer. -}

compativel :: Peca -> Orientacao -> Bool
compativel (Peca Recta h) _ = True
compativel (Peca (Rampa ori) h) o |ori == o           = True
                                  |invertOri ori == o = True
                                  |otherwise = False

compativel (Peca Lava h) _ = False
compativel (Peca (Curva ori) h) o |ori == o || (ori == rodaOriDir o) = True
                                  |otherwise = False
{- | Esta função diz-nos qual a peca numa determinada posição do tabuleiro.
-}

getPeca :: Tabuleiro -> Posicao -> Peca
getPeca t pos = getPecaH t pos
               where
                getPecaH2 (t:ts) 0 = t
                getPecaH2 (t:ts) x = getPecaH2 ts (x-1)
                getPecaH (t:ts) (x,0) = getPecaH2 t x
                getPecaH (t:ts) (x,y) = getPecaH (ts) (x,y-1)

{- | Esta função dá-nos a altura de uma peça.-}
getPecaAlt :: Peca -> Orientacao -> Int
getPecaAlt (Peca (Rampa o) h) ori |o == ori   = h
                                  |otherwise = h+1
getPecaAlt (Peca (_) h) ori = h

{- | Esta função testa se todas as peças do tipo Lava estão à altura 0.-}
check5 :: Mapa -> Bool
check5 (Mapa q [[]]) = True
check5 (Mapa q (((Peca Lava 0):xs):ys) ) = check5 (Mapa q (xs:ys))
check5 (Mapa q (((Peca Lava _):xs):ys) ) = False
check5 (Mapa q ([]:ys)) = check5 (Mapa q ys)
check5 (Mapa q ((x:xs):ys) ) = check5 (Mapa q (xs:ys))

{- | Esta função teste se o mapa é rectangular e rodeado por Lava.-}

check6 :: Mapa -> Bool
check6 (Mapa (_,_) (y:ys)) = check6H  y ys
                             where
                              check6H y [] = checkIfLava y True
                              check6H y (y1:ys) |checkIfLava y False = check6H y1 ys
                                                |otherwise = False
{- | Esta função testa se uma peça é ou não do tipo Lava.-}

checkIfLava :: [Peca] -> Bool -> Bool
checkIfLava [] _ = True
checkIfLava (x:xs) True |x==l = checkIfLava xs True
                        |otherwise = False
                          where l = (Peca Lava 0)
checkIfLava (x:xs) False = checkIfLava (x:[last (x:xs)]) True

{-| Esta função testa se é possível converter um Mapa em uma lista de passos.-}

checkPassos :: Mapa -> Bool
checkPassos m = checkPassosH (reverseMapa m) []
                 where
                  reverseMapa (Mapa (pos,ori) tab) = reversePassos tab pos ori
                  checkPassosH [] n = (countNotLava (constroi n)) == countNotLava m
                  checkPassosH (x:xs) n |x== Nothing = False
                                        |otherwise   = checkPassosH xs (n++[x_])
                                        where
                                          Just x_ = x

{-| Esta função tranforma um tabuleiro em uma lista de passos.-}
reversePassos :: Tabuleiro -> Posicao -> Orientacao -> [Maybe Passo]
reversePassos tab pos ori = reversePassosAc tab pos ori pos (getPecaAlt (getPeca tab pos) ori) False
                             where
                              reversePassosAc tab pos ori initpos alt False |passo==Nothing = [Nothing]
                                                                            |otherwise = passo:(reversePassosAc newTab (move pos newOri) newOri initpos newAlt True)
                                                                             where
                                                                              (passo,newOri,newAlt) = pecaToPasso (getPeca tab pos) ori alt
                                                                              l = (Peca Lava 0)
                                                                              newTab = swapTabuleiroPeca tab l pos

                              reversePassosAc tab pos ori initpos alt _|pos == initpos = []
                                                                       |passo==Nothing = [Nothing]
                                                                       |otherwise = passo:(reversePassosAc newTab (move pos newOri) newOri initpos newAlt True)
                                                                        where
                                                                        (passo,newOri,newAlt) = pecaToPasso (getPeca tab pos) ori alt
                                                                        l = (Peca Lava 0)
                                                                        newTab = if passo == Just Avanca then tab
                                                                                 else swapTabuleiroPeca tab l pos

{-| Esta função é a função auxiliar para a função reversePassos.-}
pecaToPasso :: Peca -> Orientacao -> Altura -> (Maybe Passo,Orientacao,Altura)
pecaToPasso (Peca Recta h) o alt  |h==alt = (Just Avanca,o,h)
                                  |otherwise = (Nothing,o,h)
pecaToPasso (Peca (Curva o) h) ori hact | compativel == 2 = (Just CurvaDir,rodaOriDir ori,h)
                                        | compativel == 1 = (Just CurvaEsq,rodaOriEsq ori,h)
                                        | otherwise = (Nothing,o,h)
                                        where
                                          compativel = compativelPecaOriAlt (Peca (Curva o) h) ori hact

pecaToPasso (Peca (Rampa o) h) ori hact | compativel == 2 = (Just Sobe,ori,hact+1)
                                        | compativel == 1 = (Just Desce,ori,hact-1)
                                        | otherwise = (Nothing,o,h)
                                        where
                                          compativel = compativelPecaOriAlt (Peca (Rampa o) h) ori hact
pecaToPasso (Peca Lava h) o _ = (Nothing,o,h)

{-| Esta função testa se uma peça é compativel com uma certa altura e orientação,
a função não devolve um Bool para separar as subidas das descidas e distinguir
para que lado irão as curvas.-}
compativelPecaOriAlt :: Peca -> Orientacao -> Altura -> Int
compativelPecaOriAlt (Peca (Curva o) h) ori alt |checkOriDir  && checkAlt = 2
                                                |checkOriEsq  && checkAlt = 1
                                                |otherwise = 0
                                                 where
                                                 checkOriDir = o == ori
                                                 checkOriEsq  = rodaOriEsq o == ori
                                                 checkAlt = h == alt

compativelPecaOriAlt (Peca (Rampa o) h) ori alt |checkOriSobe  && checkAltSobe  = 2
                                                |checkOriDesce && checkAltDesce = 1
                                                |otherwise = 0
                                                 where
                                                 checkOriSobe  = o == ori
                                                 checkOriDesce = invertOri o == ori
                                                 checkAltSobe  = hdiff == 0
                                                 checkAltDesce = hdiff == 1
                                                 hdiff = alt - h
{-| Esta função conta as peças que não são Lava.-}
countNotLava :: Mapa -> Int
countNotLava (Mapa (pos,ori) tab) = countNotLavaAc tab 0
                                  where
                                    countNotLavaAc ([]:ys) n = countNotLavaAc ys n
                                    countNotLavaAc [] n = n
                                    countNotLavaAc (((Peca Lava _):xs):ys) n = countNotLavaAc (xs:ys) n
                                    countNotLavaAc ((x:xs):ys) n = countNotLavaAc (xs:ys) (n+1)
