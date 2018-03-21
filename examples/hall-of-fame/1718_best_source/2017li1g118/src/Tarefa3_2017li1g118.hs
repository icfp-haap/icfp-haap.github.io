{-|
Module      : Tarefa3_2017li1g118
Description : Módulo que movimenta um carro.
Copyright : Diogo Rio <diogorio53@hotmail.com>;
            Jorge Cerqueira <jorgejg575926@gmail.com>
Um Módulo para movimentar um carro em um mapa no ambito da UC de LI1
-}


module Tarefa3_2017li1g118 (
testesT3,movimenta,
-- * Funções de movimento
moverCarro,moverLinhaReta,moverTempo,
-- * Operações com o Carro
reflect,reflectCurva,reflectCurvaNS,reflectCurvaEO,
-- * Operações vetorias
add,mult,modulo,dotProduct,
-- * Testes de Tipos
isRampa,isCurva,
-- * Funções que testam o ambiente
getPecaCarro,validaCurva
)
where

import Tarefa1_2017li1g118
import Tarefa2_2017li1g118
import LI11718

type Vetor = (Double,Double)
testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = []

tabuleiroTeste = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta tab t c = nC
          where
          (tab_,t_,p_,nC) = moverCarro (tab,t,p,Just c)
          p = getPecaCarro (tab,Just c)

moverCarro :: (Tabuleiro,Tempo,(Peca,Posicao),Maybe Carro) -> (Tabuleiro,Tempo,(Peca,Posicao),Maybe Carro)
moverCarro (tab,t,p,Nothing) = (tab,0,p,Nothing)
moverCarro (tab,0,p,c) = (tab,0,p,c)
moverCarro (tab,t,(p,pPos),c) |vel == (0,0) = (tab,0,(p,pPos),c)
                              |nP == Peca Lava 0 && ph>=0 = (tab,t,(p,pPos),Nothing)
                              |(isRampa nPx || isRampa px) && (abs(ph-nPh) <=1) = moverCarro (tab,nT,(nP,npPos),nC)
                              |isCurva nPx = if validaCurva nP nC then moverCarro (tab,nT,(nP,npPos),nC)
                                             else if nPh >= 0  then moverCarro (tab,0,(nP,npPos),Nothing)
                                             else moverCarro (tab,t,(p,pPos),reflectCurva nP c)
                              |nPh>ph = moverCarro (tab,t,(p,pPos),reflect pPos npPos c)
                              |nPh<ph = moverCarro (tab,0,(nP,npPos),Nothing)
                              |otherwise = moverCarro (tab,nT,(nP,npPos),nC)
                              where
                              (Peca px ph) = p
                              (Peca nPx nPh) = nP
                              Just (Carro _ _ vel) = c
                              (nT,nC) = moverLinhaReta (t,c)
                              (nP,npPos) = getPecaCarro (tab,nC)

{-| Função que move o carro uma pequena distância em uma linha reta. -}
moverLinhaReta :: (Tempo,Maybe Carro) -> (Tempo,Maybe Carro)
moverLinhaReta (t,c) |t<=0 = (0,moverTempo t c)
                     |otherwise = (nT,moverTempo t_ c)
                     where
                     Just (Carro pos ang vel) = c
                     nT = t-t_
                     t_ = 0.05/(modulo vel)

{- Dado um tempo e um carro devolve o carro quando aplicada a velocidade no espaço de tempo dado.-}
moverTempo :: Tempo -> Maybe Carro -> Maybe Carro
moverTempo t c = nC
               where
               Just (Carro pos ang vel) = c
               nPos = add pos (mult vel t)
               nC = Just (Carro nPos ang vel)

{-Devolve a Peca em que se encontra um carro.-}
getPecaCarro :: (Tabuleiro,Maybe Carro) -> (Peca,Posicao)
getPecaCarro (t,c) = ((t !! y_) !! x_,(x_,y_))
                where
                Just (Carro (x,y) ang (vx,vy)) = c
                x_ = floor x
                y_ = floor y

{-| Inverte a velocidade do carro no eixo do x ou y.-}
reflect :: Posicao -> Posicao -> Maybe Carro -> Maybe Carro
reflect (xi,yi) (xf,yf) c |xi /= xf && yi /= yf = Just (Carro pos ang (-vx,-vy))
                          |xi /= xf = Just (Carro pos ang (-vx,vy))
                          |yi /= yf = Just (Carro pos ang (vx,-vy))
                          where
                          Just (Carro pos ang (vx,vy)) = c

{-| Devolve True se um Tipo for (Rampa Orientacao).-}
isRampa :: Tipo -> Bool
isRampa (Rampa _) = True
isRampa _         = False

{-| Devolve True se um Tipo for (Curva Orientacao)-}
isCurva :: Tipo -> Bool
isCurva (Curva _) = True
isCurva _         = False

{-| Soma dois vetores.-}
add :: Vetor -> Vetor -> Vetor
add (a,b) (c,d) = (a+c,b+d)

{-| Multiplica dois vetores. -}
mult :: Vetor -> Double -> Vetor
mult (x,y) a = (a*x,a*y)

{-| Calcula o módulo de um vetor -}
modulo :: Vetor -> Double
modulo (x,y) = sqrt (x^2 + y^2)

{-| Verifica se o carro está numa posição válida da curva, ou seja, não se encontra na parte com Lava.-}
validaCurva :: Peca -> Maybe Carro -> Bool
validaCurva (Peca (Curva Norte) h) c = y_ >= (-x_ + 1)
                                     where
                                     Just (Carro (x,y) o (vx,vy)) = c
                                     x_ = x - fromIntegral (floor x)
                                     y_ = y - fromIntegral (floor y)
validaCurva (Peca (Curva Sul)  h) c = y_ <= (-x_ + 1)
                                     where
                                     Just (Carro (x,y) o (vx,vy)) = c
                                     x_ = x - fromIntegral (floor x)
                                     y_ = y - fromIntegral (floor y)

validaCurva (Peca (Curva Este) h) c = y_ >= x_
                                     where
                                     Just (Carro (x,y) o (vx,vy)) = c
                                     x_ = x - fromIntegral (floor x)
                                     y_ = y - fromIntegral (floor y)

validaCurva (Peca (Curva Oeste) h) c = y_ <= x_
                                     where
                                     Just (Carro (x,y) o (vx,vy)) = c
                                     x_ = x - fromIntegral (floor x)
                                     y_ = y - fromIntegral (floor y)

{-| Reflete uma velocidade em relação ao tipo da Peca.-}
reflectCurva :: Peca -> Maybe Carro -> Maybe Carro
reflectCurva (Peca (Curva o) h) c |o==Norte || o==Sul   = reflectCurvaNS c
                                  |o==Este  || o==Oeste = reflectCurvaEO c

{-| Reflete a velocidade de uma curva com orientação Norte ou Sul.-}
reflectCurvaNS :: Maybe Carro -> Maybe Carro
reflectCurvaNS c =Just (Carro pos ori nVel)
                 where
                 Just (Carro pos ori vel) = c
                 normal = (sqrt(2)/2,sqrt(2)/2)
                 dP = dotProduct vel normal
                 nVel = add vel (mult normal ((-2)*dP))

{-| Reflete a velocidade de uma curva com orientação Este ou Oeste.-}
reflectCurvaEO :: Maybe Carro -> Maybe Carro
reflectCurvaEO c =Just (Carro pos ori nVel)
                where
                Just (Carro pos ori vel) = c
                normal = (-sqrt(2)/2,sqrt(2)/2)
                dP = dotProduct vel normal
                nVel = add vel (mult normal ((-2)*dP))

{-| Calcula o Produto de dois vetores.-}
dotProduct :: Ponto -> Ponto -> Double
dotProduct (x1,y1) (x2,y2) = (x1*x2) + (y1*y2)
