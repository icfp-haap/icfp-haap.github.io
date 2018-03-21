{-|
Module      : Tarefa4_2017li1g118
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g118 
(testesT4,atualiza,addToLista,calcularNitrosAcoes,calcularFNitro,calculaTempoNitro,calcularAtrito,calcularAngulo,calcularNitros,calcularFPneus,calcularAceleracao,calcularPeso)
where

import LI11718
import VectorLib
import TestesT4


{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = testesT4'

debug :: (Tempo,Jogo,Acao) -> Jogo
debug (t,j,a) = atualiza t j 0 a
{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t (Jogo map pist cars nits hists) j a
  = (Jogo map pist nCars nNits nHists)
  where
  car  = cars !! j
  nit  = nits !! j
  hist = hists !! j
  aReceberNitro = lNitros !! j
  lNitros = calcularNitros a nits (replicate (length nits) False)
  (nCar,nNit,nHist) = updateCarro map pist a car nit t hist aReceberNitro
  (nCars,nNits,nHists) = (swapAt j nCar cars,
                          swapAt j nNit nits,
                          swapAt j nHist hists
                          )

{-Atualiza a posição e a velocidade de um carro.-}
updateCarro :: Mapa -> Propriedades -> Acao -> Carro -> Tempo -> Tempo -> [Posicao]
               -> Bool -- Verifica se o carro está a receber nitro
               -> (Carro, Tempo, [Posicao])
updateCarro m p a c tNitro tStep hist n
  = ((Carro (x,y) nDirecao novaVel), nTNitro, nListaPos)
  where
  Propriedades atr pen ace pes nit rod = p
  (Carro (x,y) ang vel) = c
  novaVel     = vAdd vel somaForcas
  somaForcas  = (vAddList [fAtrito,fAcelaracao,fGravidade,fPneus,fNitro])
  fAtrito     = calcularAtrito atr c tStep
  fAcelaracao = calcularAceleracao a c ace tStep
  fGravidade  = calcularPeso m c pes tStep
  fPneus      = calcularFPneus c pen tStep
  fNitro      = calcularFNitro c n nit tStep
  nDirecao    = calcularAngulo a c rod tStep
  nTNitro     = calculaTempoNitro a tNitro tStep
  posicao     = (floor x,floor y)
  nListaPos   = addToLista posicao hist

{-|Adiciona uma posicao a uma lista de posicões se a cabeça desta for diferente
à posição a adicionar.-}
addToLista :: Posicao -> [Posicao] -> [Posicao]
addToLista (a,b) [] = [(a,b)]
addToLista pos1 xxs@(pos2:_)
  |pos1 == pos2 = xxs
  |otherwise    = pos1:xxs

{-|Função que devolve a força aplicada pelo atrito.-}
calcularAtrito :: Double -> Carro -> Tempo -> Vetor
calcularAtrito a (Carro _ _ v) t 
  = vMult v (-a/1*t)

{-|Função que devolve o novo angulo para o qual o carro está virado.-}
calcularAngulo :: Acao -> Carro -> Double -> Tempo -> Angulo
calcularAngulo (Acao _ _ kE  kD  _) (Carro _ a _) rod t
  = case (kE,kD) of
    (True,False) -> a + (rod*t)
    (False,True) -> a - (rod*t)
    otherwise    -> a

{-|Função que devolve a força exercida pela aceleração do carro.-}
calcularAceleracao :: Acao -> Carro -> Double -> Tempo -> Vetor
calcularAceleracao (Acao kA kT _ _ _) (Carro _ a v) ace t
  = case (kA,kT) of
    (True,False) -> vMult (vFromAngle (-a*pi/180)) (ace*t)
    (False,True) -> vMult (vFromAngle (-a*pi/180)) (-ace*t)
    otherwise    -> (0,0)

{-|Função que devolve a força exercida pelos pneus do carro.-}
calcularFPneus :: Carro -> Double -> Tempo -> Vetor
calcularFPneus (Carro _ a (0,0)) _ _ = (0,0)
calcularFPneus (Carro _ a v) pen t
  |vDot fPneus v > 0 = vMult fPneus (-norma*t)
  |otherwise         = vMult fPneus (norma*t)
  where
  fPneus = vPerpendicularFromAng (vDegreeToRad (-a))
  norma  = (sin ang) * pen * (vMag v)
  ang    = vAngleBetween v aVetor
  aVetor = vFromAngle (-a*pi/180)

{-|Calcula os carros que estão a receber nitro.
  i.e se de dois carros o que tem indice 1 é o único a receber nitro devolve [True,False]-}
calcularNitros :: Acao -> [Tempo] -> [Bool] -> [Bool]
calcularNitros a [] b = b
calcularNitros a (t:ts) b
  = calcularNitros a ts nB
  where
  nB = nitroToBool a t b

{-|Faz o mesmo que calcularNitros no entanto recebe ações de todos os jogadores.-}
calcularNitrosAcoes :: [Acao] -> [Tempo] -> [Bool] -> [Bool]
calcularNitrosAcoes [] [] b = b
calcularNitrosAcoes (a:as) (0:ts) b = b
calcularNitrosAcoes (a:as) (t:ts) b
  = calcularNitrosAcoes as ts nB
  where
  nB = nitroToBool a t b

{-|Função que devolve um [Bool] que representa se um carro está a receber nitro ou não.-}
nitroToBool :: Acao ->  Tempo -> [Bool] -> [Bool]
nitroToBool _ 0 bools = bools
nitroToBool (Acao _ _ _ _ Nothing) _ bools = bools
nitroToBool (Acao _ _ _ _ (Just n)) _ bools
  = nitroToBoolH 0 n bools
  where
  nitroToBoolH x n [] = []
  nitroToBoolH x n (bool:bools)
    |x==n = True : bools
    |otherwise = bool : (nitroToBoolH (x+1) n bools)

{-|Função que calcula a força do nitro aplicada a um carro.-}
calcularFNitro :: Carro -> Bool -> Double -> Tempo -> Vetor
calcularFNitro _ False _ _
  = (0,0)
calcularFNitro (Carro _ ang _) True nit t
  = let vec = vFromAngle (vDegreeToRad (-ang)) in
    vSetMag (nit*t) vec

{-|Calcular a nova quantidade de nitro que um jogador terá no próximo frame.-}
calculaTempoNitro :: Acao -> Tempo -> Tempo -> Tempo
calculaTempoNitro (Acao _ _ _ _ Nothing) tNitro _ = tNitro
calculaTempoNitro _ tNitro tStep |tNitro>0  = tNitro - tStep
                                 |otherwise = 0

{-|Calcula o peso que está a ser aplicado no carro.-}
calcularPeso :: Mapa -> Carro -> Double -> Tempo -> Vetor
calcularPeso (Mapa _ tab) (Carro (x,y) _ _) grav t
  = vMult peso t
  where
  --Função que calcula o peso do carro
  peso = vMult (pecaToVet peca) grav
  peca = obterPeca (floor x, floor y) tab
  --Função que devolve a peça em que o carro se encontra.
  obterPeca (x,y) tab = ((tab !! y) !! x)
  --Função que devolve o vetor normalizado da gravidade para uma certa peça.
  pecaToVet :: Peca -> Vetor 
  pecaToVet (Peca (Rampa o) _)
    = gravOri o
    where
    gravOri Sul   = ( 0,-1)
    gravOri Norte = ( 0, 1)
    gravOri Este  = (-1, 0)
    gravOri Oeste = ( 1, 0)
  pecaToVet _ = (0,0)

{-|Troca um elemento em um certo índice numa lista por um outro elemento indicado.-}
swapAt :: Int -> a -> [a] -> [a]
swapAt 0 n (x:xs) = n:xs
swapAt _ _ []     = error "Index maior que o tamanho da lista."
swapAt i n (x:xs) = x:(swapAt (i-1) n xs)