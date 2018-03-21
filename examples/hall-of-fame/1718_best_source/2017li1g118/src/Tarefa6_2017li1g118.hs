{-|
Module      : Tarefa6_2017li1g118
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g118 where

import Tarefa1_2017li1g118
import LI11718
import Mapas
import VectorLib
import Data.Fixed

type Percurso = [(Posicao,Peca,Orientacao)]
{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick (Jogo mapa props carros _ hist) j 
  = (Acao acel trav dir esq useNitro)
  where
  myCar = carros !! j
  myHist = hist !! j
  percurso = definePercurso mapa
  target = desiredPos myCar myHist percurso
  (acel,trav,selfNitro) = acaoAcelerar myCar props target
  (dir,esq)   = steering myCar target --guiador' myCar target

  mainSelfNitro = selfNitroMapa myCar props mapa target  
  useNitro
    |nitroAdv /= Nothing = nitroAdv
    |mainSelfNitro = Just j
    |otherwise = Nothing

  nitroAdv = nitroAdversarios carros indexCarrosAFrente props mapa

  indexCarrosAFrente = aFrente lugares j
  lugares = getPosicoes indexs 
  indexs = getIndexs carros hist percurso

bot2 tick jogo j
  = (Acao acel trav dir esq Nothing)
  where
  (Acao acel trav dir esq _) = bot tick jogo j

{-|Controla se o carro está a acelerar ou travar.-}
acaoAcelerar :: Carro -> Propriedades -> Ponto -> (Bool,Bool,Bool)
acaoAcelerar (Carro pos ang vel) props desiredPos
  |vDot vAng vel < 0 = (True,False,True)
  |diff <  0 = (False,True ,False)
  |diff >  (desVel/1.5) = (True,False,True)
  |diff >  0 = (True ,False,False)
  |otherwise = (False,False,False)
  where
  posToDesired = vAdd desiredPos (vMult pos (-1))  
  mag = vMag vel
  vAng = vFromAngle ang
  desVel = if vDot posToDesired vAng < 0 then mag
           else desiredVel props
  diff = desVel - mag

{-|Devolve a velocidade desejada dependendo das propriedades da pista.-}
desiredVel :: Propriedades -> Double
desiredVel (Propriedades atrito _ _ _ _ _)
  = 0.130*(atrito^2) - 0.025*atrito + 1.10

{-|Controla o guiador do carro.-}
steering :: Carro -> Ponto -> (Bool,Bool)
steering (Carro a ang vel) c
  |turn > 0 = (False,True)
  |turn < 0 = (True,False)
  |turn ==0 = (False,False)
  where
  b = vAdd a (vFromAngle (vDegreeToRad (-ang)))
  (abx,aby) = vAdd b (vMult a (-1))
  (acx,acy) = vAdd c (vMult a (-1))
  vAB3D = (realToFrac abx,realToFrac aby,0)
  vAC3D = (realToFrac acx,realToFrac acy,0)
  (_,_,turn) = vCrossProduct3by3 vAB3D vAC3D

{-|Controla algumas situações onde o bot deve usar nitro.-}
selfNitroMapa :: Carro -> Propriedades -> Mapa -> Ponto -> Bool
selfNitroMapa (Carro pos@(x,y) a vel) (Propriedades atr _ _ _ _ _) (Mapa _ tab) desiredPos@(dx,dy)
  |vDot posToDesired vel < 0 = False
  |h<0 = True
  |h>=0= False
  where
  isCurva :: Tipo -> Bool
  isCurva (Curva _) = True
  isCurva _ = False

  angC = vAngleBetween (vFromAngle (vDegreeToRad (-a))) posToDesired
  angVel = vAngleBetween vel posToDesired

  posToDesired = vAdd desiredPos (vMult pos (-1)) 
  (Peca t h) = (tab !! (floor y)) !! (floor x)
  (Peca td _) = (tab !! (floor dy) !! floor dx)

{-|Devolve a posição que o carro quer seguir.-}
desiredPos :: Carro -> [Posicao] -> Percurso -> Ponto
desiredPos (Carro (x,y) _ _) hist percurso
  = getDesiredPos index percurso
  where
  last3Pos = takeFirst 3 hist
  fPos = if hist == [] then (floor x, floor y)
         else hist !! 0
  percursos = partition fPos percurso
  ocurrenceIndex = mostElem last3Pos percursos
  index = searchPosIndex fPos percursoPos (ocurrenceIndex) 0
  (percursoPos,_,_) = unzip3 percurso

{-|Devolve os indices dos carros que estão à frente de um certo carro.-}
aFrente :: [Int] -> Int -> [Int]
aFrente xxs myCarid
  = aFrenteAc xxs (xxs !! myCarid) 1
  where
  aFrenteAc :: [Int] -> Int -> Int -> [Int] 
  aFrenteAc xxs carLug n
    = let i = elemIndice n xxs in
      if n == carLug then []
      else i : (aFrenteAc xxs carLug (n+1))

  elemIndice :: Eq a => a -> [a] -> Int
  elemIndice a (x:xs)
    |a==x = 0
    |a/=x = 1 + elemIndice a xs

{-|Controla as situações onde o bot deve utilizar nitro nos adversários.-}
nitroAdversarios :: [Carro] -> [Int] -> Propriedades -> Mapa -> Maybe Int
nitroAdversarios [] _ _ _ = Nothing
nitroAdversarios _ [] _ _ = Nothing
nitroAdversarios carros (i:is) props@(Propriedades atr _ _ _ _ _) map@(Mapa _ tab)
  = case t of
    (Curva _) -> if h>=0 && atr <1 then Just i
                 else nitroAdversarios carros is props map
    otherwise -> nitroAdversarios carros is props map
  where
  (Carro (x,y) _ _) = carros !! i
  (fx,fy) = (floor x, floor y)
  (Peca t h) = (tab !! fy) !! fx

{-|Obtem os lugares em que os carros se encontram através do
  indice da peca da pista em que se encontram.-}
getPosicoes :: [Int] -> [Int]
getPosicoes pos
  = getPosicoesAc pos (replicate (length pos) 0) 1
  where
  getPosicoesAc :: [Int] -> [Int] -> Int -> [Int]
  getPosicoesAc pos lugares n
    |pos == nPos = lugares
    |otherwise = getPosicoesAc nPos nLug (n+1)
    where
    nPos = swapAt index (-1) pos
    nLug = swapAt index n lugares
    index = maxIndex pos

  maxIndex :: [Int] -> Int
  maxIndex (l:ls)
    = maxIndexAc ls l 0 1
    where
    maxIndexAc [] _ maxi _ = maxi
    maxIndexAc (x:xs) max maxi n
      |x>max = maxIndexAc xs x n (n+1)
      |x<=max = maxIndexAc xs max maxi (n+1)

  swapAt :: Int -> a -> [a] -> [a]
  swapAt 0 a (x:xs) = (a:xs)
  swapAt n a (x:xs) = x : (swapAt (n-1) a xs)

{-|Obtem as posições (distância em peças relativamente ao inicio da pista)
  de todos os carros.-}
getIndexs :: [Carro] -> [[Posicao]] -> Percurso -> [Int]
getIndexs carros hists percurso
  = indexes
  where

  last3Poses = map (takeFirst 3) hists

  percursos = getPercursos posicoes
  (percursoPos,_,_) = unzip3 percurso

  posicoes  = getPosicoes carros hists

  ocurrenceIndexes = getOcurrenceIndexes last3Poses percursos
  indexes = getIndexes posicoes ocurrenceIndexes

  getIndexes :: [Posicao] -> [Int] -> [Int]
  getIndexes [] [] = []
  getIndexes (p:ps) (oi:ois)
    = (searchPosIndex p percursoPos oi 0):(getIndexes ps ois)

  getOcurrenceIndexes :: [[Posicao]] -> [[Percurso]] -> [Int]
  getOcurrenceIndexes [] [] = []
  getOcurrenceIndexes (h:hs) (p:ps)
    = (mostElem h p) : (getOcurrenceIndexes hs ps)

  getPercursos :: [Posicao] -> [[Percurso]]
  getPercursos [] = []
  getPercursos (p:ps) = (partition p percurso) : (getPercursos ps)

  getPosicoes :: [Carro] -> [[Posicao]] -> [Posicao]
  getPosicoes [] [] = []
  getPosicoes ((Carro (x,y) _ _):cs) (h:hs)
    = fPos : (getPosicoes cs hs)
    where
    fPos = if h == [] then (floor x, floor y)
           else h !! 0

{-|Função que testa se um carro passou mais de 2 peças à frente.-}
checkValidPos :: [Carro] -> [[Posicao]] -> Percurso -> [Bool]
checkValidPos carros hists percurso
  = checkValidPosH indicesAtuais indicesAnteriores
  where
  checkValidPosH :: [Int] -> [Int] -> [Bool]
  checkValidPosH (i_atual:iats) (i_ant:iants)
    |i_atual < i_ant = True : (checkValidPosH iats iants)
    |i_atual > i_ant + 2 = False : (checkValidPosH iats iants)
    |otherwise = True : (checkValidPosH iats iants)
  checkValidPosH [] [] = []

  indicesAtuais = getIndexs carros hists percurso
  indicesAnteriores = getIndexs carros (map mytail hists) percurso
  mytail :: [a] -> [a]
  mytail [] = []
  mytail (x:xs) = xs

{-|Devolve a posição que o carro quer seguir mas 
  recebe o indice de onde o carro se encontra na pista.-}
getDesiredPos :: Int -> Percurso -> Ponto
getDesiredPos i percurso
  = case (tipoAtual,oriAtual,proxTipo,proxOri) of
      (Curva Este,Sul,Recta,_) -> (fx+0.20, fy+1.20)
      (Curva Sul,Oeste,Recta,_)->(fx-0.20, fy+0.20)
      (Curva Oeste,Norte,Recta,_)->(fx+0.80, fy-0.20)
      (Curva Norte,Este,Recta,_)->(fx+1.20, fy+0.80)

      (_,Este,Curva Este,_) -> (fx+1.20 , fy+0.80)
      (_,Este,Curva Sul,_) -> (fx+1.20 , fy+0.20)

      (_,Sul,Curva Sul,_) -> (fx+0.20 , fy+1.20)
      (_,Sul,Curva Oeste,_) -> (fx+0.80 , fy+1.20)

      (_,Oeste,Curva Oeste,_) -> (fx-0.20 , fy+0.20)
      (_,Oeste,Curva Norte,_) -> (fx-0.20 , fy+0.80)

      (_,Norte,Curva Norte,_) -> (fx+0.80 , fy-0.20)
      (_,Norte,Curva Este,_) -> (fx+0.20 , fy-0.20)

      (_,Norte,_,_) -> (fromIntegral x + 0.5 ,fromIntegral y - 1.1)
      (_,Sul,_,_)   -> (fromIntegral x + 0.5 ,fromIntegral y + 1.1)
      (_,Este,_,_)  -> (fromIntegral x + 1.1 ,fromIntegral y + 0.5)
      (_,Oeste,_,_) -> (fromIntegral x - 1.1 ,fromIntegral y + 0.5)
  where
  (fx,fy) = (fromIntegral x,fromIntegral y)
  atual@((x,y),Peca tipoAtual _,oriAtual) = percurso !! i
  proxima@(_,Peca proxTipo _,proxOri) = percurso !! indexProxPeca

  indexProxPeca = limit (i+1) maxIndexPercurso
  maxIndexPercurso = (length percurso) - 1

{-|Quando um valor ultrapassa um máximo é-lhe subtraído o máximo-}
limit :: Int -> Int -> Int
limit index maxIndex
  |index>maxIndex = index-maxIndex
  |otherwise = index

{-|Pocura o índice de uma posição numa lista de posições
  sendo que recebe a posição, a lista de posições e
  quantas vezes a posição ocorreu antes.-}
searchPosIndex :: Posicao -> [Posicao] -> Int -> Int -> Int
searchPosIndex _ [] _ _ = 0
searchPosIndex pc (p:ps) i n
  = case (i,pc==p) of
    (_,False) -> searchPosIndex pc ps i (n+1)
    (0,True ) -> n
    otherwise -> searchPosIndex pc ps (i-1) (n+1)

{-|Retira os primeiros n elementos do inicio de uma lista.-}
takeFirst :: Int -> [a] -> [a]
takeFirst 0 _  = []
takeFirst _ [] = []
takeFirst n (l:ls) = l : (takeFirst (n-1) ls)

{-|Retira os primeiros n elementos do fim de uma lista.-}
takeLast :: Int -> [a] -> [a]
takeLast n l
  = takeLastAc (length l - n) l
  where
  takeLastAc n [] = []
  takeLastAc n (l:ls)
    |n <= 0  = l : (takeLastAc (n-1) ls)
    |n >  0  = takeLastAc (n-1) ls

{-|Separa um percurso em partes em que uma certa posição se repete.-}
partition :: Posicao -> Percurso -> [Percurso]
partition pos l
  = reverse (partitionAc pos l [[]] False)
  where
  partitionAc pos [] ac _ = ac
  partitionAc pos (l:ls) ac b
    |b = partitionAc pos ls ([l]:ac) newB
    |otherwise = partitionAc pos ls (addToFront l ac) newB
    where
    (pPos,_,_) = l
    newB = pPos == pos

{-|Devolve a parte do percurso que têm mais posições compativeis com
  uma lista de posições.-}
mostElem :: [Posicao] -> [Percurso] -> Int
mostElem hist percursos
  = mostElemAc hist percursos 0 0 0
  where
  mostElemAc hist [] maxi maxElem i = maxi
  mostElemAc hist (percurso:percursos) maxi maxElem i
    |numElem > maxElem = mostElemAc hist percursos i numElem (i+1)
    |otherwise = mostElemAc hist percursos maxi maxElem (i+1)
    where
    numElem = elemAmount hist posPercurso
    (posPercurso,_,_) = unzip3 percurso

  elemAmount :: [Posicao] -> [Posicao] -> Int
  elemAmount [] y = 0
  elemAmount (x:xs) y
    |elem x y  = 1 + elemAmount xs y
    |otherwise = 0 + elemAmount xs y

{-|Adiciona um elemento à primeira lista de uma lista de listas.-}
addToFront :: a -> [[a]] -> [[a]]
addToFront a (l:ls)
  = ((l ++ [a]):ls)

{-|Através de um mapa define um percurso.-}
definePercurso :: Mapa -> Percurso
definePercurso (Mapa (pos,ori) tab)
  = definePercursoH (pos,primeiraPeca,nOri) tab pos 0
  where
  nOri = proximaOrientacao ori primeiraPeca
  primeiraPeca = pecaAt pos tab
  definePercursoH :: (Posicao,Peca,Orientacao) -> Tabuleiro -> Posicao -> Int -> Percurso
  definePercursoH a@(pos,peca,ori) tab posInicial n
    |pos == posInicial && n/=0 = []
    |otherwise = a : definePercursoH nA tab posInicial (n+1)
    where
    nA = proximaPeca a tab

{-|Obtém a próxima peça do percurso.-}
proximaPeca :: (Posicao,Peca,Orientacao) -> Tabuleiro -> (Posicao,Peca,Orientacao)
proximaPeca (pos,peca,ori) tab
  = (nPos,nPeca,nOri)
  where
  nOri = proximaOrientacao ori nPeca
  nPos = proximaPosicao pos ori
  nPeca= pecaAt nPos tab


{-|Dada a orientacao de entrada de uma peca devolve a orientacao de saida.-}
proximaOrientacao :: Orientacao -> Peca -> Orientacao
proximaOrientacao ori (Peca tipo _)
  = case tipo of
    (Curva pOri) -> if pOri == ori then rodaOriDir ori
                    else rodaOriEsq ori
    (Lava) -> error ""
    otherwise -> ori

{-|Obtém a próxima posição dada uma posição e uma orientação.-}
proximaPosicao :: Posicao -> Orientacao -> Posicao
proximaPosicao (x,y) ori
  = case ori of
    Sul   -> (x,y+1)
    Norte -> (x,y-1)
    Este  -> (x+1,y)
    Oeste -> (x-1,y)

{-|Devolve a peça numa certa posição.-}
pecaAt :: Posicao -> Tabuleiro -> Peca
pecaAt (x,y) tab
  = (tab !! y) !! x

