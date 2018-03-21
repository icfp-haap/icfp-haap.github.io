{-|
Module      : Tarefa6_2017li1g71
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g71 where
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import Tarefa3_2017li1g71
import Tarefa4_2017li1g71
import Mapas
import LI11718
import Data.List

-- | A Ação viraDir é uma ação base para apenas virar à direita
viraDir::Acao
viraDir = (Acao False False False True Nothing)
-- | A Ação viraEsq é uma ação base para apenas virar à direita
viraEsq::Acao
viraEsq = (Acao False False True False Nothing)
-- | Para testar se um dado angulo está acima da reta com uma dada inclinação
above::Double->Double->Bool
above angC angP = if y>=(tan angP)*x
                  then True
                  else False
                where (x,y) = polarToCart (1,angC)

-- | Função para saber em que quadrante se encontra um dado angulo
whichQuad::Double->Int
whichQuad ang | x>=0 && y>=0 = 1
              | x>=0 && y<=0 = 4
              | x<=0 && y>=0 = 2
              | x<=0 && y<=0 = 3
              where (x,y) = polarToCart (1,ang) 
{- | A Função aproxima dá a ação a tomar para aproximar um dado
angulo ao valor que se pretende para depois efectuar os movimentos correspondentes-}
aproxima::Double->Double->Acao
aproxima angC angP = if notElem (reduz angP) [pi/2,(3*pi)/2]
                      then if  reduz angC - reduz angP == 0 
                           then (Acao False False False False Nothing)
                           else if elem (whichQuad angP) [1,4]
                                then if above angC angP
                                     then viraDir
                                     else viraEsq
                                else if above angC angP
                                     then viraEsq
                                     else viraDir
                      else if angP == (pi/2)
                           then  case quad of
                                 2 -> viraDir
                                 3 -> viraDir
                                 _ -> viraEsq 
                           else   case quad of
                                   1 -> viraDir
                                   4 -> viraDir
                                   _ -> viraEsq 
                    where quad = whichQuad angC                 
{- | A Função posAng dá a melhor direção que o carro deve ter numa dada
peça, para isso recebe a orientação de entrada na peça, a peça em que está e a peça
em que vai entrar -}
posAng::Propriedades->Orientacao->Peca->Peca->Angulo
posAng _ ori p0@(Peca (Curva o) _) p1= if (tipoEspaco p1 == Metade) && (ori == (proxOri (proxOri ori p0) p1))
                                  then case o of
                                       Norte -> if ori == Norte
                                                then (pi/4)
                                                else ((5*pi)/4)
                                       Sul -> if ori==Sul
                                              then ((5*pi)/4)
                                              else (pi/4)
                                       Este -> if ori==Este
                                               then ((7*pi)/4)
                                               else ((3*pi)/4)
                                       Oeste -> if ori==Oeste
                                                then ((3*pi)/4)
                                                else ((7*pi)/4)
                                  else case o of
                                       Norte -> if ori == Norte
                                                then 0
                                                else ((3*pi)/2)
                                       Sul -> if ori==Sul
                                              then pi
                                              else (pi/2)
                                       Este -> if ori==Este
                                               then ((3*pi)/2)
                                               else pi
                                       Oeste -> if ori==Oeste
                                                then (pi/2)
                                                else 0
                                  
                                   
posAng pro ori p0 p1 = if pro /= asfalto
                       then if (tipoEspaco p0==Completo) && (tipoEspaco p1==Metade) && (getTipo p0==Recta)
                            then case getTipo p1 of
                                 (Curva o) -> case o of
                                              Norte -> if ori==Norte
                                                       then ((7*pi)/18)
                                                       else ((10*pi)/9)
                                              Sul -> if ori==Sul
                                                     then ((25*pi)/18)
                                                     else (pi/9)
                                              Este -> if ori == Este
                                                      then ((17*pi)/9)
                                                      else ((11*pi)/18)
                                              Oeste -> if ori == Oeste
                                                       then ((8*pi)/9)
                                                       else ((29*pi)/18)
                                 _ -> case ori of
                                      Norte -> (pi/2)
                                      Sul -> ((3*pi)/2)
                                      Este -> 0
                                      Oeste -> pi
                            else case ori of
                                 Norte -> (pi/2)
                                 Sul -> ((3*pi)/2)
                                 Este -> 0
                                 Oeste -> pi
                       else if (tipoEspaco p0==Completo) && (tipoEspaco p1==Metade) && (getTipo p0==Recta)
                            then case getTipo p1 of
                                 (Curva o) -> case o of
                                              Norte -> if ori==Norte
                                                       then (pi/3)
                                                       else ((7*pi)/6)
                                              Sul -> if ori==Sul
                                                     then ((4*pi)/3)
                                                     else (pi/6)
                                              Este -> if ori == Este
                                                      then ((11*pi)/6)
                                                      else ((2*pi)/3)
                                              Oeste -> if ori == Oeste
                                                       then ((5*pi)/6)
                                                       else ((5*pi)/3)
                                 _ -> case ori of
                                      Norte -> (pi/2)
                                      Sul -> ((3*pi)/2)
                                      Este -> 0
                                      Oeste -> pi
                            else case ori of
                                 Norte -> (pi/2)
                                 Sul -> ((3*pi)/2)
                                 Este -> 0
                                 Oeste -> pi

-- | A função poeZero põe uma mapa todo com a altura zero e substitui as rampas por retas chamando a função zero
poeZero::Tabuleiro->Tabuleiro
poeZero = (map zero)
-- | função que tranforma uma lista de peças numa outra lista de peças com todas elas a 0 de altura e as rampas como sendo rectas
zero::[Peca]->[Peca]
zero [] = []
zero ((Peca tipo alt):t) = case tipo of
                           (Rampa o) -> (Peca Recta 0) : zero t
                           _ -> (Peca tipo 0): zero t
{- | A Função listaAng devolve a lista de angulos que a direção do
    bot deve ter tendo em conta o tabuleiro, a posição inicial, a orientação
    inicial e a lista de posições percorridas para o percurso-}
listaAng::Tabuleiro->Posicao->Orientacao->[Posicao]->(Orientacao->Peca->Peca->Angulo)->[Angulo]
listaAng _ _ _ [] f= []
listaAng tab posi oi [h] f= [f oi (getPeca tab h) (getPeca tab posi)]
listaAng tab posi oi (h:x:t) f= (f oi pA pP):(listaAng tab posi (proxOri oi pA) (x:t) f)
                          where pA = getPeca tab h
                                pP = getPeca tab x
-- | A Função limV controla a aceleração do carro dependendo se o carro vai morrer ou não num dado intervalo de tempo
limV::Mapa->Propriedades->Tempo->Carro->Acao->Acao
limV m@(Mapa (pI,oi) tab) p t c ac@(Acao a tr e d n) = if movimenta (getTabul m) t c == Nothing 
                                                       then if p==gelo
                                                            then if mV>1
                                                                 then (Acao False True e d n)
                                                                 else (Acao False False e d n)
                                                            else if mV > 2
                                                                 then (Acao False True e d n)
                                                                 else (Acao False False e d n)
                                                       else (Acao True False e d n)
                                                where (mV,ang) = cartToPolar (velocidade c)

-- | A função vMin controla o módulo da velocidade para que este não diminua nem aumente demasiado                              
vMin::Tabuleiro->Angulo->Acao->Carro->Propriedades->(Double,Bool)->Acao
vMin tab angP (Acao a t e d n) c pro (lV,b) =  if b
                                               then if mV > lV
                                                    then (Acao False True e d n)
                                                    else if mV <= 0.4
                                                         then (Acao True False e d n)
                                                         else if mV <= ((k_peso pro)*5) && 
                                                                 (tipoEspaco (whichPeca (posicao c) tab)==Completo) && 
                                                                 (mVp/=0) && 
                                                                 (angA/=angP)
                                                              then (Acao True False e d n)
                                                              else (Acao a t e d n)
                                               else if mV <= 0.4
                                                    then (Acao True False e d n)
                                                    else if mV <= ((k_peso pro)*5) && 
                                                            (tipoEspaco (whichPeca (posicao c) tab)==Completo) && 
                                                            (mVp/=0) && 
                                                            (angA/=angP)
                                                         then (Acao True False e d n)
                                                         else (Acao a t e d n)
                                       where (mV,ang) = cartToPolar (velocidade c)
                                             (mVp,angA) = cartToPolar (vPeso tab c (k_peso pro) 0.1)
                                             p =  whichPeca (posicao c) tab
                                             pP = getPeca tab (move (pontoPos(posicao c)) (toOri angP))

centra::Angulo->Ponto->Angulo
centra ang (x,y) = case toOri ang of
                    Norte -> if x > toEnum (floor x)+0.7
                             then (2*pi)/3
                             else if x < toEnum (floor x)+0.3
                                  then (pi/3)
                                  else ang
                    Sul -> if x > toEnum (floor x)+0.7
                             then (4*pi)/3
                             else if x < toEnum (floor x)+0.3
                                  then ((5*pi)/3)
                                  else ang
                    Este -> if y > toEnum (floor y)+0.7
                            then (pi/6)
                            else if y < toEnum (floor y)+0.3
                                 then ((11*pi)/6)
                                 else ang
                    Oeste -> if y > toEnum (floor y)+0.7
                             then ((5*pi)/6)
                             else if y < toEnum (floor y)+0.3
                                  then ((7*pi)/6)
                                  else ang
centrado::Espaco->Ponto->Posicao->Orientacao->Bool
centrado Metade _ _ _= True
centrado Completo (cx,cy) (px,py) o = case o of
                                       Norte -> entre (pX+0.3,pX+0.7) cx
                                       Sul -> entre (pX+0.3,pX+0.7) cx
                                       _ -> entre (pY+0.3,pY+0.7) cy
                                     where (pX,pY) = (toEnum px,toEnum py)                  
-- | A função que dado um angulo o tranforma na sua respetiva orientação
toOri :: Angulo -> Orientacao
toOri a = if a == (pi/2)
          then Norte
          else if a==0
               then Este
               else if a==pi
                    then Oeste
                    else Sul
-- | A função getP devolve a posição inicial de uma mapa
getP::Mapa->Posicao
getP (Mapa ((x,y),_) _) = (x,y)

-- | Função que diz se um dado valor está entre os valores do par, sendo o primeiro o minimo e o segundo o maximo
entre::Ord a=>(a,a)->a->Bool
entre (x,y) z = (z>=x && z<=y)

-- | Tipo que serve apenas para facilitar operações ao histórico
type Historico = [[Posicao]]
-- | Função para obter a primeira ocorrencia de um dado elemento numa lista
obtI::Eq a=>a->[a]->Int
obtI x [] = 0
obtI x (h:t) = if x == h
               then 0
               else 1 + obtI x t
-- | A Função fromListtoJ transforma a lista de jogadores criada para saber quais as posições relativas entre os carros nos seu valores normais
fromListtoJ::Int->Int->Int
fromListtoJ 1 nj = (nj - 1)
fromListtoJ n nj = (-1)+(fromListtoJ (n-1) nj)
-- | A função fLtJ transforma iuma lista de elementos criada para saber quais as posições relativas entre os carros nos seu valores normais
fLtJ::[Int]->Int->[Int]
fLtJ [] _ = []
fLtJ (h:t) nj = fromListtoJ h nj : fLtJ t nj
-- | A funçao findFirst diz qual o elemento que vai na frente da corrida se este for diferente do jogador, senão, dá o 2º
findFirst::Historico->Mapa->Int->Int->Int
findFirst h m@(Mapa (pI,oi) tab) j nj= pE
                                   where pE = if fromListtoJ(head lPelasP) nj== j
                                              then fromListtoJ(lPelasP!!1) nj
                                              else fromListtoJ(head lPelasP) nj
                                         listaO = (getOrdem nj h (guardaPosVal m ((getPeca tab pI),pI) oi))
                                         lPelasP = snd (unzip (ordJogo listaO))
                         
-- | A função daNitro aplica nitro ao carro que vai em primeiro se este morrer com o nitro dado
daNitro::Jogo->Int->Int->Acao->Tempo->[Posicao]->Acao
daNitro jog j jA ac@(Acao a t e d n) tem lPos = if movimenta tab 0.6 (carroJn) /= Nothing &&
                                                   (tempoN (0.6) j nit)>0 &&
                                                   fst ((getOrdem (j+1) (historico jog) lPos)!!0) < fst ((getOrdem (j+1) [[(pontoPos (posicao carroJn))]] lPos)!!0) &&
                                                   (not ((tipoEspaco (whichPeca (posicao ((carros jog)!!j)) tab))==Metade))
                                                then (Acao a t e d (Just j))
                                                else if movimenta tab tM carroCn == Nothing && tM > 0 && mVa (getVel carroJ) > 0 
                                                                 then (Acao a t e d (Just jA))
                                                                 else (Acao a t e d Nothing)
                                              where carroJ = (carros jog)!!jA
                                                    tM = tempoN (tem) j nit
                                                    carroCn = adicionaV carroJ (polarToCart(fN*tM,toRadian(direcao carroJ)))
                                                    carroJn = adicionaV ((carros jog)!!j) (polarToCart(fN*tM,toRadian(direcao ((carros jog)!!j))))
                                                    fN = (k_nitro (pista jog))
                                                    nit = (nitros jog)
                                                    mVa (vx,vy) = sqrt (vx^2+vy^2)
                                                    tab = (getTabul (mapa jog))

-- | função que converte pontos em posições
pontoPos::Ponto->Posicao
pontoPos (x,y) = (floor x,floor y)                                    
-- | função que calcula em que indice aparece uma determinada ocorrencia de um elemento
obP::Eq a=>Int->a->[a]->Int
obP n x (h:t) | (n==1) = obtI x (h:t)
              | (n==0) = 1
              | otherwise = ((obtI x (h:t))+1) + obP (n-1) x (drop ((obtI x (h:t))+1) (h:t))
-- | função que conta quantas vezes um dado elemento aparece numa lista
numO::Eq a=>a->[a]->Int
numO x [] = 0
numO x (h:t) | x==h = 1 + numO x t
             | otherwise = numO x t
-- | A função getOrdem calcula o quão "longe" no percurso um certo jogador está, obtendo uma lista com o primeiro elemento sendo a "posição relativa" e o segundo o identificador do jogador
getOrdem::Int->[[Posicao]]->[Posicao]->[(Int,Int)]
getOrdem 1 (h:t) l = [(pos,1)]
                   where posR =  (head h)
                         lH =  (numO posR h)
                         pos = if null h
                               then 1
                               else if (numO posR h)>(numO posR l)
                                    then (obP (numO posR l) posR l)
                                    else obP lH posR l
getOrdem n (h:t) l = (pos,n) : getOrdem (n-1) t l
                   where posR =  (head h)
                         lH =  (numO posR h)
                         pos = if null h
                               then 1
                               else if (numO posR h)>(numO posR l)
                                    then (obP (numO posR l) posR l)
                                    else obP lH posR l
-- | função que insere um par de elementos ordenados e o insere ordenamente numa lista de pares ordenadas segundo o primeiro elemento
insertPar::Ord a=>(a,b)->[(a,b)]->[(a,b)]
insertPar (x,y) [] = [(x,y)]
insertPar (x,y) ((a,b):t) | x >= a = (x,y):((a,b):t)
                          | otherwise = (a,b) : insertPar (x,y) t
-- | função que ordena uma lista de pares segundo o primeiro elemento
ordJogo::[(Int,Int)]->[(Int,Int)]
ordJogo [] = []
ordJogo ((a,b):t) = (insertPar (a,b) (ordJogo t))
-- | função que testa se todos os elementos de uma lista são iguais
nViguais::[Int]->Bool
nViguais [h] = True
nViguais (h:t) = elem h t && nViguais t
juntaVoltas::[Int]->[Int]->[(Int,Int)]
juntaVoltas [] _ = []
juntaVoltas (h:t) nV = (nV!!h,h) : juntaVoltas t nV 
-- | A função ordemDoJogo ordena a lista de posições tendo em conta o quão longe vão no percurso, e também, o número de voltas dadas por cada carro à pista
ordemDoJogo::[Int]->[Int]->[Int]
ordemDoJogo [] _  = []
ordemDoJogo lPos nV = if nViguais nV
                      then lPos
                      else novaOrdem
                    where novaOrdem = snd (unzip (ordJogo (juntaVoltas lPos nV)))
                         


{-guardaPosVal  Temos de dar a mapa,peça,posicao e orientacao e dá a lista de posiçoes-}
{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado a tual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao  -- ^ a decisão tomada pelo /bot/
bot tick jog@(Jogo m@(Mapa (pI,oi) tab) pro@(Propriedades fAtri fPn fAcel fPe fN fR) cars n h) j = acaoN
                                                                                         where posNor = (guardaPosVal m ((getPeca tab pI),pI) oi) -- Lista de posições do caminho 
                                                                                               angulosS = (listaAng tab pI oi posNor (posAng pro)) -- Lista dos angulos 
                                                                                               histo = reverse (h!!j) -- histórico do jogador
                                                                                               angPeca = if (numO posA histo) > (numO posA posNor)
                                                                                                         then angulosS!!(obP (numO posA posNor) posA posNor)
                                                                                                         else angulosS!!(obP (numO posA histo) posA posNor)
                                                                                               pP = if (numO posA histo) > (numO posA posNor)
                                                                                                         then posNor!!((obP (numO posA posNor) posA posNor)+1)
                                                                                                         else posNor!!((obP (numO posA histo) posA posNor)+1)
                                                                                               oriP = toOri angPeca
                                                                                               dir = -reduz(toRadian (direcao carroJ))
                                                                                               acaoA = aproxima dir angPeca
                                                                                               acaoC = aproxima dir (centra angPeca (posicao carroJ))
                                                                                               acaoF = if centrado (tipoEspaco pA) (posicao carroJ) posA (toOri angPeca) || tipoEspaco(getPeca tab pP)==Metade
                                                                                                       then if pro/= gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoA) carroJ pro  (9,False)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoA) carroJ pro  (9,False)
                                                                                                       else if pro/=gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoC) carroJ pro  (9,False)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoC) carroJ pro  (9,False)
                                                                                               carroJ = cars!!j
                                                                                               pA =  (whichPeca (posicao carroJ) tab)
                                                                                               posA = pontoPos (posicao carroJ)
                                                                                               acaoN =  if null histo
                                                                                                        then acaoF
                                                                                                        else if centrado (tipoEspaco pA) (posicao carroJ) posA (toOri angPeca)
                                                                                                             then daNitro jog j (findFirst h m j (length cars)) acaoF 0.18 posNor
                                                                                                             else acaoF
          


