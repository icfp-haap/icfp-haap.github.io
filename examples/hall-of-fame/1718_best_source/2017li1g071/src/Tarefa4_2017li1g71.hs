{-|
Module      : Tarefa4_2017li1g71
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g71 where
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import Tarefa3_2017li1g71
import Mapas
import LI11718

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(1,fazTestesT4 carro0 terra,(Acao True False False False Nothing)),
            (1,fazTestesT4 carro1 asfalto,(Acao True False False False Nothing)),
            (1,juntaCarros (fazTestesT4 carro0 terra) carro1,(Acao True False True False (Just 1))),
            (5,fazTestesT4 carro1 gelo,(Acao False True False True Nothing)),
            (3,fazTestesT4 carro2 gelo,(Acao True False False False (Just 0))),
            (3,fazTestesT4 carro1 asfalto,(Acao True False False False (Just 0))),
            (4,juntaCarros (juntaCarros (fazTestesT4 carro0 terra) carro1) carro2 ,(Acao False True False True (Just 2))),
            (4,juntaCarros (juntaCarros (fazTestesT4 carro0 gelo) carro1) carro2 ,(Acao False True False True Nothing)),
            (4,juntaCarros (juntaCarros (fazTestesT4 carro0 asfalto) carro1) carro2 ,(Acao False True False True Nothing)),
            (1,juntaCarros (fazTestesT4 carro0 asfalto) carro1,(Acao True False True False (Just 1)))] 
-- | A função toRadian recebeu um grau e transforma-o no seu simétrico em radianos
toRadian::Double->Double
toRadian x = -(x*pi)/180
-- | A função polarToCart passa coordenadas cartesianas para polares
cartToPolar::(Double,Double)->(Double,Double)
cartToPolar (0,0) = (0,0)
cartToPolar (x,y) | (x>=0)&&(y>=0) = (r,acos (x/r))
                  | (x<=0)&&(y<=0) = (r,pi+(acos (-x/r)))
                  | (x<=0)&&(y>=0) = (r,acos (x/r))
                  | (x>=0)&&(y<=0) = (r,pi+(acos (-x/r)))
                  where r = sqrt (x^2+y^2)
-- | A função polarToCart passa coordenadas polares para cartesianas 
polarToCart::(Double,Double)->(Double,Double)
polarToCart (r,a) = (r*cos a,r*sin a)


----------------------------------------------
--                                          --
-- NITROS                                   --
--                                          --
----------------------------------------------
-- | A função getVel devolve a velocidade de um dado carro
getVel::Carro->Velocidade
getVel (Carro _ _ v) = v
-------------------------------------------------------------------------------------------------------
-- | Função que calcula o tempo durante o qual o nitro vai estar ativo
tempoN::Tempo->Int->[Tempo]->Tempo
tempoN t 0 (ht:tt) | t > ht = ht
                   | ht-t >= 0 = t
                   | ht<=0 = 0
tempoN t n (ht:tt) = tempoN t (n-1) tt
-- | A função ativaNitro dá a lista de velocidades a somar aos carros após o nitro
ativaNitro::Int->[Carro]->Double->Tempo->[Velocidade] -- Velocidade Nitro
ativaNitro 0 ((Carro p d v):tc) fN t = polarToCart (t*fN,dir) : (replicate (length tc) (0,0))
                                     where dir = toRadian d
ativaNitro n (c:tc) fN t = (0,0):ativaNitro (n-1) tc fN t
-- | A função atualizaNitro tem como objetivo atualizar a quantidade de nitro disponivel para cada carro
atualizaNitro::Int->Tempo->[Tempo]->[Tempo] -- Tempo Nitro
atualizaNitro i t lt = changeListElem (lt!!i - tempoN t i lt) i lt 

-------------------------------------------------------------------------------------------------------
----------------------------------------------
--                                          --
-- VELOCIDADES                              --
--                                          --
----------------------------------------------
-- | A função reduz devolve o angulo semelhante ao dado mas restringido ao intervalo [0,2*pi]
reduz::Double->Double
reduz x = if x>2*pi
          then reduz (x-(2*pi))
          else if x<0
               then reduz(x+(2*pi))
               else x
-- | A função vAcel calcula qual o vetor velocidade correspondente à aceleração do carro
vAcel::Carro->Acao->Double->Tempo->Velocidade
vAcel (Carro p d v) (Acao a s _ _ _) fAcel t = if a
                                               then if s
                                                    then (0,0)
                                                    else polarToCart (fAcel*t,reduz(toRadian d))
                                               else if s 
                                                    then  polarToCart (fAcel*t,reduz((toRadian d)+pi))
                                                    else (0,0)

-- | A função vAtri calcula qual o vetor velocidade correspondente ao atrito do carro
vAtri::Carro->Double->Tempo->Velocidade -- Independente
vAtri (Carro p d v) fA t = oPares (*) (-fA*t,-fA*t) v
-- | A função sentidoOposto calcula qual o vetor da força dos pneus que tem sentido oposto à velocidade
sentidoOposto::Double->Double->Double->Double
sentidoOposto y d b = if maxi - mini < (pi/2)
                      then reduz (b+pi)
                      else reduz b
                    where (maxi,mini) = (reduz(max y b),reduz(min y b))
-- | A função vPneu calcula qual o vetor velocidade correspondente à força dos pneus do carro
vPneu::Carro->Double->Tempo->Velocidade -- Independente
vPneu  (Carro p d v) fP t = (polarToCart (abs(sin(difAng))*fP*t*mV,(sentidoOposto aV direc aP)))
                          where (_,aV) = cartToPolar v
                                direc = (toRadian d)
                                aP = reduz (direc + (pi/2))
                                difAng = reduz (direc - aV)
                                mV = sqrt ((fst v)^2 + (snd v)^2)
-- | A função vPeso calcula qual o vetor velocidade correspondente à gravidade sobre o carro
vPeso::Tabuleiro->Carro->Double->Tempo->Velocidade -- Independente
vPeso tab (Carro p d v) fP t = case getTipo(whichPeca p tab) of
                               (Rampa o) -> case o of
                                            Norte -> (0,fP*t)
                                            Sul -> (0,-fP*t)
                                            Este -> (-fP*t,0)
                                            Oeste -> (fP*t,0) 
                               _ -> (0,0)  
-- | A função novaV dá qual a velocidade a somar ao carro para que este fique atualizado tendo em conta as propriedades do jogo
novaV::Tabuleiro->Acao->Carro->Propriedades->Tempo->Velocidade
novaV tab ac c (Propriedades fAt fPn fAcel fG fN fR) t = oPares (+) (vAtri c fAt t) (oPares (+) (vPneu c fPn t) (oPares (+) (vAcel c ac fAcel t) (vPeso tab c fG t)))
-- | A função adicionaV adiciona uma certa velocidade à velocidade de um carro e devolve esse novo carro
adicionaV::Carro->Velocidade->Carro
adicionaV (Carro p d v) nv = (Carro p d (oPares (+) v nv))
-- | A função atualizaV atualiza o jogo tendo em conta o tempo, o Jogo anterior, o identificador de jogador e uma ação; Mudando apenas as velocidades dos carros
atualizaV::Tempo->Jogo->Int->Acao->Jogo-- atrito , pneus, acel, peso, nitro, roda
atualizaV t jog@(Jogo m@(Mapa k tab) pro@(Propriedades fAtri fPn fAcel fG fN fR) cars ln h) ij ac@(Acao a s e d n)  = (Jogo m pro newCars ln h)
                                                                                                                    where carsSnitro = changeListElem (adicionaV (cars!!ij) nV) ij cars
                                                                                                                          nV = novaV tab ac (cars!!ij) pro t
                                                                                                                          newCars = if n /= Nothing 
                                                                                                                                    then zipWith (adicionaV) carsSnitro (ativaNitro (removeJust n) cars fN (tempoN t 0 ln))
                                                                                                                                    else carsSnitro
                                                                                                                    
    
----------------------------------------------
--                                          --
-- DIREÇÃO                                  --
--                                          --
----------------------------------------------
-- | A função adicionaD muda a direção de um carro dada uma ação, a constante kRoda, um tempo e o carro que se pretende rodar
adicionaD::Acao->Double->Tempo->Carro->Carro
adicionaD (Acao _ _ l r _) fR t (Carro p d v) = if l
                                                then if r
                                                     then (Carro p d v)
                                                     else (Carro p (d+(fR*t)) v)
                                                else if r
                                                     then (Carro p (d-(fR*t)) v)
                                                     else (Carro p d v)
-- | A função atualizaD recebe um tempo, um jogo anterior, um identificador de jogador e uma ação e devolve o jogo com as direções certas e quantidades de nitro de cada carro                                           
atualizaD::Tempo->Jogo->Int->Acao->Jogo
atualizaD tem 
          jog@(Jogo m pro@(Propriedades fAtri fPn fAcel fPe fN fR) cars n h)
          ij
          ac = (Jogo m pro newCars newNitro novoH)
             where newCars = changeListElem (adicionaD ac fR tem (cars!!ij)) ij cars
                   novoH = changeListElem (guardaPos (cars!!ij) (h!!ij)) ij h
                   newNitro = case ac of
                              (Acao _ _ _ _ Nothing) -> n
                              (Acao _ _ _ _ (Just jA)) -> atualizaNitro ij tem n 


----------------------------------------------
--                                          --
-- POSICAO                                  --
--                                          --
----------------------------------------------
-- | A função guardaPos atualiza o histórico de um carro tendo em conta a lista de posições pelas quais um certo carro passou
guardaPos::Carro->[Posicao]->[Posicao]
guardaPos (Carro (x,y) d v) [] = guardaPos (Carro (x,y) d v) [(floor x,floor y)]
guardaPos (Carro (x,y) d v) l = if p /= head l
                                then p:l
                                else l
                              where p = (floor x,floor y)

{-|                                                                                                                                 
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t j i a = atualizaD t (atualizaV t j i a) i a
-- | A função fazTestesT4 cria testes para serem usados através de um mapa fixo, apenas mudando qual o carro que o iria percorrer
fazTestesT4::Carro->Propriedades->Jogo
fazTestesT4 (Carro (px,py) d v) pro = (Jogo {mapa=mapas4!!3,
                                             pista=pro, 
                                             carros=[Carro (px,py) d v], 
                                             nitros=[3], 
                                             historico=[[(1,1)]]})
-- | Exemplo de Carro (Carro 0)
carro0::Carro
carro0 = (Carro {posicao = (5.5,3.5),direcao = 10,velocidade = (1,(-2))})
-- | Exemplo de Carro (Carro 1)
carro1::Carro
carro1 = (Carro {posicao = (5.5,5.5),direcao = (-40),velocidade = (1,1)})
-- | Exemplo de Carro (Carro 2)
carro2::Carro
carro2 = (Carro {posicao= (5.5,5.5),direcao=0,velocidade = (2,0)})
-- | A função juntaCarros adiciona um carro a um certo Jogo
juntaCarros::Jogo->Carro->Jogo
juntaCarros (Jogo {mapa= m,
                   pista= pro, 
                   carros=cars, 
                   nitros=(hn:tn), 
                   historico=(hh:th)})
            c = (Jogo {mapa= m,
                   pista= pro, 
                   carros=c:cars, 
                   nitros=[hn]++(hn:tn), 
                   historico=[hh]++(hh:th)})