module Tarefa3_2017li1g71 where
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import LI11718

testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta  1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Norte) 1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Recta 0,Peca Recta 1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Recta 0,Peca Recta 1,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0.5)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,1)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 1,Peca Recta 1,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0 ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],3,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,1)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0 ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0 ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Este) (-1) ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1) ,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],10,Carro {posicao = (1.5,1.5), direcao = 45, velocidade = (1,0.3)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],20,Carro {posicao = (7.2,1.5), direcao = 45, velocidade = (2,0.3)}),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],1,(Carro (3.5,2.5) 90 (0,(-1)))),
            ([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],1,(Carro (2.5,1.5) 0 (0,1)))]
-- | a função getaltura devolve a altura de uma peça dada essa mesma peça
getaltura::Peca->Int
getaltura (Peca _ a) = a
-- | a função whichPeca devolve a peça em que um certo ponto está num tabuleiro
whichPeca::Ponto->Tabuleiro->Peca
whichPeca (px,py) t = getPeca t (floor px,floor py)
-- | a função step faz com que se calcule o ponto seguinte dado o ponto de partida, a velocidade e a duração de tempo em que se vai deslocar
step::Ponto->Velocidade->Tempo->Ponto
step p v t= oPares (+) p (oPares (*) v (t,t))
-- | a função foraPeca calcula se um ponto sai fora da peça com uma dada velocidade com um dado periodo de tempo
foraPeca::Tabuleiro->Ponto->Velocidade->Tempo->Bool
foraPeca m (px,py) v t= case tipoPeca0 of
                        Lava -> True
                        Curva o -> case o of
                                   Norte -> nPy<(-nPx)+bSulNorte || saiuNormal
                                   Sul -> nPy>(-nPx)+bSulNorte || saiuNormal
                                   Este -> nPy<nPx+bEsteOeste || saiuNormal
                                   Oeste -> nPy>nPx+bEsteOeste || saiuNormal
                        _ -> saiuNormal 
                       where tipoPeca0 = getTipo(whichPeca (px,py) m)
                             (nPx,nPy) = (step (px,py) v t) 
                             saiuNormal = (floor px,floor py) /= (floor nPx,floor nPy)
                             bSulNorte = setLineNS (px,py)
                             bEsteOeste = setLineEO (px,py)

--auxiliares foraPeca
setLineNS::Ponto->Double
setLineNS (px,py) = 1+(toEnum (floor px) + toEnum (floor py))

setLineEO::Ponto->Double
setLineEO (px,py) = (toEnum (floor py) - toEnum (floor px))
------------------------
-- | a função obterDirVel devolve o conjunto de orientações sobre a qual se desloca a velocidade
obterDirVel::Velocidade->(Orientacao,Orientacao)
obterDirVel (vx,vy) | (vx==0&&(-vy)>0) = (Norte,Norte)
                    | (vx==0&&(-vy)<0) = (Sul,Sul)
                    | (vy==0&&vx>0) = (Este,Este)
                    | (vy==0&&vx<0) = (Oeste,Oeste)
                    | otherwise = case quad of
                                  1 -> (Norte,Este)
                                  3 -> (Sul,Oeste)
                                  4 -> (Sul,Este)
                                  2 -> (Norte,Oeste)
                    where quad | (vx>0&&(-vy)>0) = 1
                               | (vx<0&&(-vy)<0) = 3
                               | (vx>0&&(-vy)<0) = 4
                               | otherwise = 2

data Espaco = Completo | Metade deriving (Show,Eq)
tipoEspaco::Peca->Espaco
tipoEspaco (Peca (Curva _) _) = Metade
tipoEspaco _ = Completo
-- | a função obterMenorTempoR devolve o tempo que demora a sair da peça e o lado pelo qual sai
obterMenorTempoR::Orientacao->Ponto->Velocidade->(Double,Orientacao)
obterMenorTempoR o (x,y) (vx,vy) = case o of 
                                  Norte -> (abs((toEnum(floor y)-y)/vy),Norte)
                                  Sul -> (abs((toEnum(ceiling y)-y)/vy),Sul)
                                  Este -> (abs((toEnum(ceiling x)-x)/vx),Este) 
                                  Oeste -> (abs((toEnum(floor x)-x)/vx),Oeste)
-- | Calcula qual o menor tempo para depois calcular o ponto
menorTempo::(Double,Orientacao)->(Double,Orientacao)->(Double,Orientacao)
menorTempo (t0,o0) (t1,o1) | (t0>t1) = (t1,o1)
                           | otherwise = (t0,o0)
-- | a função pontoSaidaGeral devolve o ponto de saida,o tempo que demorou a sair e o lado da peça,sendo que se considera a peça um quadrado, pelo qual sai
pontoSaidaGeral::Ponto->Velocidade->(Ponto,Tempo,Orientacao)
pontoSaidaGeral p v = ((step p v novoT),novoT,ladoBateu)
                               where (a,b) = obterDirVel v
                                     novoT = fst(menorTempo (obterMenorTempoR a p v) (obterMenorTempoR b p v))
                                     ladoBateu = snd(menorTempo (obterMenorTempoR a p v) (obterMenorTempoR b p v))

-- | Função de ordem superior para operar 2 pares
oPares::(a->b->b)->(a,a)->(b,b)->(b,b)
oPares f (x0,y0) (x1,y1) = (f x0 x1,f y0 y1)
-- | Calcula o determinante de uma matriz composta por 2 vetores
determinante::(Ponto,Ponto)->(Ponto,Ponto)->Double
determinante ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
-- | a função intersecaoRetas devolve o ponto de interseçao de duas retas, sendo que estas retas estão definidas como sendo apenas um par de pontos
intersecaoRetas::(Ponto,Ponto)->(Ponto,Ponto)->Maybe Ponto
intersecaoRetas par1@((x1,y1),(x2,y2)) par2@((x3,y3),(x4,y4)) | det == 0 = Nothing
                                                              | otherwise = Just (parteX/det,parteY/det)
                                                              where parteX = ((x1*y2 - y1*x2) * (x3 - x4) - (x1-x2) * (x3*y4 - y3*x4))
                                                                    parteY = ((x1*y2 - y1*x2) * (y3 - y4) - (y1-y2) * (x3*y4 - y3*x4))
                                                                    det = determinante par1 par2
-- | a funçao tempoEntrePontos calcula o tempo que demora a chegar de um ponto a outro com uma dada velocidade
tempoEntrePontos::Ponto->Velocidade->Ponto->Tempo
tempoEntrePontos (x0,y0) (vx,vy) (x1,y1) = deslocamento/modVel
                                         where deslocamento = sqrt((x1-x0)^2 + (y1-y0)^2)
                                               modVel = sqrt (vx^2 + vy^2)
--apenas para poder usar os Maybes no resto das funçoes
removeJust::Maybe a->a
removeJust (Just x) = x
{- | a função pontoSaidaPeca é uma função que devolve o ponto em que o carro sai da peça
e a orientação, ou seja, o lado da peça pelo que sai, sendo Norte,Sul,Este,Oeste respetivamente
Cima,Baixo,Direita,Esquerda e usamos o Nothing para descrever a colisao com as curvas -}
pontoSaidaPeca::Tabuleiro->Ponto->Velocidade->(Ponto,Tempo,Maybe Orientacao)
pontoSaidaPeca t p v | tipoEspaco(whichPeca p t)==Completo = (a,b,Just c)
                     | otherwise = case getTipo(whichPeca p t) of
                                   (Curva o) -> if (c==oriCon o)||(c==oriDir o)
                                                then  (a,b,Just c)
                                                else case o of
                                                     Norte -> ((removeJust(iDNS)),(tempoEntrePontos p v (removeJust(iDNS))),Nothing)
                                                     Sul -> ((removeJust(iDNS)),(tempoEntrePontos p v (removeJust(iDNS))),Nothing)
                                                     Este -> ((removeJust(iDEO)),(tempoEntrePontos p v (removeJust(iDEO))) ,Nothing)
                                                     Oeste -> ((removeJust(iDEO)),(tempoEntrePontos p v (removeJust(iDEO))),Nothing)
                     where (a,b,c) = pontoSaidaGeral p v
                           vIE = (toEnum(floor (fst p)),toEnum(ceiling (snd p)))
                           vSD = (toEnum(ceiling (fst p)),toEnum(floor (snd p)))
                           vSE = (toEnum(floor (fst p)),toEnum(floor (snd p)))
                           vID = (toEnum(ceiling (fst p)),toEnum(ceiling (snd p)))
                           iDNS = (intersecaoRetas (p,(step p v 1)) (vIE,vSD))
                           iDEO = (intersecaoRetas (p,(step p v 1)) (vID,vSE))

data Mudanca = Horizontal | Vertical | Diagonal | Segue | Morre deriving (Show,Eq)
poealturas::Peca->(Maybe Double,Maybe Double,Maybe Double,Maybe Double)
poealturas (Peca tipo a) = case tipo of
                           Lava -> (Nothing,Nothing,Nothing,Nothing)
                           Recta -> (Just aD,Just aD,Just aD,Just aD)
                           Rampa o -> case o of
                                      Norte -> (Just (aD+1),Just aD,Just (aD+0.5),Just (aD+0.5))
                                      Sul -> (Just aD,Just (aD+1),Just (aD+0.5),Just (aD+0.5))
                                      Este -> (Just (aD+0.5),Just (aD+0.5),Just (aD+1),Just aD)
                                      Oeste -> (Just (aD+0.5),Just (aD+0.5),Just aD,Just (aD+1))
                           Curva o -> case o of
                                      Norte -> (Nothing,Just aD,Just aD,Nothing)
                                      Sul -> (Just aD,Nothing,Nothing,Just aD)
                                      Oeste -> (Just aD,Nothing,Just aD,Nothing)
                                      Este -> (Nothing,Just aD,Nothing,Just aD)
                        where aD = fromIntegral a
getalturaOri::Orientacao->(Maybe Double,Maybe Double,Maybe Double,Maybe Double)->Maybe Double
getalturaOri ori (n,s,e,o) = case ori of
                           Norte -> n
                           Sul -> s
                           Este -> e
                           Oeste -> o
decideMudanca::Peca->Orientacao->Peca->Mudanca
decideMudanca p0@(Peca tipo0 a0) o p1@(Peca tipo1 a1) | a0 >= 0 = if altP0==Nothing || altP1==Nothing
                                                                     then Morre
                                                                     else if abs(removeJust altP0-removeJust altP1) < 1
                                                                          then Segue
                                                                          else if removeJust altP0-removeJust altP1>=1
                                                                               then Morre
                                                                               else if removeJust altP1-removeJust altP0>=1
                                                                                    then case o of
                                                                                         Norte -> Vertical
                                                                                         Sul -> Vertical
                                                                                         _ -> Horizontal
                                                                                    else Morre
                                                         | a0 < 0 = case tipo0 of
                                                                    Curva _ -> if altP0==Nothing
                                                                               then Diagonal
                                                                               else if altP1==Nothing
                                                                                    then case o of
                                                                                         Norte -> Vertical
                                                                                         Sul -> Vertical
                                                                                         _ -> Horizontal
                                                                                     else if abs(removeJust altP0-removeJust altP1) < 1
                                                                                          then Segue
                                                                                          else if removeJust altP0-removeJust altP1>=1
                                                                                               then Morre
                                                                                               else if removeJust altP1-removeJust altP0>=1
                                                                                                    then case o of
                                                                                                         Norte -> Vertical
                                                                                                         Sul -> Vertical
                                                                                                         _ -> Horizontal
                                                                                                    else Morre
                                                                    _ -> if altP1 == Nothing 
                                                                         then case o of
                                                                              Norte -> Vertical
                                                                              Sul -> Vertical
                                                                              _ -> Horizontal
                                                                         else if abs(removeJust altP0-removeJust altP1) < 1
                                                                              then Segue
                                                                              else if removeJust altP0-removeJust altP1>=1
                                                                                   then Morre
                                                                                   else if removeJust altP1-removeJust altP0>=1
                                                                                        then case o of
                                                                                             Norte -> Vertical
                                                                                             Sul -> Vertical
                                                                                             _ -> Horizontal 
                                                                                        else Morre
                                                         where altP0 = getalturaOri o (poealturas p0)
                                                               altP1 = getalturaOri (oriCon o) (poealturas p1)



-- | a função mudaVelocidade calcula a velocidade de saída após uma certa colisao
mudaVelocidade::Tipo->Velocidade->Mudanca->Velocidade
mudaVelocidade t (vx,vy) m = case m of
                             Horizontal -> (-vx,vy)
                             Vertical -> (vx,-vy)
                             Diagonal -> case t of
                                         (Curva Norte)-> vetorRefletido (vx,vy) (1,1)
                                         (Curva Este)-> vetorRefletido (vx,vy)(-1,1)
                                         (Curva Sul)-> vetorRefletido (vx,vy) (-1,-1)
                                         (Curva Oeste)-> vetorRefletido (vx,vy) (1,-1)
                             Segue -> (vx,vy)
-- | Função Final
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t (Carro {posicao=p@(px,py),direcao=d,velocidade=v}) | foraPeca m p v t && tipoMudanca == Morre = Nothing
                                                                 | foraPeca m p v t = movimenta m (t-b) colidiu
                                                                 | otherwise = Just e
                                                                 where (a,b,c) = pontoSaidaPeca m p v
                                                                       (_,_,ori) = pontoSaidaGeral p v
                                                                       tipoMudanca = decideMudanca (whichPeca p m) ori (getPeca m (move (floor px,floor py) ori))
                                                                       colidiu = (Carro {posicao=step a novaV (0.000000000001),direcao=d,velocidade=novaV})             
                                                                       e = (Carro {posicao=(step p v t),direcao=d,velocidade=v})
                                                                       novaV = (mudaVelocidade (getTipo(whichPeca p m)) v tipoMudanca)

-- | a função nVetor normaliza um vetor, para depois poder ser usado para calcular os ricochetes em curvas
nVetor::(Floating a,Num a)=>(a,a)->(a,a)
nVetor (x,y) = oPares (/) (x,y) (sqrt(x^2+y^2),sqrt(x^2+y^2))
-- | a função vetorRefletido calcula qual o vetor velocidade com que um carro sai após a colisao com uma curva
vetorRefletido::(Floating a,Num a)=>(a,a)->(a,a)->(a,a)
vetorRefletido (x,y) (z,w) = (x-(2*nx*pE),y-(2*ny*pE))
                             where pE = (x*nx+y*ny)
                                   (nx,ny) = nVetor (z,w)


