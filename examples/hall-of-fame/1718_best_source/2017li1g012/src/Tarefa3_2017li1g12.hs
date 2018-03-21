{-|
Module : Tarefa3_2017li1g12.hs
Description : Módulo Haskell Com Funções para testar as Colisões de uma carro.
Copyright : Ricardo Vieira Carvalho <rvcarvalho99@gmail.com>;
            José Luís Abreu Mendes  <jlamendes@outlook.com>

Um módulo que contém Funções que se chamam umas às outras recursivamente, de forma a que dado um tabuleiro, um interválo de tempo e um Carro, sejam calculadas as suas colisões.
-}


module Tarefa3_2017li1g12 where


import LI11718
import Testestarefa3

s3 = [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) (-1), Peca Recta (-1), Peca (Curva Este) (-1) ,Peca Lava 0], [Peca Lava 0, Peca Recta (-1), Peca Lava 0, Peca Recta (-1), Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) (-1), Peca Recta (-1), Peca (Curva Sul) (-1), Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]

type Positiontab = (Double,Double) 

type Recta = (Double,Double,Double)

-----------------------------------------------------------------------------------------------------------------------------------------------------

testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25,a27, a26, a27, a28, a29, a30, a31, a32, a33, a34]

{-| A função 'movimenta' que recebe um tabuleiro, um período de tempo e um carro, e calcula o novo estado do carro após se ter movimentado durante o período de tempo dado. As regras de colisões são as que bem conhece da física, devendo também assumir que a direção do carro não é alterada e o carro desloca-se a velocidade constante e nem “acelera” nem “curva”, inclusive durante colisões.
O resultado é do tipo Maybe Carro para contemplar a possibilidade de o carro ser “destruído”, devendo a função devolver Nothing quando o carro é “destruído” (em qualquer momento durante a sua movimentação), ou Just e em caso contrário, sendo e o novo estado do carro.

== Exemplos de utilização:

>>>  movimenta [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta (0),Peca (Curva Norte) 0,Peca (Rampa Norte) (-1),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) 1,Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] 2 (Carro (1.6,1.5) 45 (0.8,0.1))
Nothing

>>>  movimenta [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] 2 (Carro (1.6,1.5) 45 (0.8,0.1))
Just (Carro {posicao = (3.2000000000000006,1.7000000000000002), direcao = 45.0, velocidade = (0.8,0.1)})


-}


movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t c = maybecarro
                where Carro posiCar diriCar veliCar = c
                      (a,b) = posiCar 
                      pospeca = (fromIntegral (floor a), fromIntegral (floor b))
                      (posfCar,velfCar) = transitaOuPara (posicaofinal posiCar veliCar pospeca m) posiCar (disttotal veliCar t) veliCar m
                      maybecarro = nothingOrJustCar posfCar velfCar diriCar

-----------------------------------------------------------------------------------------------------------------------------------------------------

{-| A função 'nothingOrJustCar' vai receber como argumento uma posição, uma  velocidade e um angulo. Caso a posição dada como argumento seja (-1,-1), a função deve devolver Nothing, caso contrário vai devolver Just e o estado atual do carro. 

== Exemplos de utilização:

>>>  nothingOrJustCar (1,1) (1,2) 45
Just (Carro {posicao = (1.0,1.0), direcao = 45.0, velocidade = (1.0,2.0)})

>>>  nothingOrJustCar (-1,-1) (1,2) 45
Nothing

-}


nothingOrJustCar :: Positiontab -> Velocidade -> Angulo -> Maybe Carro
nothingOrJustCar pos vel dir | pos == (-1,-1)      = Nothing 
                             | otherwise           = Just (Carro pos dir vel)  


{-| A função 'tipoCurva' vai testar se o uma peça vai ser do tipo curva, e para isso apenas recebe um tipo, e devolve um booliano.
== Exemplos de utilização:

>>>  tipoCurva (Curva Norte)
True

>>>  tipoCurva Recta
False

-}


tipoCurva :: Tipo -> Bool
tipoCurva x | x == Curva Norte   = True
            | x == Curva Sul     = True
            | x == Curva Oeste   = True
            | x == Curva Este    = True
            | otherwise          = False

{-| A função 'tipoRampa' vai testar se o uma peça vai ser do tipo Rampa, e para isso apenas recebe um tipo, e devolve um booliano.
== Exemplos de utilização:

>>>  tipoRampa (Rampa Oeste)
True

>>>  tipoRampa (Curva Oeste)
False

-}

tipoRampa :: Tipo -> Bool
tipoRampa x | x == Rampa Norte   = True
            | x == Rampa Sul     = True
            | x == Rampa Oeste   = True
            | x == Rampa Este    = True
            | otherwise          = False

{-| A função 'pegaPecaTab' vai retirar do Tabuleiro dado como argumento a Peça que se encontre na Posição dada (Positiontab) como argumento da função.

== Exemplos de utilização:

>>> pegaPecaTab (1,1) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Peca (Curva Norte) 0



>>> pegaPecaTab (3,2) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Peca (Rampa Sul) 0


-}


pegaPecaTabDouble :: Positiontab -> Tabuleiro -> Peca
pegaPecaTabDouble (a,b) t = (t !! d) !! c
                          where (c,d) = (floor a ,floor b)


{-| A função 'intersreta' vai receber duas retas do tipo __ax +by = c__ e vai calcular o ponto onde estas se intersetam. Caso as retas não se intersetem, a função deverá devolver a posição (-1,-1).

== Exemplos de utilização:

>>> intersreta  (1,1,1) (1,2,2)
(-0.0,1.0)



>>> intersreta  (0,0,1) (1,2,2)
(-1.0,-1.0)


-}


intersreta:: (Double, Double, Double) -> (Double, Double, Double) -> (Positiontab,Int)
intersreta (a, b, c) (d, e, f) | (a==0 && b==0 && d==0 && e==0 || a==0 && b==0 || d==0 && e==0 || b==0 && e==0 || a==0 && d==0) = ((-1,1) , 0)  
intersreta (0, b, c) (d, 0, f) = ( ((f/d),(c/b)), 1)
intersreta (a, 0, c) (0, e, f) = ( ((c/a),(f/e)), 1)
intersreta (0, b, c) (d, e, f) = ( ((((-e*c)/b)+f)/d,(c/b)) , 1)
intersreta (a, b, c) (0, e, f) = ( ((((-b*f)/e)+c)/a, (f/e)) , 1)
intersreta (a, 0, c) (d, e, f) = ( ((c/a),((((-d*c)/a)+f)/e)) , 1)
intersreta (a, b, c) (d, 0, f) = ( ((f/d),((((-a*f)/d)+c)/b)) , 1) 
intersreta (a, b, c) (d, e, f) = ( (x,y) ,1)
                               where x = (((f/e)-(c/b))/((-a/b) + (d/e)))
                                     y = (-a/b)*x + (c/b)


{-| A função 'vdirector_ponto_recta' vai receber um vetor (Velocidade) e um ponto (Positiontab) e vai dar uma Recta do tipo __ax+by=c__.

== Exemplos de utilização:

>>> vdirector_ponto_recta (2,1) (1,1)
(1.0,2.0,-1.0)



>>> vdirector_ponto_recta (3,5) (1,5)
(5.0,3.0,-10.0)



-}


vdirector_ponto_recta :: Velocidade -> Positiontab -> (Double,Double,Double)
vdirector_ponto_recta (0,b) (x,y) = (1, 0, x)
vdirector_ponto_recta (a,0) (x,y) = (0, 1, -y)
vdirector_ponto_recta (a,b) (x,y) = (b, a, c)
                                  where c = ((b)*x+a*(-y))


{-| A função 'disttotal' vai receber um vetor (Velocidade) e um Double que representa um período de tempo, e vai calcular a distância total percorrida por um carro nesse tempo com esse vetor. Caso o vetor tenha um valor absoluto de 0, o valor percorrido pelo carro será 0. 

== Exemplos de utilização:

>>> disttotal (1,1) 2
2.8284271247461903


>>> disttotal (1,0) 10
10.0

>>> disttotal (0,0) 10
0.0


-}


disttotal :: Velocidade -> Tempo -> Double
disttotal (a,b) time = moduloV * time
                     where moduloV = sqrt (a^2 + b^2)


{-| A função 'verificarPecaAtual' vai receber uma posição e um tabuleiro, e vai devolver o Tipo da peça que se encontrar nessa posição dada como argumento no tabuleiro também dado como argumento.

== Exemplos de utilização:

>>> verificarPecaAtual (1,1) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Curva Norte

>>>verificarPecaAtual (1,4) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Lava


-}


verificarPecaAtual :: Positiontab -> Tabuleiro -> Tipo 
verificarPecaAtual (x,y) tab = tipoA
                             where Peca tipoA alturaA = pegaPecaTabDouble (x,y) tab



{-| A função 'verificarProximaPeca' vai receber duas posições, uma inicial e uma final, e vai devolver a Posição do tabuleiro onde a posição final se encontrar.
== Exemplos de utilização:

>>> verificarProximaPeca  (1.2,1.2) (2,1.2)
(2.0,1.0)


>>>> verificarProximaPeca  (1.2,1.4) (1.2,2)
(1.0,2.0)


-}



verificarProximaPeca :: Positiontab -> Positiontab -> Positiontab
verificarProximaPeca (xi,yi) (xf,yf) = (fromIntegral (floor xentry),fromIntegral (floor yentry))
                                     where (xentry,yentry) = (xf + ((xf-xi) * 0.000001) ,(yf + ((yf-yi) * 0.000001))) 


verificarProximaPecaiplus :: Positiontab -> Positiontab -> Positiontab
verificarProximaPecaiplus (xi,yi) (xf,yf) = (fromIntegral (floor xentry),fromIntegral (floor yentry))
                                          where (xentry,yentry) = (xi + ((xf-xi) * 0.000001) ,(yi + ((yf-yi) * 0.000001))) 


--vai dizer qual e a pos final de acordo com a peca (curva ou recta/rampa)
{-| A função 'posicaofinal' vai receber uma posição inicial, um vetor (Velocidade), a posição onde se encontra a peça, e um Tabuleiro, e consoante a Peça que se encontrar nessa posição, vai devolver uma posição final.

== Exemplos de utilização:

>>> posicaofinal (1.1,1.2) (1,1) (1,1) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
(1.9000000000000001,2.0)


>>>>  posicaofinal (2.3,1.2) (1.4,1) (2,1) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
(3.0,1.7)


-}

posicaofinal  :: Positiontab -> Velocidade -> Positiontab -> Tabuleiro -> Positiontab
posicaofinal posi vel pospeca tab | tipop == (Curva Norte)          = pontoFinalCurvaNorte posi pospeca rectaVelocidade vel
                                  | tipop == (Curva Este)           = pontoFinalCurvaEste posi pospeca rectaVelocidade vel
                                  | tipop == (Curva Oeste)          = pontoFinalCurvaOeste posi pospeca rectaVelocidade vel    
                                  | tipop == (Curva Sul)            = pontoFinalCurvaSul posi pospeca rectaVelocidade vel 
                                  | otherwise                       = pontoFinalRectaOuRampa posi pospeca rectaVelocidade vel
                                  where Peca tipop alturap = pegaPecaTabDouble pospeca tab
                                        rectaVelocidade = vdirector_ponto_recta vel posi


--(a,b) é a posição da peça

{-| A função 'pontoFinalCurvaNorte' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor, e vai devolver um ponto, que corresponde ao lugar onde a recta interseta algum ponto da Peça atual que será Curva Norte, que vai ser o Ponto final.

== Exemplos de utilização:

>>> pontoFinalCurvaNorte (3,2) (2,1) (1,2,-1)  (-1,-1)
(2.3333333333333335,1.6666666666666667)


>>>> pontoFinalCurvaNorte (2.8,1.7) (2,1) (1.7,2.8,1.0999)  (-1,-1)
(2.733311111111111,1.26668888888888


-}
pontoFinalCurvaNorte :: Positiontab -> Positiontab -> Recta -> Velocidade -> Positiontab 
pontoFinalCurvaNorte posi (a,b) vel velvetor = intersectaCurva posi (a,b) vel velvetor (0,1,(-b-1)) (-1,1,-(b+a+1)) (1,0,(a+1))

{-| A função 'pontoFinalCurvaSul' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor, e vai devolver um ponto, que corresponde ao lugar onde a recta interseta algum ponto da Peça atual que será Curva Sul, que vai ser o Ponto final.

== Exemplos de utilização:

>>> pontoFinalCurvaSul (1.2,1.3) (1,1) (1.3,1.2,0.10000009)  (1,1)
(1.4800000359999999,1.5199999640000001)



>>>> pontoFinalCurvaSul (1,1) (1,1) (1,1,0)  (1,1)
(1.5,1.5)


-}
pontoFinalCurvaSul :: Positiontab -> Positiontab -> Recta -> Velocidade -> Positiontab 
pontoFinalCurvaSul posi (a,b) vel velvetor = intersectaCurva posi (a,b) vel velvetor (0,1,-b) (-1,1,-(b+a+1)) (1,0,a)

{-| A função 'pontoFinalCurvaEste' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor, e vai devolver um ponto, que corresponde ao lugar onde a recta interseta algum ponto da Peça atual que será Curva Este, que vai ser o Ponto final.

== Exemplos de utilização:

>>> pontoFinalCurvaEste (1,2) (1,1) (1.8,1.0,1.4)  (0.5,-0.5)
(1.7499999999999998,1.7499999999999996)


>>>> pontoFinalCurvaEste (1.1,2.1) (1,1) (1.8,1.0,1.4)  (0.5,-0.5)
(1.8888888888888888,2.0)


-}
pontoFinalCurvaEste :: Positiontab -> Positiontab -> Recta -> Velocidade -> Positiontab 
pontoFinalCurvaEste posi (a,b) vel velvetor = intersectaCurva posi (a,b) vel velvetor (0,1,(-b-1)) (1,1,(a-b)) (1,0,a)

{-| A função 'pontoFinalCurvaOeste' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor, e vai devolver um ponto, que corresponde ao lugar onde a recta interseta algum ponto da Peça atual que será Curva Oeste, que vai ser o Ponto final.

== Exemplos de utilização:

>>> pontoFinalCurvaOeste  (1.9,1.1) (1,1) (1.1,1.9,-1.5)  (-0.5,0.5)
(1.8750000000000002,1.8750000000000002)


>>>> pontoFinalCurvaEste (1.1,2.1) (1,1) (1.8,1.0,1.4)  (0.5,-0.5)
(1.8888888888888888,2.0)


-} 

pontoFinalCurvaOeste :: Positiontab -> Positiontab -> Recta -> Velocidade -> Positiontab 
pontoFinalCurvaOeste posi (a,b) vel velvetor = intersectaCurva posi (a,b) vel velvetor (0,1,-b) (1,1,(a-b)) (1,0,(a+1))

---- 4 hip. 
{-| A função 'pontoFinalRectaOuRampa' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor, e vai devolver um ponto, que corresponde ao lugar onde a recta interseta algum ponto da Peça atual que será uma Recta ou Rampa, que vai ser o Ponto final.


-} 
pontoFinalRectaOuRampa :: Positiontab -> Positiontab -> Recta -> Velocidade -> Positiontab
pontoFinalRectaOuRampa posi (a,b) vel velvetor = intersectaRectaOuRampa posi (a,b) vel velvetor (0,1,-b) (0,1,(-b-1)) (1,0,a) (1,0,(a+1))

--dando as retas (tipo de interseção) 
{-| A função 'intersectaCurva' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor (Velocidade), e três Rectas, e vai Calcular onde é que a Primeira recta se vai intersectar com uma das outras três rectas.


-}
intersectaCurva :: Positiontab -> Positiontab -> Recta -> Velocidade -> Recta -> Recta -> Recta -> Positiontab
intersectaCurva posi (a,b) vel velvetor recta1 recta2 recta3 | r1x < (a + 1) && r1x > a && not ((r1x,(-r1y)) == (posi)) && (ePonto1 == 1) && rVP1                                           = (r1x,-r1y)
                                                             | (-r3y) < (b + 1) && (-r3y) > b && not ((r3x,(-r3y)) == (posi)) && (ePonto3 == 1) && rVP3                                     = (r3x,-r3y)
                                                             | r2x < (a + 1) && r2x > a && (-r2y) < (b + 1) && (-r2y) > b && not ((r2x,(-r2y)) == (posi)) && (ePonto2 == 1) && rVP2         = (r2x,-r2y)
                                                             | otherwise = (-1,-1)
                                                             where ((r1x,r1y), ePonto1) = intersreta vel recta1
                                                                   ((r2x,r2y), ePonto2) = intersreta vel recta2
                                                                   ((r3x,r3y), ePonto3) = intersreta vel recta3
                                                                   (vx,vy) = velvetor 
                                                                   rVP1 = relacaoVelPosf posi (r1x,-r1y) (vx,vy)
                                                                   rVP2 = relacaoVelPosf posi (r2x,-r2y) (vx,vy)
                                                                   rVP3 = relacaoVelPosf posi (r3x,-r3y) (vx,vy)

{-| A função 'intersectaRectaOuRampa' vai receber uma posição inicial e a posição onde essa peça se encontra, uma recta, e um vetor (Velocidade), e quatro Rectas, e vai Calcular onde é que a Primeira recta se vai intersectar com uma das outras quatro rectas.


-}

intersectaRectaOuRampa :: Positiontab -> Positiontab -> Recta -> Velocidade -> Recta -> Recta -> Recta -> Recta -> Positiontab
intersectaRectaOuRampa posi (a,b) vel velvetor recta1 recta2 recta3 recta4 | r1x < (a + 1) && r1x > a && not ((r1x,-r1y) == (posi)) && (ePonto1 == 1) && rVP1                                           = (r1x,-r1y)
                                                                           | r2x < (a + 1) && r2x > a && not ((r2x,-r2y) == (posi)) && (ePonto2 == 1) && rVP2                                           = (r2x,-r2y)
                                                                           | (-r3y) < (b + 1) && (-r3y) > b && not ((r3x,-r3y) == (posi)) && (ePonto3 == 1) && rVP3                                     = (r3x,-r3y)
                                                                           | (-r4y) < (b + 1) && (-r4y) > b && not ((r4x,-r4y) == (posi)) && (ePonto4 == 1) && rVP4                                     = (r4x,-r4y)     
                                                                           | otherwise = (-1,-1)
                                                                           where ((r1x,r1y), ePonto1) = intersreta vel recta1
                                                                                 ((r2x,r2y), ePonto2) = intersreta vel recta2
                                                                                 ((r3x,r3y), ePonto3) = intersreta vel recta3
                                                                                 ((r4x,r4y), ePonto4) = intersreta vel recta4 
                                                                                 (vx,vy) = velvetor 
                                                                                 rVP1 = relacaoVelPosf posi (r1x,-r1y) (vx,vy)
                                                                                 rVP2 = relacaoVelPosf posi (r2x,-r2y) (vx,vy)
                                                                                 rVP3 = relacaoVelPosf posi (r3x,-r3y) (vx,vy)
                                                                                 rVP4 = relacaoVelPosf posi (r4x,-r4y) (vx,vy)


 
relacaoVelPosf :: Positiontab -> Positiontab -> Positiontab -> Bool
relacaoVelPosf (pix,piy) (rx,ry) (vx,vy) = relacaoVelxPosfx (pix,piy) (rx,ry) (vx,vy) && relacaoVelyPosfy (pix,piy) (rx,ry) (vx,vy) 


relacaoVelxPosfx :: Positiontab -> Positiontab -> Positiontab -> Bool
relacaoVelxPosfx (pix,piy) (rx,ry) (vx,vy) | vx == 0        = True
                                           | otherwise      = ((rx - pix) / vx) > 0


relacaoVelyPosfy :: Positiontab -> Positiontab -> Positiontab -> Bool
relacaoVelyPosfy (pix,piy) (rx,ry) (vx,vy) | vy == 0        = True
                                           | otherwise      = ((ry - piy) / vy) > 0


{-| A função 'convertOPPtoO'~recebe um argumento vindo da função @Orientacaoproximapeca@ e "traduz" esse argumento para uma Orientacão
== Exemplos de utilização:

>>> convertOPPtoO E
Este


>>>> convertOPPtoO N
Norte

-}

convertOPPtoO :: Orientacaoproximapeca -> Orientacao 
convertOPPtoO opp | opp == N      = Norte
                  | opp == S      = Sul
                  | opp == E      = Este
                  | opp == O      = Oeste


data Orientacaoproximapeca = N | S | E | O | MesmaPeca
     deriving (Eq,Read,Show)

{-| A função 'orientacoesproximapeca' serve para percorrer a Rosa dos Ventos no Sentido dos ponteiros do relógio. Tem ambos os argumentos duas vezes para que possa passar de (por exemplo) Oeste para Norte.

-}
orientacoesproximapeca = [Norte,Este,Sul,Oeste,Norte,Este,Sul,Oeste]

{-| A função 'indexori' serve para percorrer a função @listaOrientacoes@, e para isso recebe como argumento a listaOrientacoes, uma Orientação, e um Inteiro.

== Exemplos de utilização:

>>> indexori orientacoesproximapeca  Norte 0
0

>>> indexori orientacoesproximapeca  Oeste  0
3

-}

indexori :: [Orientacao] -> Orientacao -> Int -> Int
indexori (a:b) o i | a == o        = i
                   | otherwise     = indexori b o (i+1)


{-| A função 'orientacaoproximapeca' vai receber duas Positiontab (uma posição inicial e uma final) e vai devolver a orientacão da peça onde se encontrar a posição final. Caso esta seja a mesma da posição inicial, a função deverá retornar como resulado __Mesmapeca__

== Exemplos de utilização:

>>> orientacaoproximapeca (1,1) (1,1)
MesmaPeca


>>>> orientacaoproximapeca (1,1) (2,1)
E

-}

orientacaoproximapeca :: Positiontab -> Positiontab -> Orientacaoproximapeca
orientacaoproximapeca (posix,posiy) (posfx,posfy) | posfy == (posiy + 1)                      = S
                                                  | posfy == (posiy - 1)                      = N
                                                  | posfx == (posix + 1)                      = E
                                                  | posfx == (posix - 1)                      = O
                                                  | (posfy == posiy) && (posfx == posix)      = MesmaPeca


-- se for -1 -1 destroi
{-| A função 'transitaOuPara' vai receber duas posições, uma inicial e uma final, uma distância, um vetor velocidade e Tabuleiro, e vai devolver uma posição e uma velocidade. Caso a distância seja 0, a função vai devolver como posição a mesma posição que a inicial, e a velocidade será igual.

== Exemplos de utilização:

>>> transitaOuPara (1.5,1.5) (2,1.5) 0 (1,0) t1
((2.0,1.5),(1.0,0.0))



>>>> transitaOuPara (1.5,1.5) (2,1.5) 1 (1,0) t1
((1.66,1.9737087712930803),(1.0,0.0))


-}




transitaOuPara :: Positiontab -> Positiontab -> Double -> Velocidade -> Tabuleiro -> (Positiontab,Velocidade)
transitaOuPara posf posi disttotal vel tab | (fst posf == -1)                                                 = ((-1,-1) , (0,0))
                                           | moduleDist > disttotal || moduleDist == disttotal                = (finalPosition,vel)
                                           | otherwise                                                        = transitaOuPara posfN posiN (disttotal - moduleDist) veloN tab
                                           where pospeca = verificarProximaPecaiplus posi posf
                                                 posfAdjust = posfAjustada posf pospeca 0.01
                                                 (a,b) = posfAdjust
                                                 (c,d) = posi 
                                                 (vx,vy) = vel 
                                                 moduleDist = sqrt ((a-c)^2 + (b-d)^2)
                                                 factFinale = (disttotal/moduleDist)
                                                 finalPosition = (c - (factFinale * (c-a)),d - (factFinale * (d-b)))
                                                 (posiN,posfN,veloN) = verificarAltura posi posfAdjust pospeca vel tab
                                                 
{-| A função 'posfAjustada' ajusta a posição final aos limites das peças dos tabuleiros. devido aos arredondamentos a posição final nem sempre vai calhar no limite das peças (sejam estas curvas rectas ou rampas) pelo que se a posição for um pouco desviada desses limites, então esta função ajusta a posição ao limite.

== Exemplos de utilização:

>>> posfAjustada (1.00003,2.2) (1,2) (0.01)
(1.0,2.2)


>>>> posfAjustada (3.004433,2.0) (3,2) (0.01)
(3.0,2.0)

-}

posfAjustada :: Positiontab -> Positiontab -> Double -> Positiontab
posfAjustada (posfx,posfy) (pospecax,pospecay) intervalo | (posfx > (pospecax - intervalo)) && (posfx < (pospecax + intervalo))        = (fromint (round posfx),posfy)
                                                         | (posfy > (pospecay - intervalo)) && (posfy < (pospecay + intervalo))        = (posfx,fromint (round posfy))
                                                         | -posfy < y2D1 && -posfy > y3D1                                                = (posfx,-y1D1) 
                                                         | -posfy < y2D2 && -posfy > y3D2                                                = (posfx,-y1D2)
                                                         | otherwise                                                                   = (posfx,posfy)
                                                         where y1D1 = (- posfx) + (pospecax - pospecay)
                                                               y2D1 = (- posfx) + (pospecax - pospecay) + intervalo
                                                               y3D1 = (- posfx) + (pospecax - pospecay) - intervalo
                                                               y1D2 = posfx + (- pospecax - pospecay - 1)
                                                               y2D2 = posfx + (- pospecax - pospecay - 1) + intervalo
                                                               y3D2 = posfx + (- pospecax - pospecay - 1) - intervalo
                                                               fromint = fromIntegral




-- finalPosition = (c + distotalfinalX , d + distotalfinaly) 
-- distotalfinalXres = ((disttotal^2)/(1+((b^2)/(a^2))))
-- (distotalfinalX,distotalfinaly) = (distotalfinalXres,sqrt((disttotal^2) - (distotalfinalXres^2)))

--  where (posfinal,velocfinal) =transitaOuPara
-- verificar outra vez (distotalfinalX,distotalfinaly)

{-| A função 'verificarAltura' vai receber duas posições, uma inicial e uma final, uma distância, um vetor velocidade e Tabuleiro, e vai devolver uma posição e uma velocidade. Caso a distância seja 0, a função vai devolver como posição a mesma posição que a inicial, e a velocidade será igual.

== Exemplos de utilização:

>>> verificarAltura (1.6,1.6) (2,1.6) (1,1) (1,0) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
((2.0,1.6),(3.0,1.6),(1.0,0.0))


>>>> verificarAltura (2.1,1.6) (3,1.6) (2,1) (1,0) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
((3.0,1.6),(3.6,1.6),(1.0,0.0))

-}

verificarAltura :: Positiontab -> Positiontab -> Positiontab -> Velocidade -> Tabuleiro -> (Positiontab,Positiontab,Velocidade)
verificarAltura posi posf pospecaA vel tab | (alturaN > (alturaA + 1)) || (alturaN == (alturaA + 1))         = functionAPlus1 posi posf pospecaA vel tab
                                           | (alturaN < (alturaA - 1)) || (alturaN == (alturaA - 1))         = functionAMinus1 posi posf pospecaA vel tab
                                           | (alturaN == alturaA)                                            = functionA posi posf pospecaA vel tab
                                           where pospecaN = verificarProximaPeca posi posf 
                                                 orientationNA = orientacaoproximapeca pospecaA pospecaN
                                                 orientationN = convertOPPtoO orientationNA
                                                 Peca tipoA alturaA = pegaPecaTabDouble pospecaA tab
                                                 Peca tipoN alturaN = pegaPecaTabDouble pospecaN tab
                                                 indexoriA = indexori orientacoesproximapeca orientationN 0

-- Colisão por definir
{-| A função 'functionAPlus1' vai determinar se o carro segue ou colide para casos em que a peça de possível colisão tiver uma altura maior que a atual. Caso o carro colida contra uma peça com altura inválida, a função deverá retornar __((-1,-1),(-1,-1),(0,0))__

== Exemplos de utilização:

 functionAPlus1 (1.5,1.5) (1.2,1.8) (1,1) (0,-1) t1
((1.2,1.8),(2.0,1.8),(1.0,-0.0))


-}

functionAPlus1 :: Positiontab -> Positiontab -> Positiontab -> Velocidade -> Tabuleiro -> (Positiontab,Positiontab,Velocidade)
functionAPlus1 posi posf pospecaA vel tab | tipoN == Lava                                                                                                                                                                                            = colisao vel posf pospecaA tab pospecaN
                                          | (tipoA == (Rampa orientationN)) && not (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 2)))                                                                                                      = seguir posi posf vel tab pospecaN
                                          | (tipoA == (Rampa orientationN) || tipoA == Recta || tipoCurva tipoA) && (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3)))     = ((-1,-1),(-1,-1),(0,0))
                                          | (((tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 3)))) && (tipoN == Rampa (orientacoesproximapeca !! (indexoriA))))             = seguir posi posf vel tab pospecaN
                                          | (((tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 3)))) && (tipoA == Rampa (orientacoesproximapeca !! (indexoriA))))             = seguir posi posf vel tab pospecaN
                                          | (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 3)))                                                                             = seguir posi posf vel tab pospecaN
                                          | ((tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3)))) && (alturaA >= 0)                                                           = ((-1,-1),(-1,-1),(0,0)) 
                                          | ((tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3)))) && (alturaA < 0)                                                            = colisao vel posf pospecaA tab pospecaN
                                          | otherwise                                                                                                                                                                                                = colisao vel posf pospecaA tab pospecaN
                                           where pospecaN = verificarProximaPeca posi posf 
                                                 orientationNA = orientacaoproximapeca pospecaA pospecaN
                                                 orientationN = convertOPPtoO orientationNA
                                                 Peca tipoA alturaA = pegaPecaTabDouble pospecaA tab
                                                 Peca tipoN alturaN = pegaPecaTabDouble pospecaN tab
                                                 indexoriA = indexori orientacoesproximapeca orientationN 0

{-| A função 'functionAMinus1' vai determinar se o carro segue ou colide para casos em que a peça de possível colisão tiver uma altura menor que a atual. Caso o carro colida contra uma peça com altura inválida, a função deverá retornar __((-1,-1),(-1,-1),(0,0))__

== Exemplos de utilização:

 functionAPlus1 (1.5,1.5) (1.2,1.8) (1,1) (0,-1) t1
((1.2,1.8),(2.0,1.8),(1.0,-0.0))


-}

functionAMinus1 :: Positiontab -> Positiontab -> Positiontab -> Velocidade -> Tabuleiro -> (Positiontab,Positiontab,Velocidade)
functionAMinus1 posi posf pospecaA vel tab | tipoN == Lava                                                                                                                                                                                       = ((-1,-1),(-1,-1),(0,0))
                                           | ((tipoA == (Recta)) || (tipoCurva tipoA) || (tipoA == (Rampa (orientacoesproximapeca !! (indexoriA + 2))))) && (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 2)))                         = seguir posi posf vel tab pospecaN
                                           | (((tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 3)))) && (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 2))))    = seguir posi posf vel tab pospecaN
                                           | (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 3)))                                                                        = seguir posi posf vel tab pospecaN
                                           | ((tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3)))) && (alturaA >= 0)                                                      = ((-1,-1),(-1,-1),(0,0)) 
                                           | ((tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3)))) && (alturaA < 0)                                                       = colisao vel posf pospecaA tab pospecaN
                                           | otherwise                                                                                                                                                                                           = ((-1,-1),(-1,-1),(0,0))
                                           where pospecaN = verificarProximaPeca posi posf 
                                                 orientationNA = orientacaoproximapeca pospecaA pospecaN
                                                 orientationN = convertOPPtoO orientationNA
                                                 Peca tipoA alturaA = pegaPecaTabDouble pospecaA tab
                                                 Peca tipoN alturaN = pegaPecaTabDouble pospecaN tab
                                                 indexoriA = indexori orientacoesproximapeca orientationN 0


-- ver se o carro segue ou colide
--se for destruir retorna (-1,-1) para depois verificar na funcao transitaOuPara
{-| A função 'functionA' vai determinar se o carro segue ou colide para casos em que a peça de possível colisão tiver a mesma altura que a atual. Caso o carro colida contra uma peça com altura inválida, a função deverá retornar __((-1,-1),(-1,-1),(0,0))__

== Exemplos de utilização:

 functionA (1.5,1.5) (2,1.5) (1,1) (1,0) t1
((2.0,1.5),(1.5,1.5),(1.0,0.0))

-}

functionA :: Positiontab -> Positiontab -> Positiontab -> Velocidade -> Tabuleiro -> (Positiontab,Positiontab,Velocidade)
functionA posi posf pospecaA vel tab  | tipoN == Lava                                                                                                                                                                                                                                                       = ((-1,-1),(-1,-1),(0,0))
                                      | pospecaN == pospecaA && (alturaA <= (-1))                                                                                                                                                                                                                           = colisao vel posf pospecaA tab pospecaN
                                      | pospecaN == pospecaA && (alturaA > (-1))                                                                                                                                                                                                                            = ((-1,-1),(-1,-1),(0,0))
                                      | ((tipoA == (Recta)) || (tipoCurva tipoA) || (tipoA == (Rampa (orientacoesproximapeca !! (indexoriA + 2))))) && (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 2)))                                                                                         = colisao vel posf pospecaA tab pospecaN
                                      | (tipoA == (Rampa orientationN)) && not (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 2)))                                                                                                                                                                 = ((-1,-1),(-1,-1),(0,0))
                                      | (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3))) && (alturaA >= 0)                                                                                                                      = ((-1,-1),(-1,-1),(0,0)) 
                                      | (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 2))) || (tipoN == Curva (orientacoesproximapeca !! (indexoriA + 3))) && (alturaA < 0)                                                                                                                       = colisao vel posf pospecaA tab pospecaN
                                      | (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoA == Rampa (orientacoesproximapeca !! (indexoriA + 3))) || (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 1))) || (tipoN == Rampa (orientacoesproximapeca !! (indexoriA + 3)))        = seguir posi posf vel tab pospecaN
                                      | otherwise                                                                                                                                                                                                                                                           = seguir posi posf vel tab pospecaN
                                      where pospecaN = verificarProximaPeca posi posf 
                                            orientationNA = orientacaoproximapeca pospecaA pospecaN
                                            orientationN = convertOPPtoO orientationNA
                                            Peca tipoA alturaA = pegaPecaTabDouble pospecaA tab
                                            Peca tipoN alturaN = pegaPecaTabDouble pospecaN tab
                                            indexoriA = indexori orientacoesproximapeca orientationN 0



--se seguir, a velocidade vai se igual, a posicao que era inicial vai mudar. pos ini vai ser pos final anterioir

{-| A função 'seguir' vai ser usada caso um carro possa seguir caminho, ou seja, caso não haja colisão. A esta função vai ser dada uma Posição inicial e uma Posição final, e vai calcular uma Nova posição inicial e uma nova posição final.

== Exemplos de utilização:

>>>   seguir (2,1.5) (2,1.5) (1,1) t1
((2.0,1.5),(2.5,2.0),(1.0,1.0))

-}
                                            

seguir :: Positiontab -> Positiontab -> Velocidade -> Tabuleiro -> Positiontab -> (Positiontab,Positiontab,Velocidade)
seguir pi pf vel tab pospecaN = (pf,posicaofinal pf vel pospecaN tab,vel)
                 


{-| A função 'colisao' recebe uma velocidade, duas posição (sendo que a segunda é a posição da Peça), e um Tabuleiro, e retorna um tuplo com duas posições e uma Velocidade.

== Exemplos de utilização:

>>>  colisao (2,3) (1.5,1.5) (1,1) t1
((1.5,1.5),(2.0,1.8333333333333333),(-3.0,-2.0))


>>>  colisao (2,1) (2.2,1) (2,1) t1
((2.2,1.0),(2.0,1.1),(2.0,-1.0))


-}



colisao :: Velocidade -> Positiontab -> Positiontab -> Tabuleiro -> Positiontab -> (Positiontab,Positiontab,Velocidade)
colisao (x,y) (a,b) (c,d) t pospecaN | ( (tipoA == Lava) ||  (tipoRampa tipoA) || (tipoA == Recta) ) && ((a == c) || (a == (c+1)))           = ((a,b),posicaofinal (a,b) (-x,y) (c,d) t,(-x,y))                                        
                                     | ( (tipoA == Lava) ||  (tipoRampa tipoA) || (tipoA == Recta) ) && ((b == d) || (b == (d+1)))           = ((a,b),posicaofinal (a,b) (x,-y) (c,d) t,(x,-y))                                       
                                     | (tipoA == Curva Este || tipoA == Curva Oeste) &&  ( (c-d) == (a-b))                                  = ((a,b),posicaofinal (a,b) (y,x) (c,d) t,(y,x))
                                     | (tipoA == Curva Norte || tipoA == Curva Sul  ) && ((-(b-  k(f(b)))+1) == (a-k (f a)))                = ((a,b),posicaofinal (a,b) (-y,-x) (c,d) t,(-y,-x))
                                     | otherwise                                                                                            = ((-1,-1),(-1,-1),(0,0))                                          
                                     where Peca tipoA alturaA = pegaPecaTabDouble (c,d) t
                                           k = fromIntegral
                                           f = floor




posx_Ou_yIntervaloLimite :: Double -> Double -> Double -> Bool
posx_Ou_yIntervaloLimite posx_y limite intervalo | posx_y > limite - intervalo && posx_y < limite + intervalo         = True
                                                 | otherwise                                                          = False




-- ver função colisão


-- acrescentar dois casos particulares na functionA 3 linha

