{-|
Module : Tarefa1_2017li1g12.hs
Description : Módulo Haskell Com Funções para construir caminhos
Copyright : Ricardo Vieira Carvalho <rvcarvalho99@gmail.com>;
            José Luís Abreu Mendes  <jlamendes@outlook.com>

Um módulo que contém Funções que se chamam umas ás outras recursivamente, de forma a que dado uma lista com passos (caminho) , forme um tabuleiro.
|-}

module Tarefa1_2017li1g12 where

import LI11718

import Testestarefa1



testesT1 :: [Caminho]
testesT1 = [c1,c2,c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15]

{-| A função 'constroi' cria, a partir de uma lista de passos, um tabuleiro, em que a peça inicial tem como Orientação Este, e altura de 0.
 
== Exemplos de utilização:

>>> constroi [Avanca, Avanca, Sobe, CurvaDir, CurvaDir]
Mapa ((1,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-}


constroi :: Caminho -> Mapa
constroi c = Mapa (partida c,Este) t
           where t = preenchertabuleiro (caminhoToPecas 0 (partida c) Este c) (dimensao c) (0,0) c  

{-| A função 'preenchertabuleiro' vai criar um tabuleiro (lista de lista de peças) a partir de uma lista de posicoes associadas a Pecas, duas posições e uma caminho.
 
== Exemplos de utilização:

>>> preenchertabuleiro [((1,1), (Peca Recta 0))] (2,2) (0,0) [Avanca]
[[Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0]]

-}

preenchertabuleiro :: [(Posicao,Peca)] -> Posicao -> Posicao -> Caminho -> [[Peca]]
preenchertabuleiro (a:b) dimensaoTab (poslinha,poscoluna) c | poscoluna < (snd dimensaoTab)       = preencherlinha (a:b) dimensaoTab (poslinha,poscoluna) c : preenchertabuleiro (a:b) dimensaoTab (poslinha,(poscoluna+1)) c 
                                                            | otherwise                           = []

{-| A função 'preencherlinha' vai criar uma linha de um tabuleiro (uma lista de lista de peças) a partir de uma lista de posicoes associadas a Pecas, duas posições e uma caminho.
 
== Exemplos de utilização:

>>> preencherlinha [((1,0), (Peca Recta 0)), ((1,1), (Peca Recta 0))] (2,2) (0,0) [Avanca,Avanca]
[Peca Lava 0,Peca Lava 0]

>>> preencherlinha [((1,0), (Peca Recta 0))] (2,2) (0,1) [Avanca]
[Peca Lava 0,Peca Recta 0]

-}

preencherlinha :: [(Posicao,Peca)] -> Posicao -> Posicao -> Caminho -> [Peca]
preencherlinha (a:b) dimensaoTab (poslinha,poscoluna) c | poslinha < (fst dimensaoTab)          = caminhooulava (a:b) (poslinha,poscoluna) c : preencherlinha (a:b) dimensaoTab ((poslinha+1),poscoluna) c
                                                        | otherwise                               = []

{-| A função 'caminhooulava' vai dar como resultado a Peça que se encontra numa posição, a partir de uma lista de peças associadas a posições, uma posição (que e a posição cuja Peça queremos descobrir) e um caminho.
 
== Exemplos de utilização:

>>> caminhooulava  [((1,1), (Peca Recta 0))] (1,1) [Avanca]
Peca Recta 0

>>> caminhooulava  [((1,1), (Peca Recta 0))] (0,0) [Avanca]
Peca Lava 0

-}

caminhooulava :: [(Posicao,Peca)] -> Posicao -> Caminho -> Peca
caminhooulava (a:b) (poslinha,poscoluna) c | elem (poslinha,poscoluna) posicoes                   = pecas !! (index (poslinha,poscoluna) posicoes 0)   
                                           | otherwise                                            = Peca Lava 0
                                           where (posicoes,pecas) = unzipPecasCaminho (caminhoToPecas 0 (partida c) Este c) 


{-| A função 'index' vai pegar numa posição que recebe como argumento, e vai calcular a sua posição (vai devolver um inteiro) numa lista de Posições (assumimos que essa posição se encontra nessa lista). A função recebe um Inteiro como argumento que será uma acumulador que vai percorrer a lista, e que deverá ser sempre chamado com o valor de 0.
 
== Exemplos de utilização:

>>> index (1,1) [(0,0), (1,0) , (0,1), (1,1)] 0
3


>>> index (1,0) [(0,0), (1,0) , (0,1), (1,1)] 0
1

-}

index :: Posicao -> [Posicao] -> Int -> Int
index (pl,pc) posicoes i | posicoes !! i == (pl,pc)     = i 
                         | otherwise                    = index (pl,pc) posicoes (i+1)     

{-| A função 'unzipPecasCaminho' vai pegar numa lista de posições associadas a peças, e vai devolver como resultado um tuplo com uma lista de posições e uma lista de peças.

== Exemplos de utilização:

>>> unzipPecasCaminho [((1,0), (Peca Recta 0))]
([(1,0)],[Peca Recta 0])


>>>  unzipPecasCaminho [((1,0), (Peca Recta 0)),((1,2), (Peca (Curva Este) 0))]
([(1,0),(1,2)],[Peca Recta 0,Peca (Curva Este) 0])


-}

unzipPecasCaminho :: [(Posicao,Peca)] -> ([Posicao],[Peca])
unzipPecasCaminho [] = ([],[])
unzipPecasCaminho ((x,y):ps) = (x:a,y:b)
                             where (a,b) = unzipPecasCaminho ps


{-| A função 'caminhoToPecas' vai dar uma lista de Posições, cada uma associada a uma Peça, a partir de uma Altura, Posição, Orientação e Caminho.

== Exemplos de utilização:

>>>  caminhoToPecas 0 (2,3) Este [Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca, CurvaEsq]
[((2,3),Peca Recta 0),((3,3),Peca (Curva Sul) 0),((3,2),Peca Recta 0),((3,1),Peca (Curva Este) 0),((2,1),Peca Recta 0),((1,1),Peca (Curva Norte) 0),((1,2),Peca Recta 0),((1,3),Peca (Curva Oeste) 0)]

-} 

caminhoToPecas :: Altura -> Posicao -> Orientacao -> Caminho -> [(Posicao,Peca)]
caminhoToPecas a p o [] = []
caminhoToPecas a p o (p1:d) = (p,Peca tfc altA) : caminhoToPecas na np no d
                       where tfc = tipofromcaminho p1 o
                             na = nextaltura tfc o a
                             np = nextposicao no p
                             no = nextorientacao tfc o   
                             altA = alturaAtual a tfc o

{-| A função 'alturaAtual' vai ter como argumentos uma Altura, um Tipo de Peça, e uma Orientação, e vai dar a Altura da peça atual. 
 
== Exemplos de utilização:

>>> alturaAtual 0 (Recta) Oeste
0

>>> alturaAtual 0 (Rampa Este) Oeste
-1



-}  

alturaAtual :: Altura -> Tipo -> Orientacao -> Altura
alturaAtual a tipo o | o == Sul && tipo == Rampa Norte    = (a-1)
                     | o == Norte && tipo == Rampa Sul    = (a-1)
                     | o == Oeste && tipo == Rampa Este   = (a-1)
                     | o == Este && tipo == Rampa Oeste   = (a-1)
                     | otherwise                          = a 

{-| A função 'tipofromcaminho' vai pegar em passos e na sua orientação e vai dar tipos de peças. 

== Exemplos de utilização:

>>> tipofromcaminho CurvaDir Norte
Curva Norte


-} 

tipofromcaminho :: Passo -> Orientacao -> Tipo 
tipofromcaminho p1 o | p1 == Avanca     = Recta
                     | p1 == CurvaDir   = Curva o
                     | p1 == CurvaEsq   = Curva ( oriCurvEsq o)
                     | p1 == Desce      = Rampa (oriDesce o)
                     | p1 == Sobe       = Rampa o

{-| A função 'oriCurvEsq' usa como argumento uma orientação ,e devolve outra orientação, que sera a orientação de uma CurvaEsq. 

== Exemplos de utilização:

>>> oriCurvEsq Norte 
Este

>>>oriCurvEsq Sul
Oeste



-} 

oriCurvEsq :: Orientacao -> Orientacao 
oriCurvEsq o | o == Sul      = Oeste
             | o == Norte    = Este
             | o == Este     = Sul
             | o == Oeste    = Norte  

{-| A função 'oriDesce' usa como argumento uma orientação ,e devolve outra orientação, que sera a orientação de uma Rampa, pois para a Rampa ser a descer, a orientação da Rampa terá de ser oposta à direção atual. 
 
== Exemplos de utilização:

>>>  oriDesce  Sul
Norte

>>>oriDesce  Este
Oeste



-} 

oriDesce :: Orientacao -> Orientacao
oriDesce o | o == Norte    = Sul
           | o == Sul      = Norte
           | o == Este     = Oeste
           | o == Oeste    = Este

{-| A função 'nextaltura' vai dar a Altura da proxima peça, a partir da Peça atual, da sua Orientação e Altura. 
 
== Exemplos de utilização:

>>> nextaltura (Rampa Norte) Norte 1
2

-}

nextaltura :: Tipo -> Orientacao -> Altura -> Int
nextaltura (tipoFromCam) orien a | tipoFromCam == Rampa orien     = a+1
                                 | tipoFromCam == Recta || tipoFromCam == Curva Oeste || tipoFromCam == Curva Este || tipoFromCam == Curva Norte || tipoFromCam == Curva Sul      = a
                                 | otherwise        = a-1

{-| A função 'nextposicao' vai dar a Posição da proxima peça, a partir da Orientação e Posição atual. Nesta função assumimos que não existem peças no limite do tabuleiro.
 
== Exemplos de utilização:

>>> nextposicao Norte (1,1)
(1,0)

-}
nextposicao :: Orientacao -> Posicao -> Posicao
nextposicao orieOriginal pos | orieOriginal == Oeste      = ((fp-1),sp)
                             | orieOriginal == Este       = ((fp+1),sp)
                             | orieOriginal == Norte      = (fp,(sp-1))
                             | orieOriginal == Sul        = (fp,(sp+1))
                              where fp = fst pos
                                    sp = snd pos  

{-| A função 'listaOrientacoes' serve para percorrer a Rosa dos Ventos no Sentido dos ponteiros do relógio. Tem ambos os argumentos duas vezes para que possa passar de (por exemplo) Oeste para Norte.

-}

listaOrientacoes = [Norte,Este,Sul,Oeste,Norte,Este,Sul,Oeste]

{-| A função 'indexori' serve para percorrer a função @listaOrientacoes@, e para isso recebe como argumento a listaOrientacoes, uma Orientação, e um Inteiro.

== Exemplos de utilização:

>>> indexori listaOrientacoes  Norte 0
0

>>> indexori listaOrientacoes  Oeste  0
3

-}

indexori :: [Orientacao] -> Orientacao -> Int -> Int
indexori (a:b) o i | a == o        = i
                   | otherwise     = indexori b o (i+1)

{-| A função 'nextorientacao' vai dar a Orientação da proxima peça, a partir da peça atual e da sua orientação.
 
== Exemplos de utilização:

>>> nextorientacao (Curva Este) Este
Sul

-}
nextorientacao :: Tipo -> Orientacao -> Orientacao
nextorientacao (tipoFromCam) o | tipoFromCam == Recta                        = o
                               | o == (listaOrientacoes !! indexO) && tipoFromCam == Rampa (listaOrientacoes !! indexO)          =  (listaOrientacoes !! indexO)
                               | o == (listaOrientacoes !! indexO) && tipoFromCam == Rampa (listaOrientacoes !! (indexO + 2))    = (listaOrientacoes !! indexO)
                               | o == (listaOrientacoes !! indexO) && tipoFromCam == Curva (listaOrientacoes !! (indexO + 1))    = (listaOrientacoes !! (indexO +3)) 
                               | o == (listaOrientacoes !! indexO) && tipoFromCam == Curva (listaOrientacoes !! indexO)          = (listaOrientacoes !! (indexO+1)) 
                               where indexO = indexori listaOrientacoes o 0




