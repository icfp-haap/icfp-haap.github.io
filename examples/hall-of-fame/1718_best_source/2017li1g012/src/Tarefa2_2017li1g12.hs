{-|
Module : Tarefa2_2017li1g12.hs
Description : Módulo Haskell Com Funções para validar Tabuleiros.
Copyright : Ricardo Vieira Carvalho <rvcarvalho99@gmail.com>;
            José Luís Abreu Mendes  <jlamendes@outlook.com>

Um módulo que contém Funções que se chamam umas às outras recursivamente, de forma a que dado uma lista com passos (caminho) , forme um tabuleiro.
|-}

module Tarefa2_2017li1g12 where


import LI11718
import Testestarefa2

testesT2 :: [Tabuleiro]
testesT2 = [t1,t2,t3,t4,t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18]

{-| A função 'valida' testa se um dado mapa é ou não válido de acordo com um conjunto de regras. A esta função é dado um Mapa, e a função diz nos se este é ou não um Mapa Valido.
 
== Exemplos de utilização:

>>>  valida (Mapa ((2,1), Este) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]])
True

>>>  valida (Mapa ((2,3), Sul) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]])
False

-}

valida :: Mapa -> Bool
valida m = (chegapecapartida list t oi pi) && (tabuleiroSoLava (tabuleiroSemPercurso pos t)) && (rodeadodelava t) && (tabretangulo t) && (orientacaoPecaPartida oi t pi) && (lavaAltura0 (concat t))
         where Mapa (pi,oi) t = m
               list = listaposicoes pi oi t 0 oi pi
               (peca,ori,pos) = separatePecaOrientacaoPosicao (listaposicoes (pi) oi t 0 oi pi)


{-| A função 'chegapecapartida' vai receber numa lista de Peças associadas a uma Orientacao e Posicao, um Tabuleiro e uma Orientacao e Posicao, e vai dizer se o caminho vai acabar na mesma peça onde começou.

== Exemplos de utilização:

>>> chegapecapartida [(Peca (Curva Este) 0,Sul,(3,1)),(Peca (Rampa Sul) 0,Sul,(3,2)),(Peca (Curva Sul) 1,Oeste,(3,3)),(Peca Recta 1,Oeste,(2,3)),(Peca (Curva Oeste) 1,Norte,(1,3)),(Peca (Rampa Sul) 0,Norte,(1,2)),(Peca (Curva Norte) 0,Este,(1,1)),(Peca Recta 0,Este,(2,1))] [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] Este (2,1)
True

>>> > chegapecapartida [(Peca (Curva Este) 0,Sul,(3,1)),(Peca (Rampa Sul) 0,Sul,(3,2)),(Peca (Curva Sul) 1,Oeste,(3,3)),(Peca Recta 1,Oeste,(2,3)),(Peca (Curva Oeste) 1,Norte,(1,3)),(Peca (Rampa Sul) 0,Norte,(1,2)),(Peca (Curva Norte) 0,Este,(1,1)),(Peca (Curva Este) 0,Este,(2,1))] t1 Este (2,1)
False


-}

chegapecapartida :: [(Peca,Orientacao,Posicao)] -> Tabuleiro -> Orientacao -> Posicao -> Bool
chegapecapartida lp [] opi pi = False
chegapecapartida lp t opi pi | (up == pegaPecaTab pi t) && uo == opiatualizada                      = True
                             | otherwise                                                            = False
                             where (up,uo,upos) = last lp  
                                   Peca tipoA alturaA = up
                                   opiatualizada = atualizarorientacao uo upos tipoA t 


{-| A função 'tabuleiroSoLava' vai Pegar num Tabuleiro e ver se todas as Pecas são __Peca Lava 0__.

== Exemplos de utilização:

>>> tabuleiroSoLava [[Peca Lava 0],[Peca Lava 0]]
True

>>> tabuleiroSoLava [[Peca Lava 0],[Peca Lava 0], [Peca Recta 0]]
False


-}

tabuleiroSoLava :: Tabuleiro -> Bool
tabuleiroSoLava [] = True
tabuleiroSoLava (a:b) | a == takeWhile (== (Peca Lava 0)) a       = True && (tabuleiroSoLava b)
                      | otherwise                                 = False

{-| A função 'retiraPecaTabuleiro' vai Pegar num Tabuleiro e vai trocar a Peça do Tabuleiro que têm a Posicao dada como argumento.

== Exemplos de utilização:

>>> retiraPecaTabuleiro (1,1) [[Peca Lava 0,Peca (Curva Oeste) 0],[Peca Recta 0, Peca (Rampa Este) 0]]
[[Peca Lava 0,Peca (Curva Oeste) 0],[Peca Recta 0,Peca Lava 0]]


>>> retiraPecaTabuleiro (1,2) t1
[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]



-}

retiraPecaTabuleiro :: Posicao -> Tabuleiro -> Tabuleiro
retiraPecaTabuleiro (a,b) t = ( (take b t) ++ (linhaPecaRetirada : []) ++ (drop (b+1) t) ) 
                            where linhaPecaRetirada = (take a (t!!b)) ++ ((Peca Lava 0) : []) ++ ((drop (a+1) (t!!b)))

{-| A função 'tabuleiroSemPercurso' vai trocar todas as Pecas do Tabuleiro cujas posições pertençam á Lista de posições dada como argumento, por __Peca Lava 0__ .

== Exemplos de utilização:

>>> tabuleiroSemPercurso [(0,0), (1,0)] [[Peca Recta 0, Peca Recta 0]]
[[Peca Lava 0,Peca Lava 0]]

>>> tabuleiroSemPercurso [(0,0), (1,0)] [[Peca Recta 0, Peca Recta 0],[Peca Recta 0, Peca Recta 0]]
[[Peca Lava 0,Peca Lava 0],[Peca Recta 0,Peca Recta 0]]



-}

tabuleiroSemPercurso :: [Posicao] -> Tabuleiro -> Tabuleiro
tabuleiroSemPercurso [] tab = tab
tabuleiroSemPercurso (p:t) tab = tabuleiroSemPercurso t rpt
                             where rpt = retiraPecaTabuleiro p tab
                                   
{-| A função 'separatePecaOrientacaoPosicao' vai receber uma lista de Peças associadas a uma posição e orientação, e vai devolver um tuplo com uma lita de Peças, uma lista de Orientações, e uma lista de de Posições.

== Exemplos de utilização:

>>> separatePecaOrientacaoPosicao [(Peca (Curva Este) 0,Sul,(3,1)),(Peca (Rampa Sul) 0,Sul,(3,2)),(Peca (Curva Sul) 1,Oeste,(3,3)),(Peca Recta 1,Oeste,(2,3)),(Peca (Curva Oeste) 1,Norte,(1,3)),(Peca (Rampa Sul) 0,Norte,(1,2)),(Peca (Curva Norte) 0,Este,(1,1)),(Peca Recta 0,Este,(2,1))]
([Peca (Curva Este) 0,Peca (Rampa Sul) 0,Peca (Curva Sul) 1,Peca Recta 1,Peca (Curva Oeste) 1,Peca (Rampa Sul) 0,Peca (Curva Norte) 0,Peca Recta 0],[Sul,Sul,Oeste,Oeste,Norte,Norte,Este,Este],[(3,1),(3,2),(3,3),(2,3),(1,3),(1,2),(1,1),(2,1)])


>>> separatePecaOrientacaoPosicao [(Peca (Curva Este) 0,Sul,(3,1)),(Peca (Rampa Sul) 0,Sul,(3,2))]
([Peca (Curva Este) 0,Peca (Rampa Sul) 0],[Sul,Sul],[(3,1),(3,2)])


-}

separatePecaOrientacaoPosicao :: [(Peca,Orientacao,Posicao)] -> ([Peca],[Orientacao],[Posicao])
separatePecaOrientacaoPosicao [] = ([],[],[])
separatePecaOrientacaoPosicao ((a,b,c):t) = (a:x,b:y,c:z)
                                 where (x,y,z) = separatePecaOrientacaoPosicao t
                         
{-| A função 'listaposicoes' vai Pegar num Caminho, e colocar esse caminho numa lista com Peças associadas a uma Orientacao e Posicao, e para isso vai receber uma Posicao e Orientacao ( que variam de peça para peça), um Tabuleiro, um Int, (que serve para impedir que o caso de paragem ocorra logo na primeira peça), e mais uma Orientacao e Posicao (que não variam e correspondem á Orientacao e Posicao da Peca Inicial).

== Exemplos de utilização:

>>> listaposicoes (2,1) Este [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0 , Peca (Curva Este) 0, Peca Lava 0], [Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Recta 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] 0 Este (2,1)
[(Peca (Curva Este) 0,Sul,(3,1)),(Peca Recta 0,Sul,(3,2)),(Peca (Rampa Sul) 0,Sul,(3,3)),(Peca (Curva Sul) 1,Oeste,(3,4)),(Peca Recta 1,Oeste,(2,4)),(Peca (Curva Oeste) 1,Norte,(1,4)),(Peca (Rampa Sul) 0,Norte,(1,3)),(Peca Recta 0,Norte,(1,2)),(Peca (Curva Norte) 0,Este,(1,1)),(Peca Recta 0,Este,(2,1))]

>>> listaposicoes (2,1) Este [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] 0 Este (2,1)
[(Peca (Curva Este) 0,Sul,(3,1)),(Peca (Rampa Sul) 0,Sul,(3,2)),(Peca (Curva Sul) 1,Oeste,(3,3)),(Peca Recta 1,Oeste,(2,3)),(Peca (Curva Oeste) 1,Norte,(1,3)),(Peca (Rampa Sul) 0,Norte,(1,2)),(Peca (Curva Norte) 0,Este,(1,1)),(Peca Recta 0,Este,(2,1))]


-}


listaposicoes :: Posicao -> Orientacao -> Tabuleiro -> Int -> Orientacao -> Posicao -> [(Peca,Orientacao,Posicao)]
listaposicoes (x,y) o t i opi ppi | ((x == 0) || (y == 0) || (x == ((length (t !! 0)) - 1) ) || (y == ((length t) - 1)))                       = (Peca Lava 0,Norte,(0,0)) : []
                                  | (pegaPecaTab (x,y) t == pegaPecaTab ppi t) && (opiatualizada == o) && ((x,y) == ppi) && (i==1)             = []
                                  | pv && pav                                                                                                  = (pegaPecaTab (novaposicao oriatualizada (x,y)) t ,no, (novaposicao oriatualizada (x,y))) : listaposicoes (novaposicao oriatualizada (x,y)) (no) t 1 opi ppi
                                  | (pv == False) || (pav == False)                                                                            = (Peca Lava 0,Norte,(0,0)) : []
                                  where (pv,no) = (proximaPecaValida o t (x,y))
                                        pav = (proximaAlturaValida oriatualizada t (x,y))                                 
                                        Peca tipoA alturaA = pegaPecaTab (x,y) t
                                        oriatualizada = atualizarorientacao o (x,y) tipoA t
                                        opiatualizada = atualizarorientacao opi (x,y) tipoA t 
                                       

{-| A função 'novaposicao' vai dar a Posicao da próxima peça, e recebe como argumentos a Orientação da peça atual e a sua Posicao.

== Exemplos de utilização:

>>> novaposicao Este (1,1)
(2,1)

>>> novaposicao Sul (3,12)
(3,13)


-}


novaposicao :: Orientacao -> Posicao -> Posicao
novaposicao orieOriginal pos | orieOriginal == Oeste      = ((fp-1),sp)
                             | orieOriginal == Este       = ((fp+1),sp)
                             | orieOriginal == Norte      = (fp,(sp-1))
                             | orieOriginal == Sul        = (fp,(sp+1))
                              where fp = fst pos
                                    sp = snd pos  

{-| A função 'pegaPecaTab' vai retirar do Tabuleiro a Peça que se encontre na Posição dada como argumento da função.

== Exemplos de utilização:

>>> pegaPecaTab (1,1) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Peca (Curva Norte) 0



>>> pegaPecaTab (3,2) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Peca (Rampa Sul) 0


-}

pegaPecaTab :: Posicao -> Tabuleiro -> Peca
pegaPecaTab (a,b) t = (t !! b) !! a

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


{-| A função 'proximaPecaValida' testa se a Peça seguinte do caminho é valida, e para isso recebe como argumento uma Orientacão, umTabuleiro e uma Posição. A partir desses argumentos vai testar se a próxima peça será válida.
 
== Exemplos de utilização:

>>> proximaPecaValida Este [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (1,1)
(True,Este)




>>> proximaPecaValida Este [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (3,2)
(False,Este)



-}


proximaPecaValida :: Orientacao -> Tabuleiro -> Posicao -> (Bool,Orientacao)
proximaPecaValida ori tab p | tipoN == Curva (listaOrientacoes !! (indexOa + 1))                        = (True , listaOrientacoes !! (indexOa + 3))
                            | tipoN == Curva (listaOrientacoes !! indexOa)                              = (True , listaOrientacoes !! (indexOa + 1))
                            | tipoN == Rampa (listaOrientacoes !! indexOa)                              = (True , oriatualizada)
                            | tipoN == Rampa (listaOrientacoes !! (indexOa + 2))                        = (True , oriatualizada)
                            | tipoN == Recta                                                            = (True , oriatualizada)  
                            | otherwise                                                                 = (False , oriatualizada)
                            where Peca tipoA alturaA = pegaPecaTab p tab
                                  oriatualizada = atualizarorientacao ori p tipoA tab
                                  indexOa = indexori listaOrientacoes oriatualizada 0
                                  np = novaposicao oriatualizada p
                                  Peca tipoN alturaN = pegaPecaTab np tab 

{-| A função 'proximaAlturaValida' existe para atualizar a orientação da primeira peça, pois quando se começa o caminho, se a primeira peça for uma Curva (ex: Curva Norte) e a Orientação inicial for da mesma orientação, a proxima Peça a ser verificada é a peça (neste exemplo) a Este, daí ser necessário atualizar a orientação da Peça Inicial
 
== Exemplos de utilização:

>>> atualizarorientacao Norte (1,1) (Curva Norte) [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Este




>>> atualizarorientacao Norte (1,2) (Curva Norte)[[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
Norte



-}

atualizarorientacao :: Orientacao -> Posicao -> Tipo -> Tabuleiro -> Orientacao
atualizarorientacao ori p tipoA tab | tipoA == Curva (listaOrientacoes !! indexO)                  = (listaOrientacoes !! (indexO + 1))
                                    | tipoA == Curva (listaOrientacoes !! (indexO + 1))            = (listaOrientacoes !! (indexO + 3))
                                    | otherwise                                                    = ori
                                    where indexO = indexori listaOrientacoes ori 0
                                          Peca tipoA alturaA = pegaPecaTab p tab

{-| A função 'proximaAlturaValida' serve para ver se uma Altura da próxima Peça de um caminho é valida. Tem como argumento uma Orientacão , um Tabuleiro e uma Posicao. 
 
== Exemplos de utilização:

>>> proximaAlturaValida Norte [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (1,2)
True




>>> proximaAlturaValida Norte [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 3, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 3, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (1,2)
False


-}

proximaAlturaValida :: Orientacao -> Tabuleiro -> Posicao -> Bool
proximaAlturaValida ori tab p | tipoA == Rampa ori && tipoN == Rampa (listaOrientacoes !! (indexO + 2))                                        = (alturaN == alturaA)
                              | tipoA == Rampa ori && not (tipoN == Rampa (listaOrientacoes !! (indexO + 2)))                                  = (alturaN == (alturaA + 1))
                              | tipoA == Rampa (listaOrientacoes !! (indexO + 2)) && tipoN == Rampa (listaOrientacoes !! (indexO + 2))         = (alturaN == (alturaA - 1))
                              | tipoA == Rampa (listaOrientacoes !! (indexO + 2)) && not (tipoN == Rampa (listaOrientacoes !! (indexO + 2)))   = (alturaN == alturaA)
                              | tipoN == Rampa (listaOrientacoes !! (indexO + 2))                                                              = (alturaN == (alturaA - 1))
                              | otherwise                                                                                                      = (alturaN == alturaA)
                              where indexO = indexori listaOrientacoes ori 0    
                                    np = novaposicao ori p 
                                    Peca tipoN alturaN = pegaPecaTab np tab 
                                    Peca tipoA alturaA = pegaPecaTab p tab


--------- CODIGO ACRESCENTADO | FUNÇAO PRINCIPAL | 3º Ponto da Tarefa 2 | Verificar que a orientação inicial tem que ser compatível com a peça de partida. -------------

{-| A função 'orientacaoPecaPartida' serve para ver se uma Peça inicial é valida. Tem como argumento uma Orientacão (orientação inicial), um Tabuleiro e uma Posicao ( também a inicial). 
 
== Exemplos de utilização:

>>> orientacaoPecaPartida Norte [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (1,2)
True



>>> orientacaoPecaPartida Este [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]] (1,2)
False

-}

orientacaoPecaPartida :: Orientacao -> Tabuleiro -> Posicao -> Bool
orientacaoPecaPartida oinicial t pinicial | (oinicial == Norte) && (tipo == Curva Norte || tipo == Curva Este || tipo == Rampa Norte || tipo == Rampa Sul)        = True
                                          | (oinicial == Oeste) && (tipo == Curva Oeste || tipo == Curva Norte || tipo == Rampa Oeste || tipo == Rampa Este)      = True
                                          | (oinicial == Este) && (tipo == Curva Este || tipo == Curva Sul || tipo == Rampa Este || tipo == Rampa Oeste)          = True
                                          | (oinicial == Sul) && (tipo == Curva Sul || tipo == Curva Oeste || tipo == Rampa Sul || tipo == Rampa Norte)           = True
                                          | tipo == Recta                                                                                                         = True
                                          | otherwise                                                                                                             = False
                                          where Peca tipo altura = extrairElementoTabuleiroaux t pinicial 

{-| A função 'extrairElementoTabuleiroaux' pega num Tabuleiro e numa Posição, e devolve a Peça que se encontra nessa Posição do Tabuleiro. 
 
== Exemplos de utilização:

>>> extrairElementoTabuleiroaux ( [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 1]]) (1,1)
Peca (Curva Norte) 0




>>> extrairElementoTabuleiroaux ( [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 1]]) (3,2)
Peca (Rampa Sul) 0


-}

extrairElementoTabuleiroaux :: Tabuleiro -> Posicao -> Peca
extrairElementoTabuleiroaux tab pos = (tab !! (snd (pos))) !! (fst (pos))


--------- FUNÇAO PRINCIPAL | 5º Ponto da Tarefa 2 | Verifica se todas as peças de lava tem altura 0 ------------- 

{-| A função 'lavaAltura0' vai testar se dada uma lista de Peças, todas as Peças do Tipo Peca Lava têm Altura 0.
 
== Exemplos de utilização:

>>> lavaAltura0 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
True



>>> lavaAltura0 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 1]
False


-}

lavaAltura0 :: [Peca] -> Bool
lavaAltura0 [] = True
lavaAltura0 ((Peca a b):t) | a == Lava     = (b == 0) && lavaAltura0 t
                           | otherwise     = True && lavaAltura0 t  


--------- FUNÇAO PRINCIPAL | 6º Ponto da Tarefa 2 | testa as ultimas 4 func | Verifica que o mapa é rodeado por lava, ou seja, a primeira e última linha, assim como a primeira e última coluna são constituídas por peças necessariamente do tipo lava. ------------- 

 
{-| A função 'rodeadodelava' vai testar se um dado Tabuleiro é rodeado por Lava a toda a volta, ou seja, a sua primeira e última linha e primeira e ultima coluna apenas têm elementos do Tipo Peca Lava 0.
 
== Exemplos de utilização:

>>> rodeadodelava  [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
True


>>> rodeadodelava  [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 1]]
False

-}

rodeadodelava :: Tabuleiro -> Bool
rodeadodelava t | (lavancolunaaux t )&& (lavanlinhaaux t)         = True
                | otherwise                                                                                                        = False

--testa se a 1ªlinha é tudo lava

{-| A função 'lavanlinhaaux' vai testar se num dado Tabuleiro, a sua primeira e última linha (primeira e última listaa do Tabuleiro) apenas têm elementos do Tipo Peca Lava 0.
 
== Exemplos de utilização:

>>> lavanlinhaaux [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
True

>>> lavanlinhaaux [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
True


-}

lavanlinhaaux :: Tabuleiro -> Bool
lavanlinhaaux ([]) = True 
lavanlinhaaux ([]:ts) =  True
lavanlinhaaux ((t:ts):hs) | ((Peca Lava 0) == t) &&  (removeaux (Peca Lava 0) (last hs) == []) = lavanlinhaaux (ts: hs) 
                              | otherwise = False


--função que remove todos os elementos x da funçao escohida

{-| A função 'removeaux' vai remover todos os elementos de uma lista que sejam iguais ao argumento dado.
 
== Exemplos de utilização:

>>> removeaux (Peca Lava 0) [Peca Lava 0, Peca Recta 0]
[Peca Recta 0]



>>> removeaux (Peca Lava 0) [Peca Lava 0, Peca Lava 0, Peca Lava 0]
[]



-}
removeaux :: Eq a => a-> [a] -> [a]
removeaux _ [] = []
removeaux x (hs:t) = if x == hs then removeaux x t 
else hs: removeaux x t

--testa se a 1ªcoluna é tudo lava

{-| A função 'lavanlim1colunaaux ' vai testar se num dado Tabuleiro, a sua primeira coluna (composta por todos os primeiros elementos de cada lista do Tabuleiro) apenas têm elementos do Tipo Peca Lava 0.
 
== Exemplos de utilização:

>>> lavancolunaaux [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
True


>>> lavancolunaaux [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 1]]
False



-}

lavancolunaaux :: Tabuleiro -> Bool
lavancolunaaux [] = True
lavancolunaaux ((t:hs):ts) |  ((Peca Lava 0) == t) && ((Peca Lava 0) == (last hs)) = lavancolunaaux ts
                                  | otherwise = False 



--------- FUNÇAO PRINCIPAL | 6º Ponto da Tarefa 2 | Verifica que o mapa é rectangular ------------ 

{-| A função 'tabretangulo' vai testar se um dado Tabuleiro é ou não retângular.
 
== Exemplos de utilização:

>>> tabretangulo [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
True

>>> tabretangulo [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]
False

-}

tabretangulo :: Tabuleiro -> Bool
tabretangulo [] = True
tabretangulo (t:hs) | hs == [] = True 
                    | length t == length (head (hs)) = tabretangulo hs
                    | otherwise = False  

-----------------------------------------------------------------------------------------------------------------------------------------------------

