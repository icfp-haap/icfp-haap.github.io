{-|
Module : Testestarefa1.hs
Description : Módulo Haskell com Testes, para a Tarefa 3.
Copyright : Ricardo Vieira Carvalho <rvcarvalho99@gmail.com>;
            José Luís Abreu Mendes  <jlamendes@outlook.com>

Um módulo que contém Testes constítuidos por um carro com posição inicial e um tempo que o carro vai percorrer, juntamente com um tabuleiro.  -}

module Testestarefa3 where

import LI11718

t1 = [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0, Peca (Rampa Sul) 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) 1, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]

t2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t3 = [[Peca Lava 0, Peca Lava 0, Peca Lava 0 , Peca Lava 0, Peca Lava 0], [Peca Lava 0, Peca (Curva Norte) 0, Peca Recta 0, Peca (Curva Este) 0 ,Peca Lava 0], [Peca Lava 0,Peca (Rampa Norte) (-1), Peca Lava 0, Peca (Rampa Norte) (-1), Peca Lava 0], [Peca Lava 0, Peca (Curva Oeste) (-1), Peca Recta (-1), Peca (Curva Sul) (-1), Peca Lava 0], [Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]

t4 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t5 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t6 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] 

t7 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t8 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Oeste) (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t9 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 2,Peca (Curva Este) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca (Rampa Norte) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t10 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca (Curva Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Este) (-1),Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t11 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t12 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta (0),Peca (Curva Norte) 0,Peca (Rampa Norte) (-1),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) 1,Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

t13 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta (0),Peca (Rampa Norte) 0,Peca (Rampa Norte) (-1),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) 1,Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

a1 = (t1,1.6,Carro  (3.2,1.5)  45  (1,0))
a2 = (t2, 2.1,Carro (1.5,1.5) 45 (0.5,2.2))
a3 = (t2, 1.2,Carro (1.8,3) 45 (-1,0.5))
a4 = (t3, 2.3,Carro  (2.1,3.1)  45  (-1,0.5))
a5 = (t4,1.3, Carro  (3.2,2.5)  45  (1,-0.9))
a6 = (t4,2.4,Carro  (5.8,2.4)  45  (0.5,-0.4))
a7 = (t4, 1.2,Carro  (2.1,1.6)  45  (2, 0.6))
a8 = (t5,1.2,Carro  (3.4,1.6)  45  (2, 0.6))
a9 = (t5, 1.1,Carro  (5.3,3.2)  45  (1.8, 0.4))
a10 = (t5, 1.2,Carro  (3.1,3.2)  45  (1, 3.5))
a11 = (t6,  1.2,Carro  (3.1,3.2)  45  (1, 3.5))
a12 = (t6,  1.2,Carro  (4.3,5.2)  45  (1.8, 3.5))
a13 = (t6, 0.9,Carro (5.1,5.3) 45 (1.3, 0.5))
a14 = (t6, 3.3,Carro (3.1,3.4) 45 (0, 2))
a15 = (t7, 3.5, Carro (4.8,1.7) 45 (1,-0.3))
a16 = (t7, 0.9, Carro (2.1,2.2) 45 (1.1,-2))
a17 = (t8,1.2, Carro (1.9,1.8) 45 (4.6,-3))
a18 = (t8, 3.1, Carro (1.6,4.4) 45 (0.7,-0.9))
a19 = (t3, 2.3, Carro (2.4,3.1) 45 (0.6,-0.9))
a20 = (t6, 3.1, Carro (2.93,1.12) 45 (1,4.1))
a21 = (t3, 0.7, Carro (2.93,1.12) 45 (1,4.1))
a22 = (t8,1.2, Carro (1.4,4.3) 45 (3.3,3))
a23 = (t6, 1.1, Carro (2.93,1.12) 45 (1,4.1))
a24 = (t9, 0.3, Carro (5.4,4.8) 45 (1,1))
a25 = (t9, 1.0, Carro (6.4,5.8) 45 (-1,-1))
--25
a26 = (t9, 2.0, Carro (8.8,2.1) 45 (-0.55,1))
a27 = (t9, 1.6, Carro (4.86, 5.6) 45 (0.55,-0.6))
a28 = (t10, 3.0, Carro (4.1,2.3) 45 (0.5,0.6))
a29 = (t10, 2.3, Carro (5.1,2.3) 45 (1,1.6))
a30 = (t10, 1.9, Carro (5.1,1.6) 45 (1,1.6))
a31 = (t11, 3.0, Carro (4.1,3.3) 45 (0.2,1))
a32 = (t12, 2.0, Carro (1.6,1.5) 45 (0.8,0.1))
a33 = (t13, 2.0, Carro (2.6,1.5) 45 (0.8,0.1))
a34 = (t13, 2.0, Carro (3.2,4.3) 45 (0.8,0.1))