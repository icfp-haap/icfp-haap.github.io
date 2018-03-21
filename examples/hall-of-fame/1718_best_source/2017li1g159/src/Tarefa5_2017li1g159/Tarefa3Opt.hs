{-|
Module : Tarefa3Opt
Description : Tarefa3Opt Trabalho prático.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

Este módolo contêm a solução da Tarefa 3 do trabalho prático de Laboratórios de Informática 1.

-}

module Tarefa3Opt where
    
import LI11718

{-|
      O tipo de dados Parede serve essencialmente para caracterizar o "tipo de parede" de uma dada peca.
-}
data Parede = Burn | Wall | Fall | Free | Trap deriving (Show,Eq)

{-| 
      O nome é a composição das palavras Bidimensional list (lista bidimensional). 

      Este tipo de dados irá representar matrizes de forma semelhante à que é representada no C.
      Onde basicamente não interessa ão computador se é uma matriz ou lista. O compilador é
      que tem de tratar de saber onde na lista está posicionado o elemente do matriz, neste caso este processo de conversão será feito pelos programadores.
-}
data Bidimlist = Bid Dimensao [Peca] deriving Show

tabtest1 :: Tabuleiro
tabtest1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1), Peca Recta (-1),Peca Recta (-1),Peca Recta 0,Peca (Rampa Sul) 1,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0], 
            [Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca Recta 1,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tabtest2 :: Tabuleiro
tabtest2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0, Peca Recta 0,Peca Recta (-1),Peca Recta 0,Peca (Rampa Sul) 1,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0], 
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Rampa Oeste ) 1,Peca Recta 1,Peca (Rampa Oeste) 1,Peca (Rampa Sul) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tabtest3 :: Tabuleiro
tabtest3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0, Peca Recta 0,Peca Recta (-1),Peca Recta 0,Peca (Rampa Sul) 1,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0], 
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Rampa Este) 0,Peca (Rampa Oeste ) 1,Peca Recta 1,Peca (Rampa Oeste) 1,Peca (Rampa Sul) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tabtest4 :: Tabuleiro
tabtest4 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0, Peca Recta 0,Peca Recta (-1),Peca Recta 0,Peca (Rampa Sul) 1,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0], 
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) (-1) , Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Este) 0,Peca (Rampa Este) 1,Peca (Rampa Este) 2,Peca (Rampa Este) 3,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] 

tabtest5 :: Tabuleiro
tabtest5 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Norte) 0,Peca (Rampa Sul) (-1) ,Peca Recta 0,Peca Recta 0,Peca (Rampa Norte) 0,Peca (Rampa Norte) (-1),Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [
             (tabtest1,6,Carro {posicao = (3.5,1.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest1, 1.5,Carro {posicao = (8.5,4.5), direcao = 45, velocidade = (-1,0.05)})
            ,(tabtest1,1,Carro {posicao = (5.5,1.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest1,1,Carro {posicao = (6.5,1.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest1,1,Carro {posicao = (7,1.5), direcao = 45, velocidade = (1,2)})
            ,(tabtest1,1,Carro {posicao = (7,2.5), direcao = 45, velocidade = (1,1)})
            ,(tabtest1,1,Carro {posicao = (7.5,2.5), direcao = 45, velocidade = (0,1)})
            ,(tabtest1,1,Carro {posicao = (7.5,4.1), direcao = 45, velocidade = (0,1)})
            ,(tabtest1,2,Carro {posicao = (7.5,4.1), direcao = 45, velocidade = (0.5,1.5)})
            ,(tabtest1,1,Carro {posicao = (10.5,5), direcao = 45, velocidade = (0,1)})
            ,(tabtest1,4.3,Carro {posicao = (10.5,6), direcao = 45, velocidade = (0,1)})
            ,(tabtest1,6.5,Carro {posicao = (9.5,11.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest1,3,Carro {posicao = (4,11.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest2,2.5,Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest2,2.7,Carro {posicao = (5.5,11.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest2,3.5,Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest3,2.5,Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-1)})
            ,(tabtest3,2.5,Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
            ,(tabtest3,5,Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
            ,(tabtest3,5,Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest3,5,Carro {posicao = (3.5,11.1), direcao = 45, velocidade = (1,0)})
            ,(tabtest4,4,Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest5,0.8,Carro {posicao = (2.5,1.1), direcao = 45, velocidade = (1,0)})
            ,(tabtest5,3 , Carro {posicao = (3.5,0.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest5,0 , Carro {posicao = (3.5,1.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest5,1 , Carro {posicao = (3.5,1.5), direcao = 45, velocidade = (0,0)})
            ,(tabtest5,1 , Carro {posicao = (70.5,1.5), direcao = 45, velocidade = (1,0)})
            ,(tabtest5, 40 , Carro {posicao = (1.8,5.5), direcao = 45, velocidade = (-1,-1)})
            ,(tabtest5,1 , Carro {posicao = (8.1,5.8), direcao = 45, velocidade = (2,-2)})
            ,(tabtest5,1.5 , Carro {posicao = (3.5,1.5), direcao = 45, velocidade = (-1,0)})
            ,(tabtest5,1.5 , Carro {posicao = (3.5,1.5), direcao = 45, velocidade = (1,0)})
                  ]
{-|
      A implementação desta função é o objetivo da Tarefa3.
-}
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro 
movimenta tab ti vi = branch (reshape tab) vi ti (pelc vi )

            where reshape :: Tabuleiro -> Bidimlist 
                  reshape (h:t) = Bid ( length h , length t + 1 ) (concat (h:t) )


{-|
      Esta é a função Principal que será recursivamente chamada para processar um movimento.
      Isto acontece pois nós consideramos que a melhor solução seria aquela em que se dividia o problema de movimentar o carro por 3 pecas por partes.  
-}                  
branch :: Bidimlist -> Carro -> Tempo -> Posicao -> Maybe Carro  
branch _ vi ti _ | ti < 0.00000000001                                  = Just vi     
branch tab vi ti (x,y)      | not (vBelongTodim (x,y) tab)             = Nothing
                            | (\(a,b) -> a==0 && b==0 )(velocidade vi) = Just vi
                            | otherwise                                = stream (channel key vi ti) key vi ti tab
                                          where key = retStone tab ( x, y )
                                          

isLava :: Peca -> Bool
isLava (Peca Lava _ ) = True
isLava _              = False
{-|
      Esta função indentifica se o carro vai intercetar alguma parede se sim qual é primeira. No caso de o carro estar na Lava devolve Nothing.

      == Exemplos de utilização:

      >>> channel (Peca Recta (-2) ) Carro {posicao = (7.5,4.1), direcao = 45.0, velocidade = (0.5,1.5)} 4 
      Just (0,1)

      >>> channel (Peca (Curva Oeste) (-2) ) Carro {posicao = (7.5,4.1), direcao = 45.0, velocidade = (0.5,1.5)} 2
      Just (1,1)

-}
channel :: Peca->Carro ->Tempo-> Maybe (Int , Int )
channel (Peca Lava _ ) _  _                         = Nothing
channel (Peca Recta _ ) vi ti                       = compose (filter (boolA (dif vi) (velocidade vi) ti ) l1)
channel (Peca (Rampa o) _ ) vi ti                   = compose (filter (boolA (dif vi) (velocidade vi) ti ) l1)
channel (Peca (Curva o) _ ) vi ti      | check o vi = compose (filter (boolC o (dif vi) (velocidade vi) ti ) l2)
                                       | otherwise  = Nothing
            where check o vi = bPartLava o (dif vi)
            
{-| 
      Lista de todas as possibilidades de resposta para a função 'channel', no caso do carro estar sobre a peca Rampa ou Recta.
 -}
l1 :: [(Int, Int)]             
l1 = [(0,1),(0,-1),(1,0),(-1,0),(0,0)]

{-| 
      Lista de todas as possibilidades de resposta para a função 'channel', no caso do carro estar sobre a peca Curva.
 -}
l2 :: [(Int, Int)]            
l2 = [(0,1),(0,-1),(1,0),(-1,0),(1,1),(1,-1),(0,0)]

{-|
      Recebe uma lista e devolve no caso de vazia Nothing e caso esta tenha elementes devolve o primeiro.
         == Exemplos de utilização:

      >>> compose [1,2,3,4,5]
      Just 1

      >>> compose []
      Nothing
-}
compose :: [ a ] -> Maybe a
compose []                                   = Nothing
compose l                                    = Just (head l) 


-- ======================================================================
--boolA (0.9999999999,0.499999999999) (-1 , 0 ) 0.5 (0, -1)  
--boolA (0.5,0.5) (1,0) 1 (0,1)    
-- ===========================Filtros====================================  
{-|
      boolA é um filtro para ser usado como argumento da função filter sobre a lista l1 ou l2 quando o carro está a dirigir-se para uma das quatro arestas da peca. 

      == Exemplos de utilização:

      >>> boolA (0.9999999999,0.499999999999) (-1 , 0 ) 0.5 (0, -1) 
      False

      >>> boolA (0.5,0.5) (1,0) 1 (1,0) 
      True
-}      
      
boolA :: Ponto -> Velocidade-> Tempo -> (Int,Int)  -> Bool
boolA (x,y) (vx,vy) dt (v1,v2)   | v1== 0 && v2== 0 = True 
                                 | otherwise        = mu >0 && dt >= mu
            where mu  = intersecT (vx,vy) (x,y) (v1,v2)
                                
{-|
      Esta função limita-se a encontrar o tempo de interseção do carro com uma das quatro arestas. 
      
      == Exemplos de utilização:

      >>> intersecT (0.2,-4.8) (0.1,0.7 )(1,0) 
      -4.0 // Não chega a intersetar-se dentro da peca.

      >>> intersecT (0.2,-4.8) (0.1,0.7 )(0,-1) 
      0.1458333333333333333334
-}
intersecT :: Velocidade -> Ponto -> (Int,Int) -> Tempo
intersecT (0 ,vy) (x,y) (0,-1 ) = findT 0 y vy
intersecT (0 ,vy) (x,y) (0 ,1 ) = findT 1 y vy
intersecT (vx ,0) (x,y) (-1,0 ) = findT 0 x vx
intersecT (vx ,0) (x,y) ( 1,0 ) = findT 1 x vx
intersecT (vx,vy) (x,y) (-1,0 ) = if (\ a t -> a<= t && a >=0 )( y - (vy/vx)* x ) 1     then findT 0 x vx else (-4)
intersecT (vx,vy) (x,y) ( 1,0 ) = if (\ a t -> a<= t && a >=0 )( y + (1-x)* (vy/vx) ) 1 then findT 1 x vx else (-4)
intersecT (vx,vy) (x,y) ( 0,-1) = if (\ a t -> a<= t && a >=0 )( x - y* (vx/vy) ) 1     then findT 0 y vy else (-4)
intersecT (vx,vy) (x,y) ( 0,1 ) = if (\ a t -> a<= t && a >=0 )(x + (1-y)* (vx/vy)) 1   then findT 1 y vy else (-4)


-- ======================================================================
      --
-- ======================================================================
{-|
      boolC é também um filtro para ser usado como argumento da função filter sobre a lista l2 quando o carro está a dirigir-se para uma das 2 diagonais da Peca Curva. 

      == Exemplos de utilização:

      >>> boolC Sul (0.7, 0.233) (3,1) 1 (1,1)
      False

      >>> boolC Norte (0.7, 0.233) (3,1) 1 (1,-1)
      True
      
-}       
boolC :: Orientacao ->  Ponto -> Velocidade-> Tempo -> (Int,Int)-> Bool
boolC _ _ _ _ (0,0)                                                  = True
boolC o (x,y) (vx,vy) dt v     | v /= ( 1 , 1 ) && v /= ( 1 , - 1 )  = (rot180 o == inloc v || rot270 o == inloc v) && boolA (x,y) (vx,vy) dt v 
boolC o (x,y) (vx,vy) dt v     | inWall o /= v                       = False
boolC _ (x,y) (vx,vy) dt (v1,v2)                                     = (\ a t -> a<= t && a >0 ) (insecT (vx,vy) (x,y) (v1,v2) ) dt                                    
                    
{-|
      Esta função calcula a interseção do carro com uma das duas diagonais de uma peca Curva.

      == Exemplos de utilização:

      >>> insecT (0.2,-4.8) (0.1,0.7 )(1,1) 
      0.11999999999999997

      >>> insecT (0.2,-4.8) (0,0.7 )(1,-1) 
      -1.0 // Não interseta
-}
insecT :: Velocidade -> Ponto -> (Int,Int) -> Tempo
insecT (0 ,vy) (x,y) (1,1)                        = findT x y vy
insecT (0 ,vy) (x,y) (1,-1)                       = findT (1 - x) y vy
insecT (vx ,0) (x,y) (1,1)                        = findT y x vx 
insecT (vx ,0) (x,y) (1,-1)                       = findT (1 - y) x vx
insecT (vx,vy) (x,y) (1,1)   | (vy / vx ) ==  1   = -1
insecT (vx,vy) (x,y) (1,-1)  | (vy / vx ) == -1   = -1
insecT v x (1,1 )                                 = if (\ a -> a<= 1 && a >=0 ) (crX1 x v )                                             then findT (crX1 x v ) (fst x) (fst v) else (-1)
insecT v x ( 1,-1 )                               = if (\ f x y ->  f x && f y ) (\ a -> a<= 1 && a >=0 ) (crX2 x v ) (1 - crX2 x v  )  then findT (crX2 x v ) (fst x) (fst v) else (-1)

{-|
      Calcula a primeira componente das cordenadas da interseção do carro com a diagonal da Curva de orientação Este e Oeste. 

      == Exemplos de utilização:

      >>> crX1 (4,2) (1, 9)
      4.25

      >>> crX1 (4,2) (1,- 9)
      3.8
      
-}
crX1 :: Ponto -> Velocidade -> Double
crX1 (x,y) (vx,vy) = (y * vx - x * vy) / (vx -vy)

{-|
      Calcula a primeira componente das cordenadas da interseção do carro com a diagonal da Curva de orientação Norte e Sul. 

      == Exemplos de utilização:

      >>> crX2 (3,2) (1,2)
      1.6666666666667

      >>> crX2 (0.11,2) (900,80)
      -0.9093877551020408
-}
crX2 :: Ponto -> Velocidade -> Double
crX2 (x,y) (vx,vy) = (x * vy + (1-y) * vx) / (vx +vy) 


-- ======================================================================
-- ======================================================================

------------------------------Funções Auxiliares ------------------------
-------------------------------------------------------------------------
{-|
      Recebe uma posicão final, uma posição inicial e uma velocidade e devolve o tempo que demora a chegar à posição final partindo da inicial à velocidade recebida.

      == Exemplos de utilização:

      >>> findT 4 2 1 
      2

      >>> findT 9.3232 3.22221 4.525299
      1.3481959976567293
-}
findT:: Double -> Double -> Double -> Double
findT f i v = (f - i)/v
{-|
      Recebe uma 'Bidimlist' e uma 'Posicao' e devolve a 'Peca' que corresponde a essa posição. 

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> retStone t (0,0)
      Peca (Curva Este) 0

      >>> retStone a (0,1)
      Peca (Rampa Sul) 0

-}
retStone :: Bidimlist -> Posicao -> Peca 
retStone (Bid dim l) v =  l !! cordToBidC dim v 
            where cordToBidC :: Dimensao-> Posicao -> Int
                  cordToBidC (dimx,dimy) (x,y) | x<dimx && y<dimy = y * dimx + x
{-|
      Esta função Recebe uma Bidemsional list e uma posição e verifica se essa posição pode pertencer à Bidimensional lista.
            

      == Exemplos de utilização:
      Seja t uma qualquer lista de 'Peca'.

      >>> vBelongTodim (5,5) (Bid (3,3) t )
      False

      >>> vBelongTodim (2,2) (Bid (3,3) t )
      True

      >>> vBelongTodim (14,1) (Bid (3,3) t )
      False

-}

vBelongTodim :: Posicao -> Bidimlist -> Bool
vBelongTodim (x,y) (Bid (dimx,dimy) _ ) | x<dimx && y<dimy && y > (-1) && x > (-1)  = True
                                        | otherwise        = False

{-|
      Esta função recebe um carro como argumento e devolve a sua posição relativa na Peca.

      == Exemplos de utilização:

      >>> dif (Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (3.5,11.1), direcao = 45, velocidade = (1,0)})
      (0.5,0.1)
     
-}
dif :: Carro -> Ponto
dif vi = relative (pelc vi) (posicao vi)
      where relative :: Posicao -> Ponto -> Ponto
            relative (xo,yo) (x,y) = ( x - fromIntegral xo , y - fromIntegral yo)

{-|
      Recebe uma carro e devolve a posição no tabuleiro desse carro. 

      == Exemplos de utilização:

      >>> dif (Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
      (3,11)

      >>> dif (Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
      (2,11)

      >>> pelc Carro {posicao = (7.5,2.5), direcao = 45.0, velocidade = (0.0,1.0)}
      (7,2)

-}
pelc :: Carro -> Posicao
pelc vi = realtoPos (posicao vi)
            where realtoPos :: Ponto -> Posicao 
                  realtoPos (a,b) = (floor a, floor b)
{-|
      Recebe uma 'Orientacao' e devolve a rotação de 270 graus dessa Orientação . 

      ==Exemplos de utilização:

      >>> rot270 Norte
      Este

      >>> rot270 Este
      Sul

-}
rot270 :: Orientacao -> Orientacao 
rot270 Norte = Este
rot270 Sul   = Oeste
rot270 Este  = Sul
rot270 Oeste = Norte

{-|
      Recebe uma 'Orientacao' e devolve a rotação de 180 graus dessa Orientação . 

      ==Exemplos de utilização:

      >>> rot180 Este
      Oeste

      >>> rot180 Sul
      Norte

-}
rot180 :: Orientacao -> Orientacao
rot180 Norte = Sul
rot180 Sul   = Norte
rot180 Este  = Oeste
rot180 Oeste = Este

{-|
      Recebe uma aresta de uma dada 'Peca' e devolve a orientação da parede relativa ao centro da Peca.

      ==Exemplos de utilização:

      >>> inloc (-1, 0)
      Oeste

      >>> inloc (0 ,-1)
      Norte
-}
inloc :: (Int,Int) -> Orientacao
inloc (0 ,-1) = Norte 
inloc (0 , 1) = Sul    
inloc (1 , 0) = Este 
inloc (-1, 0) = Oeste 

{-|
      Esta função recebe a 'Posicao' de uma Peca e uma 'Bidimlist' e diz qual é o tipo dessa 'Peca'.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> peek (0,0) t
      Curva Este

      >>> peek (0,1) t
      Rampa Sul 


-}
peek :: Posicao -> Bidimlist -> Tipo
peek (x,y) tab = (\ (Peca y z) -> y) ( retStone  tab ( x, y ) )
{-|
      Esta função recebe a 'Posicao' de uma Peca e uma 'Bidimlist' e diz qual é a Altura dessa 'Peca'.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 3,Peca Lava 1,Peca (Rampa Sul) 0,Peca Lava 5,Peca Recta 0]

      >>> peekHei (0,0) t
      0

      >>> peekHei (1,1) t
      5 

-}
peekHei :: Posicao -> Bidimlist -> Altura
peekHei (x,y) tab = (\ (Peca y z) -> z) ( retStone tab ( x, y ) )

{-|
      Esta função com a orientação de uma peça do tipo Curva diz qual é perede interior dessa curva ("inner wall").

      == Exemplos de utilização:

      >>> inWall Norte 
      (1,-1)

      >>> inWall Este
      (1,1)

-}
inWall :: Orientacao -> (Int, Int)
inWall teta | teta == Norte || teta == Sul  = (1, -1)
inWall teta | teta == Oeste || teta == Este = (1,1) 
{-|
      Esta função indica se um certo carro se encontra na parte de lava de uma Curva. 
      
      == Exemplos de utilização:

      >>> bPartLava Oeste (0.2,0.9) 
      False

      >>> bPartLava Sul (0.2,0.7)
      True

-}

bPartLava :: Orientacao -> Ponto -> Bool
bPartLava teta (a,b)  | teta == Norte &&  a >= 1-b = True  
                      | teta == Sul   &&  a <= 1-b = True
                      | teta == Este  &&  a <= b   = True 
                      | teta == Oeste &&  a >= b   = True 
                      | otherwise                  = False

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- ====================== IDENTIFICAR ===================================
{-|
      Está função trata de decidir após de indentificada a primeira etapa do movimento do carro qual é a função ou funções que devem calcular o comportamento do carro. 
-}
stream :: Maybe (Int,Int) -> Peca -> Carro -> Tempo -> Bidimlist ->Maybe Carro
stream Nothing key vi ti _                     = Nothing
stream ( Just (0,0) ) key vi ti _              = Just (Carro (segue (velocidade vi) ti (posicao vi)) (direcao vi) (velocidade vi) )  
stream ( Just d ) (Peca (Rampa o) z) vi ti tab = decide tab (  smp upwardcheck d vi tab z o ) d vi ti 
stream ( Just d ) (Peca (Curva o) z) vi ti tab = if d == (1,1) || d == (1,-1) then dicInWall tab vi d z ti else decide tab ( smp forwardcheck d vi tab z ) d vi ti 
stream ( Just d ) (Peca Recta z) vi ti     tab = decide tab ( smp forwardcheck d vi tab z ) d vi ti   

{-|
      Esta função de nivel superior é essencialmente usada para diminuir a complexidade da função 'stream'.
-}
smp :: (Orientacao -> (Int, Int) -> t) -> (Int, Int) -> Carro -> t
smp f d vi = f (inloc d ) ( (\(a,b) (c,d) -> (a+c, b+d) )(pelc vi) d ) 

{-|
      Esta função tem a tarefa de etiquetar uma dada aresta com o nome da sua Parede.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> forwardcheck Este (0,0) t 3
      Fall

      >>> forwardcheck Este (1,1) t 0
      Burn


-}
forwardcheck :: Orientacao-> Posicao -> Bidimlist -> Altura -> Parede 
forwardcheck teta (x,y) l z | nextH > z                                                                                                 = Wall 
                            | nextP == Recta && nextH == z                                                                              = Free 
                            |(nextP == Rampa teta && nextH == z)  || (nextP == Rampa (rot180 teta) && nextH == (z-1))                   = Free
                            |(nextH ==(z-1) || nextH == z) && (nextP == Rampa (rot180 (rot270 teta)) || nextP == Rampa (rot270 teta)  ) = Free 
                            | nextP == Rampa (rot180 teta) && nextH == z                                                                = Wall  
                            | nextH == z &&   (nextP == Curva teta || nextP == Curva (rot270 teta) )                                    = Free 
                            | nextH < z                                                                                                 = Fall 
                            | otherwise                                                                                                 = Burn 
                                          where nextP = peek (x,y) l
                                                nextH = peekHei (x,y) l

{-|
      Esta função tem o mesmo objetivo que a função forwardcheck no entanto o seu domínio é apenas Pecas do tipo Rampa. Em certos casos fá-lo com a função forward check.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> upwardcheck Este (0,1) t 1 Norte
      Trap

      >>> upwardcheck Sul (1,1) t 1 Oeste
      Fall


-}
upwardcheck :: Orientacao -> Posicao -> Bidimlist -> Altura -> Orientacao -> Parede
upwardcheck teta (x,y) l z dirR   | teta == dirR                                                                           = forwardcheck teta (x,y) l (z+1)                                     
                                  | teta == rot180 dirR                                                                    = forwardcheck teta (x,y) l z                                         
                                  | nextP == Recta && (nextH == z || nextH == (z+1) )                                      = Free 
                                  | (nextP == Curva teta || nextP == Curva (rot270 teta) )&& (z== nextH || (z+1) == nextH) = Free 
                                  | nextP == Rampa teta && (nextH == z || nextH == (z+1) )                                 = Free
                                  | nextP == Rampa (rot180 teta) && ( nextH == z || nextH == (z-1) )                       = Free           
                                  | (nextP == Rampa dirR || nextP == Rampa (rot180 dirR) )&& nextH == z                    = Free 
                                  | nextP == Rampa dirR && nextH == z-1                                                    = Fall
                                  | ( nextP == Rampa (rot180 dirR) ) && nextH == z-1                                       = Trap
                                  | nextH < z                                                                              = Fall 
                                  | nextH > z                                                                              = Wall
                                  | otherwise                                                                              = Burn 
                                                       where nextP = peek (x,y) l
                                                             nextH = peekHei (x,y) l
-- ======================================================================

    
-------------------------------------------------------------------------
-----------------------   AGIR     --------------------------------------
{-|
      Recebendo a parede de uma aresta determina o que acontece ao carro. 
      Em casos em que o carro continua após intersetar-se com a aresta introduzida é atualizado o 'Carro' e repetido todo o processo
      com a função 'branch'. 
-}  

decide :: Bidimlist -> Parede -> (Int, Int) -> Carro -> Tempo ->Maybe Carro
decide _ Burn _ _ _                              = Nothing
decide _ Fall _ _ _                              = Nothing
decide tab k o vi ti    | k == Trap              = unTrapable (snd tau) tab ti vi o
                        | k == Wall              = branch tab (Carro (nPosC ( fst tau ) o ) (direcao vi) (collision (velocidade vi) o) ) (snd tau) (pelc vi) 
                        | k == Free              = branch tab (Carro (nPosF (fst tau) o ) (direcao vi) (velocidade vi)) ( snd tau) ( (\(a,b) (c,d) -> (a+c, b+d) ) (pelc vi) o)
                        | otherwise              = Just (Carro (segue (velocidade vi) ti (posicao vi) ) (direcao vi) (velocidade vi) ) 
                                              where tau = (segue (velocidade vi) ndt (posicao vi) , ti - ndt )         
                                                            where ndt = intersecT (velocidade vi) (dif vi) o 
-- ()
{-|
      Esta função subtrai um valor muito pequeno à posição final ( no dominio desta função ) de forma 
      a que a função branch após receber este carro considere que ele continua na mesma 'Peca'. 

      == Exemplos de utilização:

      >>> nPosC (4.2,8) (-1,0)
      (4.20000000000001,8.0)

      >>> nPosC (1.2,0.3333333) (0,1)
      (1.2,0.33333329999999)
-}                                                            
nPosC :: Ponto->(Int,Int)->Ponto                                                  
nPosC (x,y) (v1,v2) =( x - 0.00000000000001* fromIntegral v1, y - 0.00000000000001* fromIntegral v2 )  

{-|
      Esta função soma um valor muito pequeno à posição final (no dominio desta função) de forma a que a
      função branch após receber o carro da função decide considere que ele está em outra Peca.         

      == Exemplos de utilização:

      >>> nPosF (4.2,8) (-1,0)
      (4.19999999999999,8.0)

      >>> nPosF (1.2,0.3333333) (0,1)
      (1.2,0.33333330000001)
-}
nPosF :: Ponto->(Int,Int)->Ponto                                                  
nPosF (x,y) (v1,v2) =( x + 0.00000000000001* fromIntegral v1, y + 0.00000000000001* fromIntegral v2 ) 

{-|
      Esta função multiplica o tempo pelo vetor velocidade e soma o vetor posição para obter a nova posição.

      == Exemplos de utilização:

      >>> segue (3,4) 0.5 (2,1)
      (3.5, 3.0)

      >>> segue (1.2,0.3333333) 7.8 (0,1)
      (9.36,3.59999974)
      
-}
segue :: Num a => (a,a) -> a -> (a,a) -> (a,a)   
segue (vx,vy) t (xo,yo) = (vx*t + xo ,  vy*t + yo )
{-| 
      Esta função visa calcular o vetor velocidade após uma colisão 

      == Exemplos de utilização:

      >>> collision (3,4) (0,1)
      (3.0,-4.0)

      >>> collision (1.2,0.3333333) (1,-1)
      (-0.3333333,-1.2)
-}
collision :: Velocidade -> (Int,Int) -> Velocidade
collision (vx,vy) (0,_)   = (vx  ,-vy ) 
collision (vx,vy) (_,0 )  =(-vx , vy )
collision (vx,vy) (1,1)   = (vy ,  vx ) 
collision (vx,vy) (1,-1)  = (-vy , -vx )

{-|
      Esta função é dirigida à execução de movimentos relacionados com colisão de Carros nas Paredes diagonais das Curvas.
-}
dicInWall :: Bidimlist -> Carro -> ( Int , Int ) -> Altura -> Tempo -> Maybe Carro  
dicInWall tab vi o z ti  | z >= 0     = Nothing 
                         | otherwise  =  branch tab ( Carro  (  just_in_case val (retStone tab (pelc vi)) ) (direcao vi) (collision (velocidade vi) o) ) (ti - ndt) (pelc vi)
                                    where ndt = insecT (velocidade vi) (dif vi) o  
                                          val = segue (velocidade vi) ndt (posicao vi)
just_in_case :: Ponto -> Peca -> Ponto
just_in_case (x,y) (Peca (Curva o )_)   | o == Norte = (x +1e-4 ,y  + 1e-4)
                                        | o == Sul   = (x -1e-4 ,y  - 1e-4)
                                        | o == Este  =  (x - 1e-4 ,y  + 1e-4)
                                        | o == Oeste =  (x +1e-4 ,y  - 1e-4)

{-|
      Esta função foi feita para tratar um caso muito específico em que o carro vai numa rampa,
      paralelamente à orientação desta, para uma outra rampa com orientação oposta à rampa inicial cujo diferencial das alturas delas esta 
      compreendido no intervalo [0, 2] dependendo do ponto na aresta que as separa.  
-}                                    
unTrapable :: Tempo -> Bidimlist -> Tempo -> Carro -> (Int,Int) -> Maybe Carro
unTrapable at tab ti vi o | dir == Este  = if  fst (dif car) >= 0 && fst (dif car) < 0.5 then branch tab car (ti-at) ( s (pelc vi) o) else Nothing 
                          | dir == Oeste = if  fst (dif car) <= 1 && fst (dif car) > 0.5 then branch tab car (ti-at) ( s (pelc vi) o) else Nothing
                          | dir == Sul   = if  snd (dif car) <= 1 && snd (dif car) > 0.5 then branch tab car (ti-at) ( s (pelc vi) o) else Nothing 
                          | otherwise    = if  snd (dif car) >= 0 && snd (dif car) < 0.5 then branch tab car (ti-at) ( s (pelc vi) o) else Nothing 
            where car = Carro (segue (velocidade vi) at (posicao vi) ) (direcao vi) (velocidade vi)
                  (Rampa dir ) = peek (pelc vi) tab
                  s (x,y) (u,v) =(x+u, y+v) 
-------------------------------------------------------------------------
-------------------------------------------------------------------------