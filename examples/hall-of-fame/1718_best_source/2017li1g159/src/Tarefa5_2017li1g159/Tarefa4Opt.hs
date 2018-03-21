{-|
Module      : Tarefa4Opt
Description : Módulo da Tarefa 4 para LI1 17/18
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4Opt where

import LI11718

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [ 
             (0.3,Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0}, carros = [Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = False, esquerda = False, direita = False, nitro = Just 0})       
            ,(0.5,Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 5, k_pneus = 0, k_acel = 6, k_peso = 0, k_nitro = 0, k_roda = 180.0}, carros = [Carro {posicao = (2.7,1.5), direcao = 30.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = False, esquerda = False, direita = False, nitro = Just 0}) 
            ,(0.5,Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (2.7,1.5), direcao = 30.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = False, esquerda = False, direita = True, nitro = Just 0 }) -- Este tem de ser nitro. 
            ,(1.0,Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (2.7,1.5), direcao = 30.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]},Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Just 0}) 
            ,(0.15,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (8.7,16.5), direcao = 180.0, velocidade = (-1.0,0.0)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = False, esquerda = True, direita = False, nitro = Nothing}) 
            ,(0.15,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (8.7,16.5), direcao = 180.0, velocidade = (-1.0,0.0)}], nitros = [3.0], historico = [[]]},Acao {acelerar = True, travar = False, esquerda = True, direita = False, nitro = Just 0}) 
            ,(1.3,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (3.7,16.5), direcao = 180.0, velocidade = (-1.0,0.0)}], nitros = [3.0], historico = [[]]},Acao {acelerar = True, travar = False, esquerda = False, direita = True, nitro = Just 0}) 
            ,(1.3,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (3.7,16.5), direcao = 50.0, velocidade = (1.3,0.2)}], nitros = [3.0], historico = [[]]},Acao {acelerar = True, travar = False, esquerda = False, direita = True, nitro = Just 0}) 
            ,(3,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (3.7,18.5), direcao = 50.0, velocidade = (1.3,0.2)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}) 
            ,(3,Jogo {mapa = Mapa ((13,18),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 0, k_pneus = 0, k_acel = 4.0, k_peso = 0, k_nitro = 15.0, k_roda = 180.0}, carros = [Carro {posicao = (3.7,18.5), direcao = 50.0, velocidade = (1.3,0.2)}], nitros = [3.0], historico = [[]]},Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}) 

            ]

{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t jg id action = mixingVel t rdy id (direcao (carros jg !! id)) action
        where rdy = updateDir id t action (updateHist jg id)

{-|
Esta função destribui os argumentos por todas as funções auxiliares e estabelece a precedência destas.
-}
mixingVel :: Tempo -> Jogo -> Int -> Angulo ->Acao -> Jogo
mixingVel dt jg id prevA action = updateNitro action dt new id
    where new = cc { carros = give_Nitro dt (nitro action) ( nitros cc !! id) (pista cc) (carros cc) prevA }
          cc  = calVEL dt jg id prevA action       
{-|
Esta função verfica se o carro se encontra numa peça que é rampa, 
pois se for o caso o processamento das acelerações será diferente 
visto que se terâ que ter em consideração a aceleração gravítica.

A possibilidade de haver ou não haver uma rampa com uma certa orientação,
é representada com o manad Maybe no Quinto argumento da função 'new_velocity'.

-}  
calVEL :: Tempo -> Jogo -> Int -> Angulo -> Acao ->Jogo
calVEL dt jg id prevA action | b         = jg { carros =  replaceAtIndex id new1 (carros jg) }
                             | otherwise = jg { carros =  replaceAtIndex id new2 (carros jg) }
                                where (inf , b)   = isRamp (mapa jg) h
                                      h = carros jg !! id 
                                      new1 = h { velocidade =  new_velocity action (pista jg) h dt (Just inf) prevA}
                                      new2 = h { velocidade =  new_velocity action (pista jg) h dt  Nothing prevA }
{-|
Esta função é uma componente da função 'calVEL', pois recebendo um mapa e o respetivo carro
verifica se este encontra-se numa peça rampa. 
O resultado é um tuplo que contem na segunda componente um valor Boleano
com o valor lógico que corresponde à resposata à pergunta, "O carro encontra-se numa rampa ?". 

Na eventualidade da resposta ser 'False' então a orientação, correspondente à primeira componente do tuplo
não será relevante. ( Norte por convenção.)

Caso seja 'True' então a orientação da primeira componente corresponderá à orientação da componente da rampa em questão.

      ==Exemplos de utilização:
      Seja m = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                 [Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],
                                 [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],
                                 [Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],
                                 [Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],
                                 [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]] e
           c1 = Carro {posicao = (2.005,1.91), direcao = 20.0,velocidade = (2.4,-1.4)}
           c2 = Carro {posicao = (4.005,1.91), direcao = 20.0,velocidade = (2.4,-1.4)}
                     

      >>> isRamp m c1
      (Norte,False)

      >>> isRamp m c2
      (Norte,False)
       
-}
isRamp :: Mapa -> Carro ->( Orientacao , Bool)
isRamp (Mapa _ tab ) cc = let (u,v) = posicao cc in  belong (floor u, floor v) tab
        where belong :: (Int, Int) -> Tabuleiro -> (Orientacao, Bool)
              belong (x,y) tab = isR $ (tab !! y) !! x
                    where isR :: Peca -> (Orientacao, Bool)
                          isR (Peca (Rampa tp ) _) = (tp , True)
                          isR _                    = (Norte , False)

{-|
Esta função terá a tarefa de atribuir a aceleração correspondente ao nitro. 
Usa pattern matching para saber se o nitro é uma das ações a ser executadas pelo jogador.

O tempo usado para calcular a velocidade final resultante da acelaração do nitro,
corresponderá sempre ao valor míninmo entre o tempo do intervalo e o tempo restante de nitro. 
-}
give_Nitro :: Tempo -> Maybe Int -> Tempo ->Propriedades -> [Carro] -> Angulo -> [Carro]  
give_Nitro _ Nothing _ _ lcc w        = lcc
give_Nitro dt (Just id) nt prop lcc w = replaceAtIndex id ( ( lcc !! id ) { velocidade = aux (min dt nt) ( lcc !! id ) w (k_nitro prop ) } ) lcc
                                        
            where aux :: Tempo -> Carro-> Angulo -> Double -> Velocidade 
                  aux t (Carro _ wi v ) w k_n = soma  (trsclale (t*) (trsclale (k_n*) (get_vec  w ))) v 
                                   
{-|
Esta função procede à atualização dos tempo de nitros do jogo tendo em consideração se o nitro do jogador inserido foi usado ou não.
-}
updateNitro :: Acao -> Tempo -> Jogo -> Int ->Jogo
updateNitro action t jg id | nitro action /= Nothing = jg {nitros = replaceAtIndex id h (nitros jg) }
                           | otherwise               = jg 

                    where h = aux (nitros jg !! id)  (nitro action ) t
                          aux :: Tempo ->Maybe Int -> Tempo->Tempo
                          aux 0 _ _        = 0
                          aux c Nothing _  = c
                          aux c (Just _) p = if new < 0 then 0 else new 
                                 where new = c-p    

{-|
Esta função faz a atualização do histórico do respetivo carro.
A estratégia usada foi a de verificar se na cabeça da lista se encontrava uma posição igual à que o carro se encontra.

Caso seja no afirmativo, então significa que o histórico se encontra atualizado.
No caso contráro, então teremos de adicionar a posição atual ao histórico.

      ==Exemplos de utilização:
      Seja jg = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
                      pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
                      carros = [Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)},Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}],
                      nitros = [3.0], historico = [[(3,1)], []]}.
           

      >>> updateHist jg 0
      Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
            carros = [Carro {posicao = (2.005,1.91), direcao = 20.0,velocidade = (2.4,-1.4)},Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}],
            nitros = [3.0], historico = [[(2,1),(3,1)],[]]}

      >>> updateHist jg 1 
      Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
            carros = [Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)},Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}],
            nitros = [3.0], historico = [[(2,1),(3,1)],[]]}

-}
updateHist :: Jogo -> Int -> Jogo
updateHist state id =  state { historico = replaceAtIndex id (f (carros state !! id) (historico state!! id)) (historico state) }
                where f :: Carro -> [Posicao] -> [Posicao]
                      f  c []   = let (u,v) = posicao c  in  [trsclale floor (u,v)] 
                      f  c path = let (u,v) = posicao c  in check path (trsclale floor (u,v)) 
                                   -- where 
check :: [Posicao] -> Posicao -> [Posicao]
check [h] r | h==r      = [h]
            | otherwise = [r,h]
check (h:x:t) r | x == r    = x:t
                | h /= r    = r:h:x:t
                | otherwise = h:x:t

{-|
Esta função em conjunto com a informação correspondente as propriedades da pista
executa a função que processa a nova direção tendo em conta as ações a ser executadas.

      ==Exemplos de utilização:
      Seja a1 = ( Acao {acelerar = True , travar = False , direita = True , esquerda = False, nitro = Nothing  } ), 
           a2 = ( Acao {acelerar = False , travar = True , direita = False , esquerda = True, nitro = Just 0  } ) e
           jg = Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
                      pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
                      carros = [Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]}.
           

      >>> updateDir 0 3 a2 jg
      Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
            carros = [Carro {posicao = (2.005,1.91), direcao = 35.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]}

      >>> updateDir 0 2.3 a1 jg 
      Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],
            pista = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
            carros = [Carro {posicao = (2.005,1.91), direcao = 110.0, velocidade = (2.4,-1.4)}], nitros = [3.0], historico = [[]]}

-}
updateDir :: Int -> Tempo -> Acao -> Jogo -> Jogo
updateDir n t action jg = jg {carros = replaceAtIndex n new (carros jg) } 
        where new = h { direcao =  novaDirecao action (pista jg) t (direcao h ) }
              h   = carros jg !! n

{-|
Esta função é a que calcula o novo Angulo resultante de multiplicar o fator 'k_roda' pelo tempo
e somar à direção atual.

      ==Exemplos de utilização:
      Seja p  = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
           a1 = ( Acao {acelerar = True , travar = False , direita = True , esquerda = False, nitro = Nothing  } ) e 
           a2 = ( Acao {acelerar = False , travar = True , direita = False , esquerda = True, nitro = Just 0  } ).
           

      >>> novaDirecao a1 p 3 30
      300

      >>> novaDirecao a2 p 2.3 136
      121

-}              
novaDirecao :: Acao ->Propriedades ->Tempo-> Angulo -> Angulo
novaDirecao a settings t dir | esquerda a == direita a = dir
                             | esquerda a              = outfix $ dir + k_roda settings * t 
                             | direita a               = outfix $ dir - k_roda settings * t 

{-|
Esta é uma das mais fundamentais funções desta Tarefa, pois é nela que a grande maioria das velocidades são calculadas.

No total, após concluida está função devolverá as velocidade correspondete as 4 acelerações, atrito perpendicular aos pneus,
acelerção do motor, aceleração gravítica e atrito das rodas.

      ==Exemplos de utilização:
      Seja p  = Propriedades {k_atrito = 0.0, k_pneus = 0.0, k_acel = 0.0, k_peso = 0.0, k_nitro = 4.0, k_roda = 150.0},
           a1 = ( Acao {acelerar = True , travar = False , direita = True , esquerda = False, nitro = Nothing  } ),
           a2 = ( Acao {acelerar = False , travar = True , direita = False , esquerda = True, nitro = Just 0  } ) e
           c = Carro {posicao = (2.005,1.91), direcao = 20.0, velocidade = (2.4,-1.4)}.

      >>> new_velocity a1 p c 3 (Just Norte) 30
      (2.4,-1.4)

      >>> new_velocity a2 p c 2.3 Nothing 136
      (2.4,-1.4)

-}
new_velocity :: Acao-> Propriedades ->Carro -> Tempo ->Maybe Orientacao -> Angulo ->Velocidade
new_velocity action prop cc t or prevA | or == Nothing = soma (trsclale (t*) (soma (soma a b) c ) ) (velocidade cc)
                                       | otherwise     = soma (trsclale (t*) (soma (soma (soma a b) c ) d) ) (velocidade cc)
                                             where a = acPneus ( k_pneus prop ) prevA (velocidade cc)
                                                   b = acAtrito ( k_atrito prop ) (velocidade cc) 
                                                   c = acAcel action (k_acel prop) prevA 
                                                   d = acgravitica (k_peso prop) (beJust or)                                
{-|
acPneus é a função que executa as computações correspondetes ao cálculo da acelaração do atrito dos pneus perpendicular à direção do carro. 

      ==Exemplos de utilização:

      >>> acPneus 12  80 (-1,45)
      (-80.70728297321529,-14.23087152785075)

      >>> acPneus 6 240 (0.121,1.3333)
      (-4.008515012597378,-2.3143172215737553)

-}
acPneus :: Double -> Angulo ->Velocidade -> (Double , Double)
acPneus k_pn iw (vx,vy) = (kon * cos ( head l + pi/2) , (-kon) * sin (head l + pi/2))
                         

                          where kon = norm* k_pn * sin (head l - last l)
                                l = [ pi/180 * iw , (pi/180) * arg ]
                                arg = calAngulo (vx,vy)
                                norm = sqrt ( vx^2 + vy^2)                 
{-|
acAtrito é a função que executa as computações correspondetes ao cálculo da acelaraçao do atrito no carro. 

      ==Exemplos de utilização:

      >>> acAtrito 12 (-1,45)
      (12.0,-540.0)

      >>> acAtrito 6 (0.121,1.3333)
      (-0.726,-7.9998)


-}
acAtrito :: Double -> Velocidade -> (Double , Double)
acAtrito k_at (x,y) = trsclale ((-k_at ) *) (x,y) 
                     --   where unit = trsclale ((1/norm)*) (x,y) 
                       --       norm = sqrt (x*x + y*y)
{-|
acGravitica é a função que executa as computações correspondentes ao cálculo da aceleração da força gravítica.

      ==Exemplos de utilização:

      >>> acgravitica 12 Norte
      (-0.0,12.0)

      >>> acgravitica 6 Oeste
      (6,0)

-}
acgravitica :: Double -> Orientacao -> Velocidade
acgravitica k_p o = trsclale ((- k_p) *) dram
                where dram = vetor_declive_rampa o  

{-|
acAcel é a função que executa as computações correspondetes ao cálculo da acelaração do motor do carro. 

      ==Exemplos de utilização:

      >>> acAcel ( Acao {acelerar = True , travar = False , direita = True , esquerda = False, nitro = Nothing  } ) 4 88
      (0.13959798681000432,-3.997563308076383)

      >>> acAcel ( Acao {acelerar = False , travar = True , direita = False , esquerda = True, nitro = Just 0  } ) 2 110
      (0.6840402866513374,1.8793852415718169)

-}
acAcel :: Acao ->Double-> Angulo -> (Double, Double)
acAcel action k_ac w | travar action == acelerar action = (0,0)
                     | acelerar action = let (a,b) = get_vec w in trsclale (k_ac *) (a,b)
                     | otherwise       = let (a,b) = get_vec w in trsclale ((-k_ac) *) (a,b)


{-|
Por vezes as diferentes funções deste programa calculam ângulos com valores que podem passar os 360 graus
e por isso , esta função converte qualquer ângulo para o valor corresponde a esse ângulo no interval [0,360].

      ==Exemplos de utilização:

      >>> outfix 402.33
      42.33

      >>> outfix -55.4
      304.6

-}

outfix :: Angulo -> Angulo
outfix x | x<0 = outfix (360 + x )
         | x>360 = outfix (x - 360) 
         | otherwise = x 
{-|
Esta função é uma função de ordem superior que recebe uma função e um tuplo e aplica essa função a cada elemento desse tuplo.

      ==Exemplos de utilização:

      >>> trsclale (4*) (1,1)
      (4,4)

      >>> trsclale (length) ([3,2,1],[6])
      (3,1)

-}
trsclale :: (a->b) -> (a, a)->(b,b)
trsclale f (x,y) = (f x , f y)            
{-|
Esta função substitui um elemento de uma por outro no indice indicado.

      ==Exemplos de utilização:

      >>> replaceAtIndex 2 10 [0,0,0,0,0]
      [0,0,10,0,0]

      >>> replaceAtIndex 4 11 [3,1,2,6,3]
      [3,1,2,6,11]

-}
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

{-|
Esta função recebe um ângulo e devolve o vetor de comprimento 1 com essa direção. 

      ==Exemplos de utilização:

      >>> get_vec 37
      (0.7986355100472928,-0.6018150231520483)

      >>> get_vec 62.05
      (0.46870086699128183,-0.8833569478311816)

-}
get_vec :: Angulo -> (Double,Double)
get_vec w = ( cos ( w /180 * pi ) , - sin ( w / 180 * pi ) )

{-|
Esta função recebe uma Orientação correspondente a uma rampa e devolve o vetor unitário que representa a direção dessa rampa.

      ==Exemplos de utilização:

      >>> vetor_declive_rampa Norte
      (0,-1)

      >>> vetor_declive_rampa Sul
      (0,1)

-}
vetor_declive_rampa :: Orientacao -> (Double,Double)
vetor_declive_rampa Norte = (0 , -1)
vetor_declive_rampa Sul   = (0 ,  1)
vetor_declive_rampa Este  = (1 ,  0)
vetor_declive_rampa Oeste = (-1,  0)

{-|
Esta função recebe uma qualquer 'Velocidade' e calcula o angulo desta. 

      ==Exemplos de utilização:

      >>> calAngulo (-3,2)
      213.69006752597977

      >>> calAngulo (3,6)
      296.565051177078

-}
calAngulo :: Velocidade -> Double
calAngulo (vx,vy) | vx>0 && vy>0   = 360 - atan (vy/vx) * 180/pi  -- outfix(180/pi  * (-1 )* atan (vy/vx)) *  pi/180
                  | vx<0 && vy<0   = 180 - atan (vy/vx) *180/pi 
                  | vx>0 && vy<0   = (-1)* atan (vy/vx) * 180/pi -- (pi/180  *)  $ outfix $ (atan (vy/vx) + pi/2 )* 180/pi
                  | vx<0 && vy>0   = 180 + (-1) * atan (vy/vx) *  180/pi
                  | vx==0 && vy > 0 = 270
                  | vx==0 && vy < 0 = 90
                  | vx >= 0 && vy==0 = 0 
                  | vx < 0 && vy==0 = 180
{-|
Esta função faz a soma de dois vetores bidimensionais.  

      ==Exemplos de utilização:

      >>> soma (2,1) (-1,3)
      (1,4)

      >>> soma (2,7) (0,3)
      (2,10)

-}
soma :: Num a => (a,a) -> (a,a) -> (a,a)                                         
soma (x1,x2) (y1,y2) = (x1+y1, x2+y2) 

{-|
Esta função recebe um tipo maybe a e assume que o elemento em questão é um Just a e devolve esse a.

      ==Exemplos de utilização:

      >>> beJust Just Norte
      Norte

      >>> beJust Just 4
      4

-}
beJust :: Maybe a -> a
beJust (Just x)  = x

