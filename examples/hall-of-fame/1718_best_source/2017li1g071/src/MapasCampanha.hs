module MapasCampanha where
import LI11718
import Tarefa1_2017li1g71
desceUm::Mapa->Mapa
desceUm (Mapa (pi,oi) tab) = (Mapa (pi,oi) (map baixaUm tab))
                           where baixaUm::[Peca]->[Peca]
                                 baixaUm [] = []
                                 baixaUm ((Peca tipo alt):t) = if tipo/=Lava
                                                               then (Peca tipo (alt-1)):baixaUm t 
                                                               else (Peca Lava 0):baixaUm t
mapasFG::[Mapa]
mapasFG = [(desceUm (Mapa ((5,4),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])),

           (desceUm (Mapa ((4,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])),

           (desceUm (Mapa ((4,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])),

           (desceUm (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]])),

           (desceUm (Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                                        [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
                                        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]))]
mapasQF::[Mapa]
mapasQF = [(Mapa ((5,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),
           (constroi [Avanca,Sobe,Desce,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Desce,Sobe,CurvaDir,Avanca,CurvaEsq,Avanca,Sobe,Sobe,Desce,Desce,CurvaDir,Avanca,CurvaDir,CurvaDir,CurvaEsq,CurvaEsq,CurvaDir,Avanca]),
           (desceUm (constroi [Avanca,Avanca,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,Avanca])),
           (constroi [Avanca,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,Desce,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Sobe,Avanca,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,Avanca,Sobe,Sobe,CurvaDir,Avanca,Desce,Desce,Desce,Desce,Sobe,Avanca,CurvaDir,Sobe,Sobe,Desce,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,CurvaDir,Avanca]),
           (constroi [Avanca,Avanca,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Sobe,Desce,Desce,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Sobe,Desce,Sobe,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Desce,Sobe,Desce,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Sobe])]

mapasSF::[Mapa]
mapasSF = [(Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),

           (Mapa ((3,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0,Peca Recta 1,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Sul) 1,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 1,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),
           
           (Mapa ((8,3),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
                               [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0],
                               [Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0],
                               [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]),
           (constroi [Avanca,CurvaDir,Sobe,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,Avanca,CurvaDir,CurvaEsq,CurvaEsq,Avanca,CurvaDir,CurvaEsq,Avanca,Desce,CurvaEsq,Avanca,CurvaEsq]),
           (constroi [Avanca,Sobe,Sobe,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,Desce,Desce,CurvaDir,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir])]