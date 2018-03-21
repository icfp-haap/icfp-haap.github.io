module TestesT4 (testesT4',carro01,mapaTesteParado,carroTesteParado)where

import LI11718
import Mapas
import Tarefa1_2017li1g118

testesT4' :: [(Tempo,Jogo,Acao)]
testesT4' = [
            (0.1,(Jogo mapaTesteParado testfAtrito [carroTesteVelocidade] [1] [[]]), (Acao False False False False Nothing)), --
            (0.1,(Jogo mapaTesteParado testefPneus [carroTesteFRoda] [1] [[]]), (Acao False False False False Nothing)),
            (0.1,(Jogo mapaTesteParado testefAcel  [carroTesteParado] [1] [[]]), (Acao True False False False Nothing)),
            (0.1,(Jogo mapaTesteSubida testefPeso [carroTesteParado] [1] [[]]), (Acao False False False False Nothing)), --
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade] [0] [[]]), (Acao False False False False (Just 0))), --
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade] [1] [[]]), (Acao False False False False (Just 0))),
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade,carroTesteVelocidade] [1,1] [[],[]]), (Acao False False False False (Just 0))),
            (0.1,(Jogo mapaTesteParado testeRoda [carroTesteParado] [1] [[]]), (Acao False False True False Nothing)),
            (0.1,(Jogo mapaTesteParado testfAtrito [carroTesteVelocidade2] [1] [[]]), (Acao False False False False Nothing)), --
            (0.1,(Jogo mapaTesteParado testefPneus [carroTesteFRoda2] [1] [[]]), (Acao False False False False Nothing)),
            (0.1,(Jogo mapaTesteParado testefAcel  [carroTesteParado2] [1] [[]]), (Acao True False False False Nothing)),
            (0.1,(Jogo mapaTesteSubida testefPeso [carroTesteParado2] [1] [[]]), (Acao False False False False Nothing)), --
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade2] [0] [[]]), (Acao False False False False (Just 0))), --
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade2] [1] [[]]), (Acao False False False False (Just 0))),
            (0.1,(Jogo mapaTesteParado testefNitro [carroTesteVelocidade2,carroTesteVelocidade2] [1,1] [[],[]]), (Acao False False False False (Just 0))),
            (0.1,(Jogo mapaTesteParado testeRoda [carroTesteParado2] [1] [[]]), (Acao False False True False Nothing)),
            (1/60,(Jogo mapaTesteParado propteste1 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/60,(Jogo mapaTesteParado propteste2 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/60,(Jogo mapaTesteParado propteste3 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/60,(Jogo mapaTesteParado propteste4 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/60,(Jogo mapaTesteParado propteste5 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/60,(Jogo mapaTesteParado propteste6 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste1 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste2 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste3 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste4 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste5 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0))),
            (1/30,(Jogo mapaTesteParado propteste6 [carrofullTeste] [1] [[]]), (Acao True False True False (Just 0)))
            ]


carro01 :: Carro
carro01 = Carro (2.0,1.0) 0 (1,0)

mapaTesteSubida  = constroi [Avanca,Desce,Desce,Desce,Desce,Desce,Desce,Desce,Desce,Sobe,Avanca,Avanca]
carroTesteSubida = (Carro (2.5,1.5) 0 (0,0))
carroTesteSubida2 = (Carro (2.5,1.5) 90 (0,0))

mapaTesteParado  = constroi [Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca]
carroTesteParado = (Carro (1.5,1.5) 0 (0,0))
carroTesteVelocidade = (Carro (1.5,1.5) 0 (1,0))
carroTesteFRoda = (Carro (1.5,1.5) (-45) (1,0))

carroTesteParado2 = (Carro (1.5,1.5) 45 (0,0))
carroTesteVelocidade2 = (Carro (1.5,1.5) 45 (1,0))
carroTesteFRoda2 = (Carro (1.5,1.5) 45 (1,0))
carrofullTeste = (Carro (1.5,1.5) 45 (1,1))

propteste1  = (Propriedades 2 0 0 0 0 0)
propteste2  = (Propriedades 2 2 0 0 0 0)
propteste3  = (Propriedades 2 2 2 0 0 0)
propteste4  = (Propriedades 2 2 2 2 0 0)
propteste5  = (Propriedades 2 2 2 2 2 0)
propteste6  = (Propriedades 2 2 2 2 2 2)
testfAtrito = (Propriedades 1 0 0 0 0 0)
testefPneus = (Propriedades 0 1 0 0 0 0)
testefAcel  = (Propriedades 0 0 1 0 0 0)
testefPeso  = (Propriedades 0 0 0 1 0 0)
testefNitro = (Propriedades 0 0 0 0 1 0)
testeRoda   = (Propriedades 0 0 0 0 0 1)