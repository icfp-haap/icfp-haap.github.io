module Tarefa1_2017li1g71 where

import LI11718

testesT1 :: [Caminho]
testesT1 = [[Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Sobe,Avanca,Desce,Avanca,CurvaDir],
            [Avanca,CurvaEsq,CurvaEsq,CurvaEsq,Avanca,CurvaDir,CurvaDir,CurvaDir],
            [Avanca,Avanca,CurvaDir,Avanca,CurvaDir,CurvaDir,Sobe,Avanca,Desce,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq],
            [],
            [CurvaEsq,CurvaEsq,CurvaEsq,Avanca,Avanca,Desce,Sobe,CurvaEsq,CurvaDir,Desce,Sobe],
            [Sobe,CurvaEsq,CurvaDir,Desce,Avanca,CurvaDir,Sobe,CurvaDir,Desce,CurvaEsq,Avanca,CurvaDir,Sobe,CurvaEsq,Desce,Avanca],
            [CurvaDir,Sobe,Desce,CurvaDir,CurvaDir,Avanca,Sobe,Sobe,Desce,Avanca,CurvaDir,Desce,Avanca],
            [Desce,Sobe,CurvaDir,Sobe,Sobe,CurvaEsq,Avanca,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,Avanca],
            [Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir,Avanca],
            [Sobe,Desce,Desce,Sobe,Sobe,Sobe,Desce,CurvaDir,CurvaEsq,Sobe,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq],
            [CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq],
            [Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,
            Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,
            CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca],
            [Sobe,Desce,Sobe,Sobe,Sobe,Sobe,Desce,CurvaEsq,CurvaDir,Sobe],
            [Avanca,CurvaDir,CurvaEsq,Avanca,Desce,Desce,Sobe,Sobe,Avanca,Avanca]]

{- | O type Bloco é simplesmente uma tupla de 3 elementos:
    -O tipo de peça presente nesse bloco.
    -A posição em que está esse bloco.
    -A orientação desse bloco, ou seja, a orientação que o carro terá ao passar naquele bloco.
    Os blocos têm por fim facilitar a determinação de funções agrupando dados importantes para as
    mesmas juntos.  -}
type Bloco = (Peca,Posicao,Orientacao)
{- | A função oriCon define qual a orientação
 contrária à orientação dada. -}
oriCon::Orientacao->Orientacao
oriCon o = case o of 
           Norte -> Sul
           Sul -> Norte
           Este -> Oeste
           Oeste -> Este
{- | A função oriDir define qual a orientação
 resultante quando uma certa orientação roda à direita-}
oriDir::Orientacao->Orientacao
oriDir o = case o of
            Norte -> Este
            Sul -> Oeste
            Este -> Sul
            Oeste -> Norte
{- | A função oriEsq define qual a orientação
 resultante quando uma certa orientação roda à esquerda -}
oriEsq::Orientacao->Orientacao
oriEsq o = case o of
            Norte -> Oeste
            Sul -> Este
            Este -> Norte
            Oeste -> Sul
{- | A função blocoDir define qual o bloco resultante da rotação à 
direita de um outro bloco-}
blocoDir::(Bloco)->(Bloco)
blocoDir ((Peca p0 alt),pos,o) =((Peca novap alt),move pos o,(oriDir o))
                               where novap = (Curva o)
{- | A função blocoEsq define qual o bloco resultante da rotação à 
esquerda de um outro bloco-}
blocoEsq::(Bloco)->(Bloco)
blocoEsq ((Peca p0 alt),pos,o) = ((Peca novap alt),move pos o,(oriEsq o))
                               where novap = (Curva (oriDir o))
{- | A função blocoSobe define qual o bloco resultante da rampa de subida 
de um outro bloco-}
blocoSobe::(Bloco)->(Bloco)
blocoSobe ((Peca p0 alt),pos,o) =  ((Peca novap alt),move pos o,o)
                                where novap = (Rampa o)
{- | A função blocoDesce define qual o bloco resultante da rampa de subida 
de um outro bloco-}
blocoDesce::Bloco->Bloco
blocoDesce ((Peca p0 alt),pos,o) =  ((Peca novap (alt-1)),move pos o,o)
                                where novap = (Rampa (oriCon o))
{- | A função blocoDesce define qual o bloco resultante do avanço 
de um outro bloco-}                                         
blocoAvanca::Bloco->Bloco
blocoAvanca ((Peca p0 alt),pos,o) = ((Peca Recta alt),move pos o,o)
-- | A função mudaAltura muda a altura de um bloco, aidicionando um valor n a essa mesma altura
mudaAltura::Bloco->Int->Bloco
mudaAltura ((Peca p alt),pos,o) 0 = ((Peca p alt),pos,o) 
mudaAltura ((Peca p alt),pos,o) n = ((Peca p (alt+n)),pos,o) 
-- | A função move recebe uma posição e dada uma certa orientação altera a posição inicial
move::Posicao->Orientacao->Posicao
move (x,y) o = case o of
               Norte -> (x,(y-1))
               Sul -> (x,(y+1))
               Este -> ((x+1),y)
               Oeste -> ((x-1),y)
{- | A função tabuleiroInit recebe uma dimensão e cria um tabuleiro com essas 
dimensões em que todos os elementos são (Peça Lava 0) -}
tabuleiroInit::Dimensao->Tabuleiro
tabuleiroInit (x,y) = replicate y (replicate x (Peca Lava 0))
{- | A função changeListElem muda um elemento de uma lista dado
o elemento que se pretende inserir e o index em que se pretende introduzir esse elemento-}
changeListElem::a->Int->[a]->[a]
changeListElem n 0 (x:xs) = n:xs
changeListElem n i (x:xs) = x:changeListElem n (i-1) xs
{- | A função mPecap, altera a peça de uma determinada posição num tabuleiro,
para isso recebe um par com a peça que se pretende introduzir no tabuleiro e a posição em que
esta vai ficar ,e o tabuleiro na qual vai ser colocada.  -}
mPecap::Bloco->Tabuleiro->Tabuleiro
mPecap (p,(x,y),_) t = changeListElem (changeListElem p x (t!!y)) y t
-- | A função prim_peca devolve o bloco inicial de um dado caminho
prim_peca::Caminho->Bloco
prim_peca (f:rs) = (peca_inicial,cord,nova_o)
                 where peca_inicial = case f of 
                                      Avanca -> (Peca Recta 0)
                                      Sobe -> (Peca (Rampa Este) 0)
                                      Desce -> (Peca (Rampa Oeste) (-1))
                                      CurvaDir -> (Peca (Curva Este) 0)
                                      CurvaEsq -> (Peca (Curva Sul) 0)
                       cord = partida(f:rs)
                       nova_o = case f of
                                CurvaDir -> oriDir Este
                                CurvaEsq -> oriEsq Este
                                _ ->Este
{- | A função caminhoToBlocos recebe um bloco inicial e um caminho e partir dai devolve uma lista
de blocos correspondente ao percurso do caminho dado -}
caminhoToBlocos::Bloco->Caminho->[Bloco]
caminhoToBlocos b [] = [b]
caminhoToBlocos ((Peca p0 alt),pos,o) (p:c) = case p of --Para alterar o bloco seguinte conforme o passo dado
                                             Avanca ->((Peca p0 alt),pos,o):caminhoToBlocos (blocoAvanca(mudaAltura ((Peca p0 alt),pos,o) m)) c
                                             Desce ->((Peca p0 alt),pos,o):caminhoToBlocos (blocoDesce(mudaAltura ((Peca p0 alt),pos,o) m)) c
                                             Sobe ->((Peca p0 alt),pos,o):caminhoToBlocos (blocoSobe(mudaAltura ((Peca p0 alt),pos,o) m)) c
                                             CurvaDir -> ((Peca p0 alt),pos,o):caminhoToBlocos (blocoDir(mudaAltura ((Peca p0 alt),pos,o) m)) c
                                             CurvaEsq -> ((Peca p0 alt),pos,o):caminhoToBlocos (blocoEsq(mudaAltura ((Peca p0 alt),pos,o) m)) c
                                           where m | (p0==(Rampa o)) = 1 -- caso da peça anterior ser uma rampa com a mesma orientação do movimento, aumentando assim a altura
                                                   | otherwise = 0  -- caso de qualquer outra peça                                  

{- | A função blocosToTabuleiro devolve um tabuleiro depois de lhe ser dada uma lista de blocos
e o tabuleiro em que se pretende introduzir esses blocos-}
blocosToTabuleiro::[Bloco]->Tabuleiro->Tabuleiro
blocosToTabuleiro [] t = t
blocosToTabuleiro (b:bs) t = blocosToTabuleiro bs (mPecap b t)  
{- | A função constroi recebe um caminho e devolve um Mapa-}                                              
constroi::Caminho->Mapa
constroi [] = Mapa ((partida []), Este) (tabuleiroInit(dimensao []))
constroi (p:c) = Mapa ((partida (p:c)), Este) (blocosToTabuleiro (caminhoToBlocos (prim_peca (p:c)) c) (tabuleiroInit(dimensao (p:c))))
