{-|
Module      : Tarefa1_2017li1g118
Description : Módulo que constroi a pista do jogo.
Copyright : Diogo Rio <diogorio53@hotmail.com>;
            Jorge Cerqueira <jorgejg575926@gmail.com>
Um Módulo para construir um mapa no ambito da UC de LI1
-}

module Tarefa1_2017li1g118 (

  -- * Funçao constroi
  constroi,testesT1,
  -- * Funções
  tabuleiroInit,
  -- ** Alterar Orientação de peças
  rodaPecaDir,rodaPecaEsq,
  -- *** Dependencias alterar orientações
  rodaTipoDir,rodaOriDir,rodaTipoEsq,rodaOriEsq,invertOri,
  nextStep,move,makePath,makeTabuleiro,swapPeca,swapTabuleiroPeca,
  )
where


import LI11718

testesT1 :: [Caminho]
testesT1 = []--listaDeTestes

{-| Esta função transforma um caminho em um mapa -}
constroi :: Caminho -> Mapa
constroi c = Mapa (partida c,Este) (makeTabuleiro c)


{- | Esta função roda a orientação de uma Peca para a direita.

   >>> rodaPecaDir (Peca (Rampa Sul) 1)
   Peca (Rampa Oeste) 1
-}
rodaPecaDir :: Peca -> Peca
rodaPecaDir (Peca x h) = Peca (rodaTipoDir x) h

{- | Esta função roda a orientação de um Tipo para a direita.

   >>> rodaTipoDir (Rampa Sul)
   Rampa Oeste
-}
rodaTipoDir :: Tipo -> Tipo
rodaTipoDir (Rampa x) = Rampa (rodaOriDir x)
rodaTipoDir (Curva x) = Curva (rodaOriDir x)
rodaTipoDir Recta = Recta
rodaTipoDir Lava = Lava

{- | Esta função roda uma orientação para a direita

   >>> rodaTipoDir Sul
   Oeste
-}
rodaOriDir :: Orientacao -> Orientacao
rodaOriDir Norte = Este
rodaOriDir Este  = Sul
rodaOriDir Sul   = Oeste
rodaOriDir Oeste = Norte

{- | Esta função roda a orientação de uma Peca para a direita.

   >>> rodaPecaDir (Peca (Rampa Sul) 1)
   Peca (Rampa Este) 1
-}
rodaPecaEsq :: Peca -> Peca
rodaPecaEsq (Peca x h) = Peca (rodaTipoEsq x) h

{- | Esta função roda a orientação de um Tipo para a direita.

   >>> rodaTipoDir (Rampa Sul)
   Rampa Este
-}
rodaTipoEsq :: Tipo -> Tipo
rodaTipoEsq (Rampa x) = Rampa (rodaOriEsq x)
rodaTipoEsq (Curva x) = Curva (rodaOriEsq x)
rodaTipoEsq Recta = Recta
rodaTipoEsq Lava = Lava

{- | Esta função roda uma orientação para a direita

   >>> rodaTipoDir Sul
   Oeste
-}
rodaOriEsq :: Orientacao -> Orientacao
rodaOriEsq Norte = Oeste
rodaOriEsq Oeste = Sul
rodaOriEsq Sul = Este
rodaOriEsq Este = Norte
{- Move uma Posição para uma nova posição dado uma Orientação, ter em atenção de que valores
   de uma posição têm de estar dentro dos limites do Mapa, por exemplo move (0,0) Norte será
   impossível pois não existe posição (0,-1)

-}

{- Move uma orientaçao em 180 graus
-}
invertOri :: Orientacao -> Orientacao
invertOri Norte = Sul
invertOri Sul   = Norte
invertOri Este  = Oeste
invertOri Oeste = Este

{- Dada uma Posiçao e uma Orientacao devolve uma nova posicao que é a Posicao movida para
   cima,baixo,esquerda,Direita dependendo da orientaçao
-}
move :: Posicao -> Orientacao -> Posicao
move (x,y) Norte |x>=0,y>0  = (x,y-1)
move (x,y) Sul   |x>=0,y>=0 = (x,y+1)
move (x,y) Este  |x>=0,y>=0 = (x+1,y)
move (x,y) Oeste |x>0,y>=0  = (x-1,y)

{- Devolve a Peca correspondente a uma certa Posicao,Orientaçao Altura e tambem a
   Posiçao,Orientaçao e Altura da peça seguinte dado certo Passo
-}
nextStep :: (Posicao,Orientacao,Altura) -> Passo -> (Peca,(Posicao,Orientacao,Altura))
nextStep ((x,y),ori,alt) Avanca   = ( Peca  Recta alt                        ,((move (x,y) ori)  ,ori,alt))
nextStep ((x,y),ori,alt) Sobe     = ((Peca (Rampa ori) alt)                  ,((move (x,y) ori)  ,ori,alt+1))
nextStep ((x,y),ori,alt) Desce    = ((Peca (Rampa (invertOri ori)) (alt-1))  ,((move (x,y) ori)  ,ori,alt-1))
nextStep ((x,y),ori,alt) CurvaDir = ((Peca (Curva ori) alt)                  ,((move (x,y) (rodaOriDir ori))  ,(rodaOriDir ori),alt))
nextStep ((x,y),ori,alt) CurvaEsq = ((Peca (Curva (rodaOriDir ori)) alt)     ,((move (x,y) (rodaOriEsq ori))  ,(rodaOriEsq ori),alt))

{- Dado um caminho devolve a Lista de Pecas que o compoem bem como a sua posiçao
-}
makePath :: Caminho -> [(Peca,Posicao)]
makePath steps = makePathAc (startPos,Este,0) steps []
                 where
                  startPos = partida steps
                  makePathAc _ [] l = l
                  makePathAc (pos,ori,alt) (step:steps) l = makePathAc (nextPos,nextOri,nextAlt) steps ((peca,pos):l)
                                                          where (peca,(nextPos,nextOri,nextAlt)) = nextStep (pos,ori,alt) step

{- Dado um Caminho devolve um Tabuleiro que contem as Pecas obtidas na funcao makePath e onde as restantes Pecas
   sao Pecas de Lava
-}
makeTabuleiro :: Caminho -> Tabuleiro
makeTabuleiro steps = makeTabuleiroH (reverse(makePath steps)) (tabuleiroInit (dimensao steps))
                      where
                        makeTabuleiroH [] ac = ac
                        makeTabuleiroH (step:steps) ac = makeTabuleiroH steps (swapTabuleiroPeca ac peca pos)
                                                       where (peca,pos) = step


{-
Função que inicia um Tabuleiro de dimensão (x,y) onde x corresponde ao número de colunas
da matriz e o y ao número de linhas

>>> tabuleiroInit (2,2)
[
[Peca Lava 0,Peca Lava 0],
[Peca Lava 0,Peca Lava 0]
]
-}
tabuleiroInit :: Dimensao -> Tabuleiro
tabuleiroInit (x,y) = tabuleiroInitY (tabuleiroInitX [] x) [] y
                    where
                    tabuleiroInitY :: [Peca] -> [[Peca]] -> Int -> [[Peca]]
                    tabuleiroInitY lX lAc 0 = lAc
                    tabuleiroInitY lX lAc y = let x=lX in
                                              tabuleiroInitY lX (lX:lAc) (y-1)

                    tabuleiroInitX :: [Peca] -> Int -> [Peca]
                    tabuleiroInitX lAc 0 = lAc
                    tabuleiroInitX lAc x = let lava = Peca Lava 0 in
                                         tabuleiroInitX (lava:lAc) (x-1)


{-
Função que troca uma Peca numa determinada posição de uma lista de Peças por uma nova Peca,
é de notar que o primeiro elemento da lista é considirado como posição 0

>>> swapPeca [Peca Lava 0,Peca Lava 0] (Peca (Rampa Sul) 0) 0
[Peca (Rampa Sul) 0, Peca Lava 0]
-}
swapPeca :: [Peca] -> Peca -> Int -> [Peca]
swapPeca (l:ls) p 0 = p:ls
swapPeca (l:ls) p x = l:(swapPeca ls p (x-1))

{-
Função que troca uma Peca de uma matriz por uma nova Peca, sendo que a primeira coluna e
linha são denotados por 0

>>> swapTabuleiro [[Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0]] (Peca (Rampa Sul) 0) 0 (0,0)
[[Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0]]
-}
swapTabuleiroPeca :: Tabuleiro -> Peca -> Posicao -> Tabuleiro
swapTabuleiroPeca l p (x,y) = swapTabuleiroAc l [] (x,y) p
               where
                 swapTabuleiroAc :: Tabuleiro -> [Peca] -> Posicao -> Peca -> Tabuleiro
                 swapTabuleiroAc l      lAc (0,0) p = ((p:lAc):l)
                 swapTabuleiroAc (l:ls) lAc (x,0) p = ((swapPeca l p x):ls)
                 swapTabuleiroAc (l:ls)  [] (x,y) p = l:(swapTabuleiroAc ls [] (x,y-1) p)

listaDeTestes :: [Caminho]
listaDeTestes = [
                [Avanca,Avanca,Sobe,Sobe,Desce,CurvaEsq,CurvaEsq,CurvaEsq,CurvaEsq,Avanca,CurvaDir,CurvaDir,CurvaEsq,Avanca,Sobe],
                [Sobe,CurvaDir,CurvaEsq,Desce,Avanca,CurvaDir,CurvaEsq,Desce,Desce,Avanca,Avanca],
                [Desce,CurvaEsq],
                [CurvaEsq,CurvaEsq,Avanca,Avanca,CurvaEsq,CurvaEsq,CurvaDir,Avanca,CurvaEsq,Sobe,CurvaEsq,Avanca,Desce,CurvaDir,CurvaEsq,Sobe,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,CurvaDir,Avanca,CurvaEsq],
                [CurvaDir,Avanca,Sobe,CurvaEsq,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,CurvaEsq,Sobe,Avanca,CurvaDir,CurvaDir,Sobe,Avanca,Sobe,Desce,CurvaEsq],
                [Desce,Desce],
                [Sobe],
                [Avanca,CurvaDir,Sobe,Desce,Sobe,CurvaEsq,CurvaEsq,CurvaEsq,Avanca,CurvaEsq,CurvaDir,Sobe,Sobe,Sobe,Sobe,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Desce,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Desce,CurvaEsq,Sobe],
                [CurvaEsq,Avanca,Avanca,CurvaEsq,Sobe,Desce,Avanca,Sobe,CurvaDir,CurvaDir,CurvaDir,CurvaEsq,Sobe],
                [CurvaEsq,Avanca,CurvaEsq,Avanca,Sobe,Desce,Desce,Sobe,CurvaEsq,Sobe,Avanca,Sobe,Desce,CurvaDir,Sobe,Desce,Desce,Sobe,CurvaDir],
                [Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,CurvaEsq,CurvaDir,Avanca,Avanca,Avanca]
                ]
