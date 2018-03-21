module Bots where
import Tarefa6_2017li1g71
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import Tarefa3_2017li1g71
import Tarefa4_2017li1g71
import Mapas
import LI11718
import Graphics.Gloss
import GHC.Float
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy

botFacil :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado a tual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao  -- ^ a decisão tomada pelo /bot/
botFacil tick jog@(Jogo m@(Mapa (pI,oi) tab) pro@(Propriedades fAtri fPn fAcel fPe fN fR) cars n h) j = acaoF
                                                                                         where posNor = (guardaPosVal m ((getPeca tab pI),pI) oi) -- Lista de posições do caminho 
                                                                                               angulosS = (listaAng tab pI oi posNor (posAng pro)) -- Lista dos angulos 
                                                                                               histo = reverse (h!!j) -- histórico do jogador
                                                                                               angPeca = if (numO posA histo) > (numO posA posNor)
                                                                                                         then angulosS!!(obP (numO posA posNor) posA posNor)
                                                                                                         else angulosS!!(obP (numO posA histo) posA posNor)
                                                                                               oriP = toOri angPeca
                                                                                               dir = -reduz(toRadian (direcao carroJ))
                                                                                               acaoA = aproxima dir angPeca
                                                                                               acaoC = aproxima dir (centra angPeca (posicao carroJ))
                                                                                               acaoF = if centrado (tipoEspaco pA) (posicao carroJ) posA (toOri angPeca) || tipoEspaco(getPeca tab pP)==Metade
                                                                                                       then if pro/= gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoA) carroJ pro  (2,True)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoA) carroJ pro  (2,True)
                                                                                                       else if pro/=gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoC) carroJ pro  (2,True)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoC) carroJ pro  (2,True)
                                                                                               carroJ = cars!!j
                                                                                               pA =  (whichPeca (posicao carroJ) tab)
                                                                                               posA = pontoPos (posicao carroJ)
                                                                                               novoM = (Mapa (pI,oi) (poeZero tab))
                                                                                               pP = if (numO posA histo) > (numO posA posNor)
                                                                                                         then posNor!!((obP (numO posA posNor) posA posNor)+1)
                                                                                                         else posNor!!((obP (numO posA histo) posA posNor)+1)
                                                                                               
          
botMedio :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado a tual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao  -- ^ a decisão tomada pelo /bot/
botMedio tick jog@(Jogo m@(Mapa (pI,oi) tab) pro@(Propriedades fAtri fPn fAcel fPe fN fR) cars n h) j = acaoN
                                                                                         where posNor = (guardaPosVal m ((getPeca tab pI),pI) oi) -- Lista de posições do caminho 
                                                                                               angulosS = (listaAng tab pI oi posNor (posAng pro)) -- Lista dos angulos 
                                                                                               histo = reverse (h!!j) -- histórico do jogador
                                                                                               angPeca = if (numO posA histo) > (numO posA posNor)
                                                                                                         then angulosS!!(obP (numO posA posNor) posA posNor)
                                                                                                         else angulosS!!(obP (numO posA histo) posA posNor)
                                                                                               oriP = toOri angPeca
                                                                                               dir = -reduz(toRadian (direcao carroJ))
                                                                                               acaoA = aproxima dir angPeca
                                                                                               acaoC = aproxima dir (centra angPeca (posicao carroJ))
                                                                                               acaoF = if centrado (tipoEspaco pA) (posicao carroJ) posA (toOri angPeca) || tipoEspaco(getPeca tab pP)==Metade
                                                                                                       then if pro/= gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoA) carroJ pro  (3,True)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoA) carroJ pro  (3,True)
                                                                                                       else if pro/=gelo
                                                                                                            then vMin tab angPeca (limV m pro (0.55) carroJ acaoC) carroJ pro  (3,True)
                                                                                                            else vMin tab angPeca (limV m pro (2) carroJ acaoC) carroJ pro  (3,True)
                                                                                               carroJ = cars!!j
                                                                                               pA =  (whichPeca (posicao carroJ) tab)
                                                                                               posA = pontoPos (posicao carroJ)
                                                                                               acaoN =  if null histo
                                                                                                        then acaoF
                                                                                                        else daNitro jog j (findFirst h m j (length cars)) acaoF 0.25 posNor
                                                                                               pP = if (numO posA histo) > (numO posA posNor)
                                                                                                         then posNor!!((obP (numO posA posNor) posA posNor)+1)
                                                                                                         else posNor!!((obP (numO posA histo) posA posNor)+1)
          