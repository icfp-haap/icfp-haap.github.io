{-|
Module      : Tarefa5_2017li1g71
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import Tarefa3_2017li1g71
import Tarefa4_2017li1g71
import Tarefa6_2017li1g71
import Mapas
import LI11718
import Graphics.Gloss
import GHC.Float
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Data.Char
import System.Random
import Bots
import StateOperator
import Imagens
import Data.List
import MapasCampanha


-----------------------------------------------------------------------------
--                                                                         --
-- ESTADO DO JOGO                                                          --
--                                                                         --
-----------------------------------------------------------------------------
{- | O estado do jogo é constitúido por um jogo, que é a proxima corrida a correr,
uma lista de imagens, que são todos os aspetos gráficos do jogo,
um conjunto de Bools que definem a ação do jogador (incluindo Nitros),
uma lista de Int que possui todas as variavéis inteiras do jogo,
uma lista com o número de voltas dadas por cada jogador,
uma lista com os indices dos carros aleatórios a correr,
um float para contar o tempo que passou,
e uma lista de Double que conta o tempo que passou desde a ultima morte de um dado jogador,
uma lista de Mapas a jogar no modo campanha,
e uma Lista de Bool para saber várias coisas sobre o jogo,
uma lista de listas de inteiros para indentificar os jogadores em cada eliminátoria,
e uma Mapa para jogar o modo de corrida rápida,
e uma lista de posições no final de cada corrida. -}
type Estado = (Jogo,[[Picture]],[(Bool,Bool,Bool,Bool,Maybe Int)],[Int],[Int],Float,[Double],[Mapa],[Bool],[[Int]],Mapa,[Int],[String],Propriedades,[Propriedades]) -- Jogo,Imagens,Ação,Variaveis inteiras,Número de Voltas por Jogador, Carros a correr
-- | A função wnd cria a janela na qual vai correr o jogo, tendo a janela uma dimensão 1000*1000
wnd::Display
wnd = InWindow  "Micro Machines - World Cup Edition"
                (1000,1000)
                (500,500)
-----------------------------------------------------------------------------
--                                                                         --
-- DEFINIÇÃO DO ESTADO INICIAL                                             --
--                                                                         --
-----------------------------------------------------------------------------
-- | A função estadoInicial define o estado inicial tendo em conta um jogo inicial, a lista com os constituintes gráficos e o número de jogadores base
estadoInicial::Jogo->[[Picture]]->Int->[Int]->Int->[Mapa]->Mapa->[Propriedades]->Propriedades->Estado
estadoInicial jog [flor,gelo,praia,carros,inter,bande,num,letras] nj lC tema lMapas mapaR propC propR= (jog,[flor,gelo,praia,carros,inter,bande,num,letras],
                                                                                                       replicate nj (False,False,False,False,Nothing),[0,0,nj,tema,0,0],
                                                                                                       replicate nj 0,0,replicate nj 2,lMapas,[False,False,False],[lC],
                                                                                                       mapaR,[0..(nj-1)],listaNomes,propR,propC)
            
-- | A função pecaToPic associa cada peça a uma imagem e ajusta o tamanho
pecaToPic::Estado->Float->Peca->Picture
pecaToPic e sx (Peca tipo a) = case getTema(mudaTema e) of
                               0 -> case tipo of
                                    Lava -> scale sx sx  (pics!!0)
                                    Recta -> scale sx sx (ajustaCorR a (pics!!1) 0)
                                    (Rampa o) -> scale sx sx (fazRampa o (pics!!1) a 0)
                                    (Curva o) -> scale sx sx (Pictures [(pics!!0),(pics!!(getCurva o)),(ajustaCorC a (pics!!(getCurva o)) o (getTema e))])
                               _ -> case tipo of
                                         Lava -> scale sx sx  (pics!!0)
                                         Recta -> scale sx sx (ajustaCorR a (pics!!1) (getTema e))
                                         (Rampa o)->scale sx sx (fazRampa o (pics!!1) a (getTema e))
                                         (Curva o)-> scale sx sx  (imgCurva o)
                            where imgCurva o = Pictures [(pics!!0),(rotate (toAng o) (pics!!2)),(ajustaCorC a (pics!!2) o (getTema e))]
                                  getCurva::Orientacao->Int
                                  getCurva o = case o of
                                               Norte -> 2
                                               Sul -> 3
                                               Este -> 4
                                               Oeste -> 5
                                  pics = getPics (getTema e) e

                                                            
-- | A função dimensaoTab devolve a dimensão (x,y) de uma dado tabuleiro
dimensaoTab::Tabuleiro->(Float,Float)
dimensaoTab (l:c) = (fromIntegral (length l),fromIntegral (length (l:c)))
-- | A função scaleFactor define qual o valor de scaling a aplicar as peças base de dimensão (100,100) para que estes se ajustem à janela
scaleFactor::Tabuleiro->Float
scaleFactor t = if (tabx>=taby)
                then  1000/(100*tabx)
                else 1000/(100*taby)
              where (tabx,taby) = dimensaoTab t
-- | A função tabToPic  transforma um certo tabuleiro numa lista de imagens com o respetivo translate
tabToPic::Tabuleiro->Tabuleiro->Estado->(Float,Float)->[Picture]
tabToPic [] tabu e (x,y) = []
tabToPic ([]:t) tabu e (x,y) = tabToPic t tabu e (0,y-1)
tabToPic ((h:r):t) tabu e (x,y) = (Translate (x*(100*sx)) (y*(100*sx)) (pecaToPic e sx h)):tabToPic (r:t) tabu e (x+1,y)
                                where sx = scaleFactor (tabu)
dTf= double2Float
-- | A função drawCars devolve uma lista de imagens dos carros tendo em conta o tabuleiro, o número de jogadores, a lista de carros e o estado atual do jogo
drawCars::Tabuleiro->Int->[Carro]->Estado->[Picture]
drawCars tab 0 l e = []
drawCars tab nj ((Carro (x,y) d _):tc) e@(j,pic,p,varI,nV,f,nM,lM,fJ,lisC,mr,pos,nms,pR,pC) = (Translate  transx transy (rotate (-dTf d) (scale sx sx (carroAtual)))) : drawCars tab (nj-1) tc (j,pic,p,varI,nV,f,nM,lM,fJ,(changeListElem (tail lC) 0 lisC),mr,pos,nms,pR,pC)
                                                                                         where sx =  (scaleFactor tab)
                                                                                               (tabx,taby) = dimensaoTab tab
                                                                                               (transx,transy) = if tabx<=taby
                                                                                                                 then ((-((100*tabx*sx)/2)  + (dTf x*100*sx)),(500 - (dTf y*100*sx)))
                                                                                                                 else ((-500 + (dTf x*100*sx)),(((100*taby*sx)/2) - (dTf y*100*sx)))
                                                                                               carros= pic!!3
                                                                                               lC= lisC!!0
                                                                                               carroAtual = scale (50/960) (25/480) (carros!!(lC!!0))
-- | 
segToMs::Float->Float
segToMs x = if x > 60 
            then segToMs (x-60)
            else x*1000
-- | Função que passa de segundos para minutos
segToMin::Float->Float
segToMin x = x/60
-- | A função tempoString transforma um tempo recebido em millisegundos numa string da forma "MM:SS:MSMS"
tempoString::Float->String
tempoString x = mostra min ++":"++mostra seg++":"++takeWhile (/= '.') ms
             where min = div (floor x) 60
                   seg = mod (floor x) 60
                   ms = take 2 (show ((segToMs x) - toEnum seg*1000))
                   mostra::Int->String
                   mostra x | x>=10 = show x
                            | otherwise = '0':show x
                
drawTempo::String->[Picture]->Tabuleiro->[Picture]
drawTempo [] _ _ = []
drawTempo (h:t) lN tab = if h==':'
                         then (Translate transx transy (scale sx sx(lN!!10))):drawTempo t lN tab
                         else (Translate transx transy (scale sx sx (lN!!(digitToInt h)))):drawTempo t lN tab
                       where sx = scaleFactor tab
                             (tabx,taby) = dimensaoTab tab
                             (transx,transy) = if tabx<=taby
                                               then ((-((100*tabx*sx)/2)  + (x*100*sx)),(500 - ( y*100*sx)))
                                               else ((-500 + (x*100*sx)),(((100*taby*sx)/2) - ( y*100*sx)))
                             (x,y) = (tabx - (toEnum (length (h:t)))*0.25,0) 

drawNitros::[Double]->[Int]->Tabuleiro->[Picture]
drawNitros ln [] tab = []
drawNitros lN (h:t) tab = (Translate transx transy (scale sx sx (drawN(lN!!h)))):drawNitros lN t tab
                       where sx = scaleFactor tab
                             (tabx,taby) = dimensaoTab tab
                             (transx,transy) = if tabx<=taby
                                               then ((-((100*tabx*sx)/2)  + (x*100*sx)),(500 - ( y*100*sx)))
                                               else ((-500 + (x*100*sx)),(((100*taby*sx)/2) - ( y*100*sx)))
                             (x,y) = (tabx - (toEnum (length (h:t))),taby - 1 + 0.9)
                             drawN::Double->Picture
                             drawN x = color blue (scale (dTf x) 1 (rectangleSolid 30 10))  
-- | A função drawPos desenha a lista de posições dos carros com as suas respetivas bandeiras
drawPos::Int->[Int]->Tabuleiro->[Picture]->[Int]->[Picture]
drawPos _ [] _ _ _ = []
drawPos nj (h:t) tabu lBand lO = Translate (100*x*sx) (100*y*sx) (scale sx sx imgAdes):drawPos nj t tabu lBand lO
                                    where sx = scaleFactor (tabu)
                                          (xF,yF) = (dimensaoTab tabu)
                                          (x,y) = (xF-toEnum(length (h:t)),-yF+1)
                                          imgAdes = scale 0.2 0.2 (lBand!!(lO!!h))
desenhaNitros::[Double]->[Maybe Int]->Estado->[Picture]
desenhaNitros _ [] _ = []
desenhaNitros (h:t) (a:b) e = case a of
                              Nothing -> desenhaNitros t b e
                              Just j -> if h > 0
                                        then (Translate (fst (muda (posC j cars))) (snd (muda (posC j cars))) (scale sx sx ((rotate (dir j cars)) imgNitro))):desenhaNitros t b e
                                        else desenhaNitros t b e
                            where sx =  (scaleFactor tab)
                                  (tabx,taby) = dimensaoTab tab
                                  muda::Ponto->(Float,Float)
                                  muda (x,y) = if tabx<=taby
                                               then ((-((100*tabx*sx)/2)  + (dTf x*100*sx)),(500 - (dTf y*100*sx)))
                                               else ((-500 + (dTf x*100*sx)),(((100*taby*sx)/2) - (dTf y*100*sx)))
                                  posC::Int->[Carro]->Ponto
                                  posC j cars = posicao (cars!!j)
                                  dir::Int->[Carro]->Float
                                  dir j c = -dTf(direcao (c!!j))
                                  tab = getTabul (mapa (getJogo e))
                                  cars = getCars e
                                  imgNitro = (getPics 4 e)!!4
desenhaCorrida::Estado->Picture
desenhaCorrida e = Pictures [Translate  tsx tsy (Pictures (tabToPic tab tab e (0,0))),Pictures(desenhaNitros (nitros(getJogo e)) (getListaN e) e),
                             Pictures (drawCars tab nj cars e),(Translate  tsx tsy (Pictures (drawPos nj lPos tab bandeiras (getOrdemA e)))),
                             Translate  (12.5*sx) (-50*sx) (Pictures (drawTempo (tempoString (getTempo e)) num tab)),
                             Translate (50*sx) 0 (Pictures (drawNitros (nitros (getJogo e)) lPos tab))]
                 where sx = scaleFactor tab
                       (tabx,taby) = dimensaoTab tab 
                       (tsx,tsy) = if (tabx>=taby)
                                   then ((-500+((100*sx)/2)),((taby-1)*(100*sx)/2))
                                   else ((-(tabx-1)*(100*sx)/2),(500-((100*sx)/2)))
                       tab = getTabul (mapa (getJogo e))
                       nj = getNJ e
                       bandeiras = getPics 5 e
                       num = getPics 6 e
                       cars = getCars e
                       m@(Mapa (pI,oi) tabE) = mapa (getJogo e)
                       listaO = (getOrdem (getNJ e) (historico (getJogo e)) (guardaPosVal m ((getPeca tabE pI),pI) oi))
                       lPos =  (ordemDoJogo (fLtJ (snd (unzip (ordJogo listaO))) (getNJ e)) (getNV e))
                       nV = getNV e
                                           
desenhaTabela::Estado->Picture
desenhaTabela e = Pictures [menus!!2,(Pictures (quantasBandeiras (getElims e) bandeiras))]
                where bandeiras = getPics 5 e
                      menus = getPics 4 e
desenhaNome::Estado->Picture
desenhaNome e = Pictures (stringToPic letras (getString e) (-200,300))
              where letras = getPics 7 e 
desenhaCarro::Estado->Picture
desenhaCarro e = scale 0.5 0.5 (cars!!(getOpcaoCarro e))
               where cars = getPics 3 e
desenhaOpcao::Estado->[Picture]
desenhaOpcao e = [desenhaNome e,desenhaCarro e]
desenhaEscolheCarro::Estado->Picture
desenhaEscolheCarro e = Pictures [menus!!3,Pictures(desenhaOpcao e)]
                      where menus = getPics 4 e 

draw::Estado->Picture
draw e = case (getMenu e) of
         1 ->  desenhaTabela e     
         2 ->  if (getFimC e)
               then Pictures (desenhaFim lPos ((getElims e)!!0) bandeiras letras pics)
               else (desenhaCorrida e)
         0 -> Pictures [(pics!!0),Translate 18 ((-33) - (163*toEnum (getOption e))) (pics!!1)]
         3 -> desenhaEscolheCarro e
         5 -> Pictures [pics!!5,Pictures (stringToPic letras "parabens" (-150,0)),Pictures (stringToPic letras "venceu" (-100,-100))]
        where pics = getPics 4 e
              bandeiras = getPics 5 e
              letras = getPics 7 e
              m@(Mapa (pI,oi) tabE) = mapa (getJogo e)
              listaO = (getOrdem (getNJ e) (historico (getJogo e)) (guardaPosVal m ((getPeca tabE pI),pI) oi))
              lPos =  (ordemDoJogo (fLtJ (snd (unzip (ordJogo listaO))) (getNJ e)) (getNV e))

-- | A função iU é a função responsável obter Input do utilizazdor, indo a reação ao mesmo input variando conforme o estado atual do jogo
iU::Event->Estado->Estado
-- Input Corridas (Teclas Pressionadas)
iU (EventKey key ks _ _) e = case key of
                             (SpecialKey k) -> case k of
                                        KeyUp -> case getMenu e of
                                              2 -> if ks == Down
                                                   then mudaAct (getAct e) True 0 e
                                                   else mudaAct (getAct e) False 0 e
                                              0 -> if ks==Down then if getOption e>0
                                                   then  mudaOption (-1) e
                                                   else  mudaOption 0 e
                                                   else e
                                              _ -> e
                                             
                                        KeyRight -> case getMenu e of
                                                 2 -> if ks == Down
                                                   then mudaAct (getAct e) True 3 e
                                                   else mudaAct (getAct e) False 3 e
                                                 3 -> if ks==Down
                                                      then if getOpcaoCarro e < 31
                                                           then mudaEscolha e 1
                                                           else mudaEscolha e (-31)
                                                      else e
                                                 _ -> e
                                                 
                                        KeyDown -> case getMenu e of
                                                2 -> if ks == Down
                                                   then mudaAct (getAct e) True 1 e
                                                   else mudaAct (getAct e) False 1 e
                                                0 -> if ks==Down then if getOption e<2
                                                   then  mudaOption 1 e
                                                   else  mudaOption 0 e
                                                   else e
                                                _ -> e
                                                
                                        KeyLeft -> case getMenu e of
                                                2 -> if ks == Down
                                                   then mudaAct (getAct e) True 2 e
                                                   else mudaAct (getAct e) False 2 e
                                                3 -> if ks==Down
                                                      then if getOpcaoCarro e > 0 
                                                           then mudaEscolha e (-1)
                                                           else mudaEscolha e (31)
                                                      else e
                                                _ -> e
                                                
                                        KeyEnter -> case getMenu e of
                                                 2 -> if ks==Down
                                                      then if (getFimC e)
                                                           then if naCampanha e
                                                                then if getPassou e
                                                                     then if acabouCampanha e
                                                                          then vaiGanhou (tTempo (mudaFimC (refazJogo e) False))
                                                                          else voltaTabela (tTempo (mudaFimC (refazJogo e) False)) (proxEliminatoria (getWnrs e) ((getElims e)!!0))
                                                                     else voltaMenu (tTempo (mudaFimC (refazJogo (refazFases e)) False))
                                                                else voltaMenu (tTempo (mudaFimC (refazJogo e) False))
                                                           else e
                                                      else e
                                                 0 -> if ks== Down
                                                      then case (getOption e) of
                                                           1 -> entraMenu (mudaMapa e (getMapaRapido e) (getPropRapido e))
                                                           0 -> (entraSelecao e) 
                                                      else e
                                                 1 -> if ks==Down
                                                      then entraCampanha (poeCampanha e)
                                                      else e
                                                 5 -> if ks==Down
                                                      then voltaMenu e
                                                      else e
                                                 3 -> if ks==Down
                                                      then entraMenu (poeCarro e)
                                                      else e 
                             (Char c) -> case getMenu e of
                                          2 -> case ks of
                                               Down -> if isDigit c
                                                       then mudaNitro 0 (digitToInt c) e
                                                       else case c of
                                                            'b' -> voltaMenu (tTempo (mudaFimC (refazJogo e) False))
                                                            _ -> e
                                               Up -> if isDigit c
                                                     then naoNitro 0 e
                                                     else case c of
                                                          _ -> e
                                          _ -> e
                                          
                             _ -> e
                             where fase = length (getElims e)
iU _ e = e
trataMorto::Int->[Bool]->Estado->Estado
trataMorto 0 lVivos e = if lVivos!!0
                        then e
                        else poeMortoJ 0 e
trataMorto n lVivos e = if lVivos!!n
                        then (trataMorto (n-1) lVivos e)
                        else (trataMorto (n-1) lVivos (poeMortoJ n e))

trataAcaoMorto::Int->Estado->Acao->Acao
trataAcaoMorto j e ac = if (getTempoVivo e)!!j <= 1.5
                        then (Acao False False False False Nothing)
                        else ac
pontoMedio::Peca->Ponto->Ponto
pontoMedio (Peca tipo _) (x,y) = case tipo of
                                 (Curva o) -> case o of
                                              Este -> (xX + 0.3,yY + 0.5)
                                              Sul -> (xX+0.3,yY+0.5)
                                              _ -> (xX+0.7,yY+0.5)
                                 _ -> (xX+0.5,yY+0.5)
                               where (xX,yY) = (toEnum (floor x),toEnum (floor y))
cortouCaminho::[Posicao]->[Posicao]->Bool
cortouCaminho [] _ = False
cortouCaminho [x] _ = False
cortouCaminho h lPos = not (entre (0,iPecA+3) iPecaAtual)
                     where (iPecA,_) = (getOrdem 1 [tail h] lPos)!!0
                           (iPecaAtual,_) = (getOrdem 1 [h] lPos)!!0 
{- | A função movimentaCarros atualiza a posição de uma certa lista de carros tendo em conta o tabuleiro,
o tempo e a lista de carros, sendo que, se o carro "morrer" vai para o meio da peça-}
movimentaCarros::Tabuleiro->Tempo->[Carro]->[[Posicao]]->[Posicao]->Int->[(Carro,Bool,Bool)]
movimentaCarros _ _ [] _ _ _= []
movimentaCarros tab t (hc@(Carro p d v):tc) (hh:th) lPos j = if (movimenta tab t hc)==Nothing   
                                                             then ((Carro pm d (0,0)),False,True):movimentaCarros tab t tc th lPos (j+1)
                                                             else if cortouCaminho hh lPos
                                                                  then ((Carro mp d (0,0)),True,False):movimentaCarros tab t tc th lPos (j+1)
                                                                  else (removeJust(movimenta tab t hc),True,True):movimentaCarros tab t tc th lPos (j+1)
                                                           where pm = pontoMedio (whichPeca p tab) p
                                                                 mp = pontoMedio (whichPeca (posPonto(hh!!1)) tab) (posPonto(hh!!1))
                                                                 posPonto::Posicao->Ponto
                                                                 posPonto (x,y) = (toEnum x,toEnum y)

-- | A função atualizaBots atualiza todos os bots existentes no jogo, devolvendo assim o jogo atualizado
atualizaBots::Int->Jogo->Tempo->Estado->(Jogo,[Acao])
atualizaBots 0 jog _ _ = (jog,[])
atualizaBots j jog@(Jogo m pro cars n h) t e= case j of
                                             1 -> (atualiza t a 1 (trataAcaoMorto j e (botMedio t jog j)),(trataAcaoMorto j e (bot t jog j)):b)
                                             2 -> (atualiza t a 2 (trataAcaoMorto j e (bot t jog j)),(trataAcaoMorto j e (botMedio t jog j)):b)
                                             _ -> (atualiza t a j (trataAcaoMorto j e (botFacil t jog j)),(trataAcaoMorto j e (botFacil t jog j)):b)
                                           where (a,b) = atualizaBots (j-1) jog t e
eliminaTodos::Posicao->[Posicao]->[Posicao]
eliminaTodos p [] = []
eliminaTodos p (h:t) | p==h = eliminaTodos p t
                     | otherwise = h : eliminaTodos p t 
mudaHisto::[[Posicao]]->[Bool]->Int->[[Posicao]]
mudaHisto histo [] _= histo
mudaHisto histo (h:t) j = if h 
                          then mudaHisto histo t (j+1)
                          else mudaHisto (changeListElem (tail (eliminaTodos (histo!!j!!1) (histo!!j))) j histo) t (j+1)
deuVolta::[[Posicao]]->[[Posicao]]->[Posicao]->[Int]->([[Posicao]],[Int])
deuVolta _ [] _ nv = ([],nv) 
deuVolta [] _ _ nv = ([],nv)
deuVolta ((hh:th):t) (hj:tj) lPos (hi:ti) = if (length (elemIndices hh hj))>=(length (elemIndices hh lPos)) && completoV th lPos hj
                                            then ([] : a, (hi+1):b)
                                            else ((hh:th):a,(hi:b))
                                          where completoV::[Posicao]->[Posicao]->[Posicao]->Bool
                                                completoV [] _ _ = True
                                                completoV (hh:th) lPos his = (length (elemIndices hh his))>=(length (elemIndices hh lPos)) && completoV th lPos his
                                                (a,b) = deuVolta t tj lPos ti
fimDoJogo::[Int]->Estado->(Estado,Bool) 
fimDoJogo nv e = (mudaFimC e (length (filter (==3) nv) >=1),(length (filter (==3) nv) >=1)) 

proxEliminatoria::(Int,Int)->[Int]->[Int]
proxEliminatoria (f,s) elim = f : s : (arranjaProx (drop 4 elim))
                            where arranjaProx::[Int]->[Int]
                                  arranjaProx [a,_,b,_] = [a,b]
                                  arranjaProx (a:b:c:d:t) = a:c:arranjaProx t

-- | A função tU é a responsável pela atualização automática do jogo, tendo em conta o estado atual do jogo e o tempo que se passou desde a ultima atualização
tU::Float->Estado->Estado
tU tf e = case getMenu e of
          1 -> if fimDJogo
               then passou (fst (fimDoJogo (getNV e) e)) sPassou 
               else proxEstado
          2 -> if fimDJogo
               then substituiPos (passou (fst (fimDoJogo (getNV e) e)) sPassou) lPosicao
               else (mudaTema proxEstado)
          _ -> e
        where (newCars,_,_) = (unzip3 (movimentaCarros tab (float2Double tf) (carros nJogo) newHist lPos 0))
              (_,lVivos,lHis) = (unzip3 (movimentaCarros tab (float2Double tf) (carros jog) (historico jog) lPos 0))
              (newHist,novaVolta) = (deuVolta (mudaHisto (historico nJogo) lHis 0) (mudaHisto (historico nJogo) lHis 0) lPos (getNV e))
              jogo0 = atualiza (float2Double tf) jog 0 (trataAcaoMorto 0 estadoMortos (getAcao e 0))
              (nJogo,lAcoes) = (atualizaBots ((getNJ e)-1) jogo0 (float2Double tf) estadoMortos)
              newNitros = (nitros nJogo)
              tab = getTabul (mapa (getJogo e))
              jog = getJogo e
              estadoMortos = (trataMorto ((getNJ e)-1) lVivos e)
              estadoAtual = (mudaAcaoB lAcoes (acreTempoV (float2Double tf) (aTempo tf estadoMortos)) 1)
              m@(Mapa (pI,oI) tabE) = mapa jog
              lPos = guardaPosVal (Mapa (pI,oI) tabE) ((getPeca tab pI),pI) oI
              (proxEstado,fimDJogo) = fimDoJogo (getNV e) (mudaJogo newCars newNitros newHist novaVolta estadoAtual)
              listaO = (getOrdem (getNJ e) (historico (getJogo e)) (guardaPosVal m ((getPeca tabE pI),pI) oI))
              lPosicao =  (ordemDoJogo (fLtJ (snd (unzip (ordJogo listaO))) (getNJ e)) (getNV e))
              sPassou = (lPosicao!!0==0)||(lPosicao!!1==0)


-- | A função randomMapa devolve um mapa aleatório de uma lista de mapas predefinidos
randomMapa::IO (Mapa)
randomMapa = do
             i <- randomRIO (0,30)
             return ((mapas128++mapasFG++mapas64++mapasQF++mapas16++mapasSF++mapas4)!!i)

baralhaCarros::[Int]->IO([Int])
baralhaCarros [] = return []
baralhaCarros l = do 
                  i <- randomRIO (0,length l - 1)
                  let (x,y:z) = splitAt i l
                  m <- baralhaCarros (x++z)
                  return (y:m)
listaMapas::IO ([Mapa])
listaMapas = do
             a <- randomRIO (0,8)
             b <- randomRIO (0,8)
             c <- randomRIO (0,8)
             d <- randomRIO (0,3)
             return [((mapas128++mapasFG)!!a),((mapas64++mapasQF)!!b),((mapas16++mapasSF)!!c),(mapas4!!d)]

propriedadeR::IO (Propriedades)
propriedadeR = do
               x <- randomRIO (0,2)
               return (propriedades!!x)

listaPropriedades::IO ([Propriedades])
listaPropriedades = do
                    a <- randomRIO (0,2)
                    b <- randomRIO (0,2)
                    c <- randomRIO (0,2)
                    d <- randomRIO (0,2)
                    if a/= 1 && a/=b && c/=d && b/=c then return ([propriedades!!a,propriedades!!b,propriedades!!c,propriedades!!d]) else listaPropriedades

{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}
main::IO()
main = do 
       mapaA <- randomMapa 
       carros <- carregaCarros
       floresta <- carregaFloresta
       gelO <- carregaGelo
       praia <- carregaPraia
       inter <- carregaMenu
       bandeiras <- carregaBandeiras
       lCarros <- baralhaCarros [0..31]
       num <- carregaNum
       letras <- carregaLetras
       lMapas <- listaMapas
       propA <- propriedadeR
       propC <- listaPropriedades
       play wnd (greyN 0.4) 80 (estadoInicial (createJogo 4 (mapaA) propA) [floresta,gelO,praia,carros,inter,bandeiras,num,letras] 4 lCarros 0 lMapas (mapaA) propC propA) draw iU tU