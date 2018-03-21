module StateOperator where
import Tarefa1_2017li1g71
import Tarefa2_2017li1g71
import Tarefa3_2017li1g71
import Tarefa4_2017li1g71
import Tarefa6_2017li1g71
import LI11718
import Graphics.Gloss
import GHC.Float
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Data.Char
import Mapas
import Data.List
-- | O tipo Estad serve apenas para representar o estado normal do jogo
type Estad = (Jogo,[[Picture]],[(Bool,Bool,Bool,Bool,Maybe Int)],[Int],[Int],Float,[Double],[Mapa],[Bool],[[Int]],Mapa,[Int],[String],Propriedades,[Propriedades])
-- | A função getAct devolve o conjunto de booleanos de um estado
getAct::Estad->(Bool,Bool,Bool,Bool,Maybe Int)
getAct (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = ac!!0
-- | A função mudaAct muda o valor de um booleano do estado, dado o valor que se pretende atribuir e o indice em que esse booleano se encontra
mudaAct::(Bool,Bool,Bool,Bool,Maybe Int)->Bool->Int->Estad-> Estad
mudaAct (a,t,e,d,n) b i (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = case i of
                                              0 -> (jog,img,(changeListElem (b,t,e,d,n) 0 ac),vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                              1 -> (jog,img,(changeListElem (a,b,e,d,n) 0 ac),vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                              2 -> (jog,img,(changeListElem (a,t,b,d,n) 0 ac),vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                              3 -> (jog,img,(changeListElem (a,t,e,b,n) 0 ac),vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getMenu dá qual a atual janela do estado
getMenu::Estad->Int
getMenu (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!0
-- | A função getOption dá qual a atual opção escolhida no menu principal do jogo
getOption::Estad->Int
getOption (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!1
-- | A função mudaOption muda a opção do menu principal, dado um valor a somar à atual opção
mudaOption::Int->Estad->Estad
mudaOption n (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem ((vI!!1)+n) 1 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função entraMenu muda a janela de jogo
entraMenu::Estad->Estad
entraMenu (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem ((vI!!1)+1) 0 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função mudaNitro muda o nitro dado pelo jogador principal no estado
mudaNitro::Int->Int->Estad->Estad
mudaNitro j jA (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,novaAc j jA ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                  where novaAc::Int->Int->[(Bool,Bool,Bool,Bool,Maybe Int)]->[(Bool,Bool,Bool,Bool,Maybe Int)]
                                        novaAc 0 jA ((a,b,e,d,n):t) = (a,b,e,d,(Just jA)):t
                                        novaAc j jA (h:t) = novaAc (j-1) jA t
-- | A função naoNitro desativa o nitro dado pelo jogador principal no estado
naoNitro::Int->Estad->Estad
naoNitro j (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,novaAc j ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                  where novaAc::Int->[(Bool,Bool,Bool,Bool,Maybe Int)]->[(Bool,Bool,Bool,Bool,Maybe Int)]
                                        novaAc 0 ((a,b,e,d,n):t) = (a,b,e,d,Nothing):t
                                        novaAc j (h:t) = novaAc (j-1) t
-- | A função voltaMenu volta ao menu principal
voltaMenu::Estad->Estad
voltaMenu (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem 0 0 (changeListElem 0 1 vI)),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getJogo devolve qual o jogo atual presente no estado
getJogo::Estad->Jogo
getJogo (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = jog
-- | A função mudaJogo recebe uma lista de carros, de tempos e um histórico e insere-os no jogo do estado
mudaJogo::[Carro]->[Tempo]->[[Posicao]]->[Int]->Estad->Estad
mudaJogo nC nT nP numV ((Jogo m pro cars n h),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)= ((Jogo m pro nC nT nP),img,ac,vI,numV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getNJ devolve o número de jogadores do estado
getNJ::Estad->Int
getNJ (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!2
-- | A função getAcao devolve a ação a tomar pelo jogador principal baseando-se no estado
getAcao::Estad->Int->Acao
getAcao (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = (Acao (getA j 0 ac) (getA j 1 ac) (getA j 2 ac) (getA j 3 ac) (getN j ac))
                             where getA::Int->Int->[(Bool,Bool,Bool,Bool,Maybe Int)]->Bool
                                   getA 0 op ((a,t,e,d,n):ta) = case op of
                                                               0 -> a
                                                               1 -> t
                                                               2 -> e
                                                               3 -> d
                                   getA n op (h:t) = getA (n-1) op t
                                   getN::Int->[(Bool,Bool,Bool,Bool,Maybe Int)]->Maybe Int
                                   getN 0 ((_,_,_,_,n):t) = n
                                   getN n (h:t) = getN (n-1) t
-- | A função getCars devolve o conjunto de carros presentes num jogo no estado
getCars::Estad->[Carro]
getCars ((Jogo _ _ cars _ _),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = cars
-- | A função aTempo adiciona tempo ao tempo registado no estado
aTempo::Float->Estad->Estad
aTempo t (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,time+t,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função tTempo põe o tempo registado no estado a 0
tTempo::Estad->Estad
tTempo (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,0,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getTempo devolve o tempo decorrido no estado
getTempo::Estad->Float
getTempo (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = time
-- | A função getPics vai buscar à lista de listas de Picture a lista pretendida dado um certo indice
getPics::Int->Estad->[Picture]
getPics n (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = img!!n
-- | A função getTema devolve qual o atual tema de corridas escolhido pelo jogador
getTema::Estad->Int
getTema (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!3
-- | A função mudaTema muda o tema escolhido no estado
mudaTema::Estad->Estad
mudaTema es@(jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)= if (pista jog)==asfalto
                                                                            then (jog,img,ac,(changeListElem 0 3 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                                                            else if (pista jog)==gelo
                                                                                 then (jog,img,ac,(changeListElem 1 3 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                                                                 else (jog,img,ac,(changeListElem 2 3 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                            
-- | A função getNV devolve qual o número de voltas à pista dado por cada jogador
getNV::Estad->[Int]
getNV (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = nV
-- | A função acreTempoV aumenta o tempo vivo de todos os jogadores dado o tempo a adicionar
acreTempoV::Double->Estad->Estad
acreTempoV t (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,time,(map (+t) dTime),lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função poeMortoJ poe o tempo desde a ultima morte a 0 para um certo jogador
poeMortoJ::Int->Estad->Estad
poeMortoJ j (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,time,(changeListElem 0 j dTime),lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getTempoVivo devolve a lista de tempos desde a ultima morte de cada jogador
getTempoVivo::Estad->[Double]
getTempoVivo (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = dTime
-- | A função getOrdemA devolve a lista aleatoria de indices dos carros a correr
getOrdemA::Estad->[Int]
getOrdemA (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = elimi!!0
-- | A função mudaAcaoB muda as ações que os bots tomam no estado de forma a puderem ser operadas noutras funções
mudaAcaoB::[Acao]->Estad->Int->Estad
mudaAcaoB [] e _ = e
mudaAcaoB ((Acao ace tra esq dir nit):t) (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = mudaAcaoB t (jog,img,changeListElem (ace,tra,esq,dir,nit) j ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) (j+1)
-- | A função getListaN devolve uma lista dos nitros dados por todos os jogadores, recebendo o estado
getListaN::Estad->[Maybe Int]
getListaN (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = getNitros ac
                             where getNitros::[(Bool,Bool,Bool,Bool,Maybe Int)]->[Maybe Int]
                                   getNitros [] = []
                                   getNitros ((_,_,_,_,n):t) = n : getNitros t
-- | A função mudaNV muda o número de voltas dadas por um dado jogador num estado
mudaNV::Estad->Int->Estad
mudaNV (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = (jog,img,ac,vI,(changeListElem ((nV!!j)+1) j nV),time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função limpaH limpa o histórico de um jogador num estado 
limpaH::Estad->Int->Estad
limpaH ((Jogo m@(Mapa (pI,oI) tab) p c n h),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = ((Jogo m p c n (changeListElem [pI] j h)),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getHisto devolve o histórico de um jogador através do estado
getHisto::Estad->Int->[Posicao]
getHisto ((Jogo m p c n h),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = h!!j
-- | A função getFimC obtem a informação sobre se o a corrida já acabou ou não através do estado
getFimC::Estad->Bool
getFimC (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = fimJ!!0
-- | A função mudaFimC modifica o booleano que controla se a corrida já terminou ou não no estado
mudaFimC::Estad->Bool->Estad
mudaFimC (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) b = (jog,img,ac,vI,nV,time,dTime,lMap,(changeListElem b 0 fimJ),elimi,mapasR,pos,nomes,propR,propC)
-- | A função refazJogo recebe um estado e devolve esse estado mas com o jogo "refeito", ou seja, com o histórico limpo e os nitros repostos
refazJogo::Estad->Estad
refazJogo e@((Jogo (Mapa (pi,oi) tab) pro nC nT nP),img,ac,vI,numV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = ((Jogo (Mapa (pi,oi) tab) pro novosC novosT novosP),img,ac,vI,(replicate (getNJ e) 0),time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
                                                                                            where novosC = replicate (getNJ e) (Carro (pI pi) 0 (0,0))
                                                                                                  novosT = replicate (getNJ e) 3
                                                                                                  novosP = replicate (getNJ e) []
                                                                                                  pI::Posicao->Ponto
                                                                                                  pI (x,y) = (toEnum x + 0.5 , toEnum y + 0.5)
{- | A função createJogo cria um jogo dado o numero de jogadores, o mapa em que vai ser jogado
e as propriedades do jogo-}
createJogo::Int->Mapa->Propriedades->Jogo
createJogo 1 m pro = (Jogo m pro [Carro (getPI m) (getAI m) (0,0)] [3] [[]])
createJogo n m pro = juntaJ (createJogo 1 m pro) (createJogo (n-1) m pro)
-- | Função que dá o angulo da peça inicial de uma mapa
getAI::Mapa->Angulo
getAI (Mapa (_,oi) _) = case oi of
                        Norte -> pi/2
                        Sul -> (3*pi)/2
                        Este -> 0
                        Oeste -> pi
-- | Função que dá o ponto inicial de um mapa
getPI::Mapa->Ponto
getPI (Mapa ((x,y),_) _) = (toEnum x + 0.5,toEnum y +0.5)

-- | A função juntaJ junta dois jogos, concatenando as listas de carros, de nitros e o histŕico de ambos
juntaJ::Jogo->Jogo->Jogo
juntaJ (Jogo m pro cars n h) (Jogo m1 pro1 cars1 n1 h1) = (Jogo m pro (cars++cars1) (n++n1) (h++h1))
-- | A função poeCampanha muda o booleano que controla se o jogador está a jogar o modo campanha ou não
poeCampanha::Estad->Estad
poeCampanha (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,time,dTime,lMap,(changeListElem True 1 fimJ),elimi,mapasR,pos,nomes,propR,propC)
-- | A função naCampanha devolve o valor do booleano que diz se o jogador está no modo campanha ou não
naCampanha::Estad->Bool
naCampanha (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = fimJ!!1
-- | A função acabou campanha diz se um jogador venceu a campanha ou não
acabouCampanha::Estad->Bool
acabouCampanha e@(jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (getFase e)== 3 && fimJ!!0 && getPassou e && pos!!0 == 0
-- | A função passou controla o booleano que diz se o jogador vai fazer a proxima corrida ou não
passou::Estad->Bool->Estad
passou (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) b = (jog,img,ac,vI,nV,time,dTime,lMap,(changeListElem b 2 fimJ),elimi,mapasR,pos,nomes,propR,propC)
-- | A função mudaMapa muda o mapa que está no jogo do estado recebendo esse mesmo mapa
mudaMapa::Estad->Mapa->Propriedades->Estad
mudaMapa e@(jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) novoM prop= ((createJogo (getNJ e) novoM prop),img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getMapa devolve o mapa defenido para o modo campanha que vai ser jogado num dada fase
getMapa::Estad->Int->Mapa
getMapa (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) i = (lMap!!i)
-- | A função getFase devolve qual a fase que vai ser jogada através do estado
getFase::Estad->Int
getFase (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!4
-- | A função getElims devolve uma lista de listas de Int com o identificador dos jogadores presentes em cada fase do modo campanha
getElims::Estad->[[Int]]
getElims (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = elimi
-- | A função getMapaRapido devolve o mapa a ser jogado no modo corrida rápida
getMapaRapido::Estad->Mapa
getMapaRapido (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = mapasR
-- | A função entraCampanha muda o estado de forma a por o jogador a jogar a corrida correspondente no modo campanha
entraCampanha::Estad->Estad
entraCampanha (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = ( mudaMapa (jog,img,ac,(changeListElem 2 0 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) (lMap!!(vI!!4)) (propC!!(vI!!4)))
-- | A função voltaTabela faz o jogador voltar a janela de visualização das eliminatórias e atualiza a lista de jogadores que passaram à proxima fase
voltaTabela::Estad->[Int]->Estad
voltaTabela (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) lPos= (jog,img,ac,changeListElem ((vI!!4)+1) 4 (changeListElem 1 0 vI),nV,time,dTime,lMap,fimJ,(lPos:elimi),mapasR,pos,nomes,propR,propC)
-- | A função getPassou devolve o valor do booleano que controla se o jogador passou ou não na corrida do modo campanha
getPassou::Estad->Bool
getPassou (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = fimJ!!2
-- | A função refazFases limpa as fases percorridas até ao momento pelo jogador no modo campanha e a lista dos jogadores em cada fase, de modo a que seja possivel jogar novamente
refazFases::Estad->Estad
refazFases (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem 0 4 vI),nV,time,dTime,lMap,fimJ,(drop ((length elimi)-1)elimi),mapasR,pos,nomes,propR,propC)
-- | A função substituiPos muda a lista de posições em que os jogadores começaram a corrida por uma lista de posições dada
substituiPos::Estad->[Int]->Estad
substituiPos (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) nPos = (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,nPos,nomes,propR,propC)
-- | A função getWnrs devolve um par que são os jogadores que se apuraram para a fase seguinte do modo campanha
getWnrs::Estad->(Int,Int)
getWnrs e@(jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)  = if pos!!0 == 0 
                                                                     then ((getElims e)!!0!!(pos!!0),(getElims e)!!0!!(pos!!1))
                                                                     else ((getElims e)!!0!!(pos!!1),(getElims e)!!0!!(pos!!0))
-- | A função vaiGanhou muda o estado de modo a por o jogador a ver a tela final do modo campanha
vaiGanhou::Estad->Estad
vaiGanhou (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem 5 0 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | A função getString devolve o nome do país escolhido pelo jogador para jogar
getString::Estad->String
getString e@(jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = nomes!!(getOpcaoCarro e)
-- | A função getOpcaoCarro devolve qual a opção do jogador sobre com qual o carro jogar
getOpcaoCarro::Estad->Int
getOpcaoCarro (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = vI!!5
-- | A função mudaEscolha muda a escolha feita pelo jogador sobre qual o carro que quer jogar tendo em conta o quanto mudar essa opção
mudaEscolha::Estad->Int->Estad
mudaEscolha (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) j = (jog,img,ac,(changeListElem ((vI!!5)+j) 5 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) 
-- | A função entraSelecao muda o estado de jogo de modo a por o jogador na tela de escolher o seu carro no modo campanha
entraSelecao::Estad->Estad
entraSelecao (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,(changeListElem 3 0 vI),nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC)
-- | Lista que serve apenas para ordenar o nome dos países segundo a sua posição nas escolhas possiveis
listaNomes::[String]
listaNomes = ["alemanha","brasil","egipto","espanha","franca","islandia","japao",
              "portugal","russia","asaudita","uruguai","marrocos","irao","australia",
              "peru","dinamarca","argentina","croacia","nigeria","suica","costarica","servia",
              "mexico","suecia","coreia","belgica","panama","tunisia","inglaterra","polonia","senegal","colombia"]
-- | A função poeCarro muda o carro a jogar pelo jogador
poeCarro::Estad->Estad
poeCarro (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,(poeAcabeca (vI!!5) elimi),mapasR,pos,nomes,propR,propC)
-- | A função poeAcabeca muda a cabeça do primeiro elemento de uma lista de listas de acordo com o elemento que se pretende lá inserir
poeAcabeca::Int->[[Int]]->[[Int]]
poeAcabeca x [] = [[x]]
poeAcabeca x (l:r) = (x:(a++b)):r
                   where (a,y:b) = splitAt (removeJust (elemIndex x l)) l
getPropRapido::Estad->Propriedades
getPropRapido (jog,img,ac,vI,nV,time,dTime,lMap,fimJ,elimi,mapasR,pos,nomes,propR,propC) = propR