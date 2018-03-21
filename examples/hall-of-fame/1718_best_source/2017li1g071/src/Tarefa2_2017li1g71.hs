module Tarefa2_2017li1g71 where
import Tarefa1_2017li1g71
import LI11718
testesT2 :: [Tabuleiro]
testesT2 = fazTesteFromT1toT2++[tab0,tab1,tab2,tab3,tab4,tab5,tab6,tab7]
-----------------------------------------------------
-- | Para verificar que o Mapa é Rectangular 
check_rectangulo:: Tabuleiro->Bool
{- Só substitui o x e o y por pl(Primeira Linha) e sl(Segunda Linha) e pus
rl (Resto das Linhas) para ficar mais legivel e simplifiquei o caso da lista 
de dois elementos -}
check_rectangulo [x] = True
check_rectangulo [pl,sl] = (length pl == length sl) 
check_rectangulo (pl:sl:rl) = (length pl == length sl) && (check_rectangulo (sl:rl))

--Auxiliares
fullLava::[Peca]->Bool
fullLava [] = True
fullLava (h:t) = (h==(Peca Lava 0))&&fullLava t 
firstAndLastLava::[Peca]->Bool
firstAndLastLava (h:t) = (h==(Peca Lava 0))&&((last t)==(Peca Lava 0))
--Principal
-- | Para verificar as bordas de Lava   
bordersLava::Tabuleiro->[Int]->Bool
bordersLava [x] _ = fullLava x --Para garantir que testa a ultima linha ser toda lava
bordersLava (x:xs) (y:ys) | (y==0) = fullLava x && bordersLava xs ys --Para a primeira linha
                          | otherwise = firstAndLastLava x && bordersLava xs ys --Para o resto das linhas
-------------------------------------------------------------------
-- | Para ver se a orientação inicial e a peça inicial se encaixam --     
oriInitpossivel::Orientacao->Peca->Bool
oriInitpossivel o (Peca tipo_peca alt) = case tipo_peca of 
                                        (Curva oc) -> (oc==o)||(oc==(oriDir o))
                                        (Rampa or) -> (or==o)||(or==(oriCon o))
                                        (Lava) -> False
                                        (Recta) -> True
-------------------------------------------------------------------
-- | Para ver se as alturas de duas peças são compativeis
eProxPAlt::Peca->Orientacao->Peca->Bool
eProxPAlt (Peca (tipo_peca0) alt0) o (Peca (tipo_peca1) alt1) = case (tipo_peca0) of
                                                                         (Rampa or) -> if (or==o)
                                                                                       then case tipo_peca1 of
                                                                                            (Rampa oe) -> if (oe==o)
                                                                                                          then (alt1==(1+alt0))
                                                                                                          else (alt0==alt1)
                                                                                            _ -> (alt1==(alt0+1))
                                                                                       else case tipo_peca1 of
                                                                                            (Rampa oe) -> if (oe==o)
                                                                                                          then (alt0==alt1)
                                                                                                          else (alt1==(alt0-1))
                                                                                            _ -> (alt0==alt1)
                                                                         _ -> case tipo_peca1 of
                                                                              (Rampa or) -> if (or==(oriCon o))
                                                                                            then (alt0-1==alt1)
                                                                                            else (alt0==alt1)
                                                                              _ -> (alt0==alt1)
-- | Para ver se as entradas de duas peças são compativeis 
eProxPOri::Orientacao->Tipo->Bool
eProxPOri o tipo_peca1 = ((tipo_peca1==(Rampa o))||
                         (tipo_peca1==(Rampa (oriCon o)))||
                         (tipo_peca1==(Curva o))||
                         (tipo_peca1==(Curva (oriDir o)))||
                         (tipo_peca1==Recta))

proxOri::Orientacao->Peca->Orientacao
proxOri o (Peca tipo a) = case tipo of 
                          Recta -> o
                          (Rampa _) -> o
                          (Curva or) -> if (or==o) then (oriDir o) else (oriEsq o)
getTipo::Peca->Tipo
getTipo (Peca tp a) = tp
getPeca::Tabuleiro->Posicao->Peca
getPeca t (x,y) = t!!y!!x
-- | Dá a lista das posiçoes das peças válidas do tabuleiro até voltar à peça inicial 
guardaPosVal::Mapa->(Peca,Posicao)->Orientacao->[Posicao]
guardaPosVal m@(Mapa (posi,oi) t) (p,pos) pO| novoPar==(getPeca t posi,posi)&&(eProxPOri oS tpP)&&(eProxPAlt p oS pP)&&oS==oi = [pos,posi]
                                            | (eProxPOri oS tpP)&&(eProxPAlt p oS pP) = pos:guardaPosVal m novoPar oS
                                            | otherwise = []
                                            where oS = proxOri pO p
                                                  pP = getPeca t novaPos
                                                  tpP = getTipo pP
                                                  novoPar = (pP,novaPos)
                                                  novaPos = move pos oS
-------------------------------------------------------------------------------------
-- | Dá a lista de todas as posições possiveis no tabuleiro                                                                                                    
allPos::(Int,Int)->[(Int,Int)]
allPos (a,b) = [(x,y)| x <- [0..(a-1)] , y <- [0..(b-1)]]
--------------------------------------------------------------------------------------
-- | Verifica se todas as peças fora da trajetória válida são Lava                  
verificaTudoLava::Tabuleiro->[Posicao]->[Posicao]->Bool
verificaTudoLava t [] vpos = True
verificaTudoLava t (hapos:tapos) vpos | notElem hapos vpos = (getPeca t hapos) == (Peca Lava 0)&&verificaTudoLava t tapos vpos
                                      | otherwise = True && verificaTudoLava t tapos vpos
--------------------------------------------------------------------------------------
-- | Dado um tabuleiro dá a sua dimensão                                           
dimensaoTabuleiro::Tabuleiro->(Int,Int)
dimensaoTabuleiro (l:c) = (tx,ty)
                                 where tx = length l
                                       ty = length (l:c)
--------------------------------------------------------------------------------------
-- | Função final                                                                  
valida :: Mapa -> Bool
valida m@(Mapa (posi,oi) t) = oriInitpossivel oi (getPeca t posi)&&
                              check_rectangulo t&&
                              bordersLava t [0..(ty-1)]&&
                              verificaTudoLava t (allPos (tx,ty)) posValidas
                              && head posValidas == last posValidas
                              where (tx,ty) = dimensaoTabuleiro t
                                    posValidas = (guardaPosVal m ((getPeca t posi),posi) oi)
--------------------------------------------------------------------------------------
-- Testes à mão                                                                     --
-------------------------------------------------------------------------------------- 
tab0::Tabuleiro
tab0 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab1::Tabuleiro
tab1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab2::Tabuleiro
tab2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Oeste) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab3::Tabuleiro
tab3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab4::Tabuleiro
tab4 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca Lava 1,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab5::Tabuleiro
tab5 =  [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
            [Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
            [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab6::Tabuleiro
tab6 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],
        [Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0],
        [Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],
        [Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],
        [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
tab7::Tabuleiro
tab7 = [[Peca Recta 0]]

--------------------------------------------------------------------------------------
-- Função para usar os Testes da T1 na T2                                           --
-------------------------------------------------------------------------------------- 
constroiTestes::[Caminho]->[Mapa]
constroiTestes (h:t) = map constroi (h:t)
getTabul::Mapa->Tabuleiro
getTabul (Mapa (_,_) t) = t
getTabulTeste::[Mapa]->[Tabuleiro]
getTabulTeste m = map getTabul m
fazTesteFromT1toT2 = getTabulTeste (constroiTestes (testesT1))

