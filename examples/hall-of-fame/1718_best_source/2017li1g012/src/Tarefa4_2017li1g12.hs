{-|
Module      : Tarefa4_2017li1g12
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g12 where

import LI11718
import Tarefa2_2017li1g12
import Testestarefa4

{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}

testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11,teste12,teste13,teste14,teste15,teste16]

{-|
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.
-}
atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
atualiza t e j a = Jogo mapa pista carrosAtualiz nitrosAtualiz historicoAtualiz
                 where Jogo mapa pista carros nitros historico = e
                       Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = pista
                       Acao acelera trava esquerda direita nitro = a
                       carrosAtualiz = atualizardirecao e pista k_nitro k_roda t j a nitros carros
                       nitrosAtualiz = acionarNitros j nitros nitro t  
                       historicoAtualiz = atualizaHistorico j e


{- | Partindo do estado atual do jogo, das propriedades do estado de jogo, da constante k_nitro, da constante k_roda, da duração da acção, do identificador do jogador, 
da acção tomada pelo jogador, do tempo restante de nitros dos carros e do estado dos carros, a função ’atualizardirecao’ vai  
vai devolver o estado dos carros com a direcção do carro do jogador atualizada.

-}

atualizardirecao :: Jogo -> Propriedades -> Double -> Double -> Tempo -> Int -> Acao -> [Tempo] -> [Carro] -> [Carro]
atualizardirecao jogo propriedades k_nitro k_roda time jogador accao nitros carros | (esquerda && direita) || (esquerda == False && direita == False)    = take jogador carrosWithNitro ++ [Carro pos dir vel] ++ drop (jogador + 1) carrosWithNitro 
                                                                                   | esquerda                                                            = take jogador carrosWithNitro ++ [Carro pos (dir + (k_roda * time)) vel] ++ drop (jogador + 1) carrosWithNitro 
                                                                                   | direita                                                             = take jogador carrosWithNitro ++ [Carro pos (dir - (k_roda * time)) vel] ++ drop (jogador + 1) carrosWithNitro 
                                                                                   where Jogo mapa pista carros nitros historico = jogo 
                                                                                         Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                                                         Carro pos dir vel = carrosWithNitro !! jogador 
                                                                                         Acao acelera trava esquerda direita nitro = accao
                                                                                         carrosWithNitro = maybeSomaNitro accao propriedades nitro jogador nitros time jogo   

{- | Partindo da duração da acção, das propriedades do estado de jogo, do identificador do jogador, do estado dos carros, do estado atual do jogo e da acção tomada pelo jogador a função ’maybeSomaNitro’ 
vai atualizar o carro do jogador após todas as forças, exepto a força de nitro, terem sido aplicadas no carro.

-}


maybeSomaNitro :: Acao -> Propriedades -> Maybe Int -> Int -> [Tempo] -> Tempo -> Jogo -> [Carro]
maybeSomaNitro accao propriedades nitro jogador nitros time jogo | not (maybeAcaoNitro (nitro))        = somaResultForcas time propriedades jogador carros jogo accao 
                                                                 | otherwise                           = somaNitroAfterSRF accao propriedades nitro jogador nitros time jogo 
                                                                  where Acao acelera trava esquerda direita nitro = accao
                                                                        Jogo mapa pista carros nitros historico = jogo


{- | Partindo da acção tomada pelo jogador, das propriedades do estado de jogo, da acção nitro tomada pelo jogador, do identificador do jogador, do tempo restante de nitros dos jogadores,
da duração da acção e do estado atual do jogo a função ’somaNitroAfterSRF’ vai devolver os estados dos carros com a atualização do carro em que a acção nitro foi imposta caso 
a acção nitro tomada pelo jogador tenha sido ativada.
-}


somaNitroAfterSRF :: Acao -> Propriedades -> Maybe Int -> Int -> [Tempo] -> Tempo -> Jogo -> [Carro]
somaNitroAfterSRF accao propriedades nitro jogador nitros time jogo = take gamerWithNitro carrosWithRF ++ [Carro pos dir velPlusNitro] ++ drop (gamerWithNitro + 1) carrosWithRF
                                                                    where Jogo mapa pista carros nitros historico = jogo 
                                                                          Acao acelera trava esquerda direita nitro = accao
                                                                          carrosWithRF = somaResultForcas time propriedades jogador carros jogo accao    
                                                                          Just gamerWithNitro = nitro                                          
                                                                          Carro pos dir vel = carrosWithRF !! gamerWithNitro
                                                                          Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                                          minTime = min time (nitros !! jogador)
                                                                          norma = minTime * k_nitro
                                                                          velNitro = polarToCartesiano (norma, dir)
                                                                          velPlusNitro = ((fst vel) + (fst velNitro),(snd vel) + (snd velNitro))

{- | Partindo da duração da acção, das propriedades do estado de jogo, do identificador do jogador, do estado dos carros, do estado atual do jogo e da acção tomada pelo jogador a função ’somaResultForcas’ 
devolver os estados dos carros com a atualização do carro do jogador após todas as forças, exepto a força de nitro, terem sido aplicadas no carro.

-}


somaResultForcas :: Tempo -> Propriedades -> Int -> [Carro] -> Jogo -> Acao -> [Carro] 
somaResultForcas time propriedades jogador carros jogo accao = take jogador carros ++ [Carro pos dir velPlusRF] ++ drop (jogador + 1) carros
                                                             where Carro pos dir vel = (carros !! jogador)
                                                                   Jogo mapa pista carros nitros historico = jogo
                                                                   resulF = resultforcas accao time jogador propriedades jogo 
                                                                   velPlusRF = ((fst vel) + (fst resulF),(snd vel) + (snd resulF))

{- | Partindo do identificador do jogador e do estado atual do jogo a função ’atualizaHistorico’ 
atualiza o histórico de posições de todos os jogadores.

-}

atualizaHistorico :: Int -> Jogo -> [[Posicao]]
atualizaHistorico jogador jogo | length historyGamerCar == 0              = take jogador historico ++ [[atualPos]] ++ drop (jogador + 1) historico 
                               | atualPos == (historyGamerCar !! 0)       = historico
                               | otherwise                                = take jogador historico ++ [[atualPos] ++ historyGamerCar] ++ drop (jogador + 1) historico 
                                where Jogo mapa pista carros nitros historico = jogo 
                                      historyGamerCar = (historico !! jogador)
                                      Carro pos dir vel = (carros !! jogador)
                                      atualPos = (floor (fst pos), floor (snd pos))

{- | Partindo da acção nitro tomada pelo jogador a função ’maybeAcaoNitro’ verifica se é verdade que o nitro foi usado.

-}


maybeAcaoNitro :: Maybe Int -> Bool
maybeAcaoNitro (Nothing) = False
maybeAcaoNitro (Just _ ) = True


{- | Partindo do identificador do jogador, do tempo restante de nitros dos jogadores, da acção nitro tomada pelo jogador e pelo tempo de duração da acção, a função ’acionarNitros’ 
atualiza os tempos de nitros com a função ´atualizarNitro´ caso o jogador tenha usado nitro.

-}

acionarNitros :: Int -> [Tempo] -> Maybe Int -> Tempo -> [Tempo]
acionarNitros jogador nitros acaoNitro time | maybeAcaoNitro (acaoNitro)        = atualizarNitro time jogador acaoNitro nitros
                                            | otherwise                         = nitros 


{- | Partindo da duração da acção, do identificador do jogador, da acção nitro tomada pelo jogador e do tempo de duração da acção a função ’atualizarNitro’ 
atualiza o tempo restante de nitro de todos os jogadores.

-}


atualizarNitro :: Tempo -> Int -> Maybe Int -> [Tempo] -> [Tempo]
atualizarNitro time jogador acaoNitro nitros | time >= (nitros !! jogador)             = take jogador nitros ++ [0] ++ drop (jogador + 1) nitros
                                             | otherwise                               = take jogador nitros ++ [gamerNitro - time] ++ drop (jogador + 1) nitros
                                              where gamerNitro = nitros !! jogador


{- | Partindo da acção tomada pelo jogador, da duração da acção, do identificador do jogador, das propriedades do estado do jogo e da estado do jogo a função ’resultforcas’ 
calcula a soma de todas as forças a atuar no carro do jogador.

-}


resultforcas :: Acao -> Tempo -> Int -> Propriedades -> Jogo -> Velocidade
resultforcas accao time jogador propriedades jogo = foldr (\ (x1,y1) (x2,y2) -> (x1+x2,y1+y2)) (0,0) forcaslist
                                                  where forcaslist = map (\f -> f accao time jogador propriedades jogo) [forcaAcel_Trava,forcaGrav,forcaPneus,forcaAtrito]



{- | Partindo de um ponto em coordenadas polares a função ’polarToCartesiano’ irá devolver o mesmo ponto em coordenadas cartesianas.

-}

polarToCartesiano :: Ponto -> Ponto
polarToCartesiano (norma,ang) = (norma * cos (gr ang), - (norma * sin (gr ang))) 
                              where gr = grausToRad

{- | Partindo um angulo em graus a função ’grausToRad’ irá devolver o mesmo angulo em radianos.

-}


grausToRad :: Double -> Double
grausToRad graus = (graus * (2 * pi)) / 360


{- | Partindo um angulo em radianos a função ’radToGraus’ irá devolver o mesmo angulo em radianos.

-}

radToGraus :: Double -> Double
radToGraus rad = ((rad * 360) / (2 * pi))


{- | Partindo de um ponto em coordenadas cartesianas a função ’cartesianoToPolar’ irá devolver o mesmo ponto em coordenadas polares.

-}

cartesianoToPolar :: Ponto -> Ponto
cartesianoToPolar (x,y) | (x == 0)            = (y,90)
                        | otherwise           = ((sqrt (x^2 + y^2)),normalizaAngulo)
                        where normalizaAngulo = normalizaAng angulo
                              angulo = anguloCartesianoToPolar (x,y)


{- | Partindo de um angulo a função ’normalizaAng’ irá devolver o angulo equivalente entre 0 e 360.

-}

normalizaAng :: Double -> Double
normalizaAng angulo | (angulo >= (-360)) && (angulo < 0)        = angulo + 360
                    | (angulo <= (360)) && (angulo >= 0)        = angulo 
                    | angulo < (-360)                           = normalizaAng (angulo + 360)
                    | angulo > 360                              = normalizaAng (angulo - 360)
                    | otherwise                                 = angulo


{- | Partindo de um ponto em coordenadas cartesianas a função ’anguloCartesianoToPolar’ irá calcular o angulo das coordenadas polares desse mesmo ponto.

-}

anguloCartesianoToPolar :: Ponto -> Double
anguloCartesianoToPolar (x,y) | x == 0 && y < 0    = -90
                              | x == 0 && y > 0    = 90
                              | x < 0 && y < 0     = (radToGraus (atan ((-y)/x))) - 180
                              | x < 0 && y >= 0    = (radToGraus (atan ((-y)/x))) + 180
                              | x > 0              = radToGraus (atan ((-y)/x))
  
{- | Partindo da acção tomada pelo jogador, da duração da acção, do identificador do jogador, das propriedades do estado do jogo e da estado do jogo a função ’forcaAcel_Trava’ 
irá calcular a força de aceleração ou de travagem a atuar no carro.

-}

forcaAcel_Trava :: Acao -> Tempo -> Int -> Propriedades -> Jogo -> Velocidade
forcaAcel_Trava accao time jogador propriedades jogo | acelera && trava    = (0,0)
                                                     | acelera             = polarToCartesiano ((k_acel * time), dir)
                                                     | trava               = polarToCartesiano ((k_acel * time), dir + 180)
                                                     | otherwise           = (0,0)
                                                     where Carro pos dir vel = carros !! jogador
                                                           Acao acelera trava esquerda direita nitro = accao
                                                           Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                           Jogo mapa pista carros nitros historico = jogo

{- | Partindo da acção tomada pelo jogador, da duração da acção, do identificador do jogador, das propriedades do estado do jogo e da estado do jogo a função ’forcaAtrito’
irá calcular a força de atrito a atuar no carro.

-}

forcaAtrito :: Acao -> Tempo -> Int -> Propriedades -> Jogo -> Velocidade
forcaAtrito accao time jogador propriedades jogo = polarToCartesiano (k_atrito * time * normaVi , anguloVi + 180)
                                                 where Carro pos dir vel = carros !! jogador 
                                                       (normaVi,anguloVi) = cartesianoToPolar (vel)
                                                       Acao acelera trava esquerda direita nitro = accao
                                                       Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                       Jogo mapa pista carros nitros historico = jogo

{- | Partindo da acção tomada pelo jogador, da duração da acção, do identificador do jogador, das propriedades do estado do jogo e da estado do jogo a função ’forçaPneus’ 
irá calcular a força dos pneus a atuar no carro.

-}

forcaPneus :: Acao -> Tempo -> Int -> Propriedades -> Jogo -> Velocidade
forcaPneus accao time jogador propriedades jogo | vel == (0,0)        = (0,0)
                                                | otherwise          = polarToCartesiano (normaForcaPneus ,dirPneus veliAng dir)
                                                where Carro pos dir vel = carros !! jogador 
                                                      (veliNorma , veliAng) = cartesianoToPolar vel
                                                      normaForcaPneus = sin (grausToRad ( (veliAng - dir))) * k_pneus * time * veliNorma
                                                      Acao acelera trava esquerda direita nitro = accao
                                                      Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                      Jogo mapa pista carros nitros historico = jogo



{- | Partindo da direcção da velocidade do carro e da direcção do carro a função ’dirPneus’ irá calcular a direcção do vetor referente à força dos pneus a atuar no carro.

-}

dirPneus :: Double -> Double -> Double
dirPneus dirvel dir | dirvel > dir   = dir - 90
                    | dirvel < dir   = dir + 90
                    | dirvel == dir  = dir 


{- | Partindo da acção tomada pelo jogador, da duração da acção, do identificador do jogador, das propriedades do estado do jogo e da estado do jogo a função ’forçaGrav’ 
irá calcular a força da gravidade a atuar no carro.

-}


forcaGrav :: Acao -> Tempo -> Int -> Propriedades -> Jogo -> Velocidade
forcaGrav accao time jogador propriedades jogo  | tipo == Rampa Norte    = pc (norma, (-90)) 
                                                | tipo == Rampa Sul      = pc (norma, 90) 
                                                | tipo == Rampa Este     = pc (norma, 0) 
                                                | tipo == Rampa Oeste    = pc (norma, 180)
                                                | otherwise              = (0,0)
                                                where Carro pos dir vel = carros !! jogador
                                                      pc = polarToCartesiano
                                                      Mapa (posicao,ori) tab = mapa
                                                      pospeca = ((floor (fst pos)),floor (snd pos))
                                                      norma = time * k_peso
                                                      Peca tipo altura = pegaPecaTab pospeca tab 
                                                      Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda = propriedades
                                                      Jogo mapa pista carros nitros historico = jogo


