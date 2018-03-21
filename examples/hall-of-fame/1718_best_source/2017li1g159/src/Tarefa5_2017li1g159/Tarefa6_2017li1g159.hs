{-|
Module      : Tarefa6_2017li1g159
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g159 where

import LI11718
import Direction
import Mapas
{-|
Este é o tipo defenido na Tarefa 3 que classifica os diferentes limites de uma peça.
-}
data Parede = Burn | Wall | Fall | Free | Trap deriving (Show,Eq)
-- data Bidimlist = Bid Dimensao [Peca] deriving Show
-------- ---------------------
{-|
Constante que corresponde ao numero de sensors que o carro terá.
-}
inSensors = 16

{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick e j = de_Module (j, mapa e) (vecFile (mapa e) ) (carros e !! j) (historico e!! j) ( sensors tab (carros e!! j) )
        where (Mapa _ tab) = mapa e
{-| 
Esta é a função que usa a grande maioria das funções defenidas neste módulo para 
obter a Ação final.
-}
de_Module :: (Int,Mapa) -> [( (Int,Int) ,Orientacao)] -> Carro -> [Posicao] -> [Double] -> Acao
de_Module (j,kl) m cc hist l | basicM kl    = (prop maxind j)
                             | otherwise    = g
            where ang  = direcao cc -- direcao atual do carro
                  maxind = maxi (replaceAtIndex' (maxi l) 0 l)  --  indereço da melhor entrada do sensor
                  goAng = ang + fromIntegral maxind *12 - 90 
                  action = caution (l!! maxind) (prop maxind j) --- ação ideal caso não houvesse limitações.
                  command = extract_p kl m hist
                  pseudoAng = functionalAng (fst command) ang (dif cc)
                  g = examin (l!!8) (delib goAng action pseudoAng command)
                        where examin :: Double->(Int , Acao)-> Acao
                              examin _ (1,ac)  = ac
                              examin x (0,ac)  = caution x ac

{-|
      Esta função tendo em consideração a direção do carro, devolve um ângulo ligeiramente diferente, tal que quando
      a função responsável por decidir a ação que o agente deve tomar ajuste a direção do carro no sentido de o manter o carro no centro da peça. 
-}
functionalAng :: Orientacao -> Angulo -> Ponto -> Angulo
functionalAng Norte ang (x,y) = ang + (45 - 90*x)
functionalAng Sul   ang (x,y) = ang + (90*x - 45)
functionalAng Este  ang (x,y) = ang + (45 - 90*y)
functionalAng Oeste ang (x,y) = ang + (90*y - 45)

{-|
Esta função verifica se o mapa em que o agente se encontra é um dos simples ou é um dos mais complexo. 
-}
basicM :: Mapa -> Bool
basicM m =  False
{-|
Esta função usando a lista com o caminho idal a percorrer e o historico 
de posições do agente. Devolve a orientação indicada que o agente deve seguir.
-}
extract_p :: Mapa -> [ (Posicao ,Orientacao)] -> [Posicao] -> (Orientacao,Bool)
extract_p kl l [] = (Este, True)
extract_p kl l t  = cmp (head t) (length t - 1) (kl,l) --cmp (l !!( length t - 1 ) )
      where cmp :: Posicao -> Int -> (Mapa,[ (Posicao ,Orientacao)])-> (Orientacao,Bool)
            cmp _ (-1) _                = (Este, True)
            cmp p n (kl,l) | n>0  && fst (l!!n) == p = ( snd (l!!n), (isCurv kl (fst (l!!(n-1)) )) && (isCurv kl (fst (l!! (n) )) ) && (isCurv kl (fst (l!! (n+1) )) ) )
                           | n==0 && fst (l!!n) == p = ( snd (l!!n) , True)
                           | otherwise = cmp p (n - 1) (kl,l)

                  where isCurv :: Mapa -> Posicao -> Bool
                        isCurv (Mapa _ tb) (x,y) = let (Peca tp _ ) = (tb !! y) !!x in peek tp
                                                
                              where peek :: Tipo -> Bool
                                    peek (Curva _ ) = True
                                    peek _          = False
{-|
Esta função substitui um elemento de uma por outro no indice indicado.

      ==Exemplos de utilização:

      >>> replaceAtIndex 2 10 [0,0,0,0,0]
      [0,0,10,0,0]

      >>> replaceAtIndex 4 11 [3,1,2,6,3]
      [3,1,2,6,11]

-}
replaceAtIndex' :: Int -> a -> [a] -> [a]
replaceAtIndex' n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls
{-|
Esta função tenta decidir qual das ação é a mais indicada nas presentes circunstâncias do agente.

As unicas alternativas são a ação devolvida pela função 'bruteAct', 'major_adj' ou a
calculada usando a informação dos sensores.

-}           
delib :: Angulo -> Acao -> Angulo ->(Orientacao,Bool) ->(Int,Acao)
delib goAng ac dir (o,v) | v                             = (1 , ac)
                         | u /= Nothing && Just o == u   = (1 , ac)
                         | brute (aux dir) /= o          = (0 ,bruteAct o (brute (aux dir) ) )
                         | otherwise                     = (0 ,(major_adj o (aux dir)) { acelerar = True } )
      where u = assimpt (aux goAng)
            aux :: Angulo -> Angulo
            aux x | x<0 = aux (360 + x )
                  | x>360 = aux (x - 360) 
                  | otherwise = x 

-- caso esteja.

{-| 
Sendo que se considera o ângulo representativo da orientação Este, Norte,Oeste e Sul, respetivamenete 0 90 180 270.

O ângulo recebido terá que ter uma diferença inferior ou igual a 45 graus do angulo representativo da orientação recebida.

E esta função vai devolver a áção que tornará o Angulo da direção do carro mais proximo do angulo representativo da 
orientação recebida, caso este tenha uma distância superior a 10 do idela e vai acelerar caso contrário.

-}
major_adj :: Orientacao -> Angulo -> Acao
major_adj Este x  | x >= 315 && x < 358  = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing }    
                  | x >= 2 && x  <= 45  = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }
 
major_adj Norte x | x < 88  = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing }  
                  | x > 92 = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

major_adj Oeste x | x < 178 = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing } 
                  | x > 182 = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

major_adj Sul x   | x < 268 = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing } 
                  | x > 272 = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

major_adj _ x     = Acao {acelerar = True ,travar = False ,esquerda = False ,direita = False , nitro = Nothing }--- change according to j

-- guidelines --- my actual direction
{-| 
Esta função leva a cabo a tarefa de devolver a ação que corrigir a direção do agente.
-}
bruteAct :: Orientacao -> Orientacao -> Acao
bruteAct Norte Este  = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing } 
bruteAct Norte Oeste = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }
bruteAct Norte Sul   = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

bruteAct Sul Este  = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing } -- troquei
bruteAct Sul Oeste = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing } -- troquei
bruteAct Sul Norte = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

bruteAct Este Sul   = Acao {acelerar = False ,travar = False ,esquerda = True,direita = False , nitro = Nothing }
bruteAct Este Norte = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing } 
bruteAct Este Oeste = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False, nitro = Nothing }

bruteAct Oeste Sul    = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing } 
bruteAct Oeste Norte  = Acao {acelerar = False ,travar = False ,esquerda = True ,direita = False , nitro = Nothing }
bruteAct Oeste Este   = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }

{-|
Sendo que se considera o ângulo representativo da orientação Este, Norte,Oeste e Sul, respetivamenete 0 90 180 270.

Esta função tenta determinar a orientação associada a um ângulo sendo que para isso esse ângulo terá que ter uma 
distância inferior a 20 do ângulo representativo da orientação em questão. 
-}
assimpt :: Angulo -> Maybe Orientacao
assimpt ang | ang >=350 || ang <= 10   = Just Este
            | ang >=80 && ang <=100    = Just Norte
            | ang >= 170 && ang <= 190 = Just Oeste
            | ang >= 260 && ang <=280  = Just Sul
            | otherwise                = Nothing
{-|
Sendo que se considera o ângulo representativo da orientação Este, Norte,Oeste e Sul, respetivamenete 0 90 180 270.

Esta função tenta determinar a orientação associada a um ângulo sendo que para isso esse ângulo terá que ter uma 
distância inferior a 45 do ângulo representativo da orientação em questão. 
-}
brute :: Angulo -> Orientacao
brute   ang | ang >=315 || ang < 45   = Este
            | ang >=45 && ang < 135   = Norte
            | ang >= 135 && ang < 225 = Oeste
            | ang >= 225 && ang <315  = Sul
{-| 
Esta função tenta muderar a aceleração da Ação em função da previsão do tempo de chegada a uma parede.  
-}
caution :: Double -> Acao -> Acao      
caution x ct | x > 0.2 && x <= 0.5   = ct { acelerar = False ,  nitro = Nothing }
             | x <= 0.2   = ct { acelerar = False , travar = True , nitro = Nothing }
             | x > 0.5 && x < 0.7   = ct { nitro = Nothing }
             | otherwise = ct
{-|

É possível com o indice do maior elemento da lista de tempos do sensor, determinar a direção mais 
pragmática e daí deduzir a ação que faz o agente segui-la.

-}
prop :: Int -> Int ->Acao
prop x j | x<2         = Acao {acelerar = False ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }
         | x>13        = Acao {acelerar = False ,travar = False,esquerda = True ,direita = False , nitro = Nothing }
         | x>9 && x<14 = Acao {acelerar = True ,travar = False ,esquerda = True ,direita = False , nitro = Nothing }
         | x>1 && x <7 = Acao {acelerar = True ,travar = False ,esquerda = False ,direita = True , nitro = Nothing }
         | x == 7      = Acao {acelerar = True ,travar = False ,esquerda = False ,direita = True , nitro = Just j  }
         | x == 8      = Acao {acelerar = True ,travar = False ,esquerda = False ,direita = False , nitro = Just j }
         | x == 9      = Acao {acelerar = True ,travar = False ,esquerda = True ,direita = False , nitro = Just j  }

{-|
Esta função calcula o índice do maior elemente de uma lista.

      ==Exemplos de utilização:

      >>> maxi [3,5,45,5,4,25,2]
      2

      >>> maxi [43,2325,4245,55,6,5225,22]
      5


-}
maxi :: Ord a => [ a ] -> Int
maxi l = snd $ maximum $ zip l [0..]

{-|
Esta função recebe uma qualquer 'Velocidade' e calcula o angulo desta. 

      ==Exemplos de utilização:

      >>> calAngulo (-3,2)
      213.69006752597977

      >>> calAngulo (3,6)
      296.565051177078

-}
calAngulo :: Velocidade -> Angulo
calAngulo (vx,vy) | vx>0 && vy>0   = 360 - atan (vy/vx) * 180/pi  -- outfix(180/pi  * (-1 )* atan (vy/vx)) *  pi/180
                  | vx<0 && vy<0   = 180 - atan (vy/vx) *180/pi 
                  | vx>0 && vy<0   = (-1)* atan (vy/vx) * 180/pi -- (pi/180  *)  $ outfix $ (atan (vy/vx) + pi/2 )* 180/pi
                  | vx<0 && vy>0   = 180 + (-1) * atan (vy/vx) *  180/pi
                  | vx==0 && vy > 0 = 270
                  | vx==0 && vy < 0 = 90
                  | vx >= 0 && vy==0 = 0 
                  | vx < 0 && vy==0 = 180
{-| 
O Resultado desta função é uma lista de tempos, que corresponderá ao tempo que
16 sensores , à velocidade do carro, em direções diferentes ( expalhados por 180 graus )
demoraram a encontram um limite.
-}
sensors :: Tabuleiro ->Carro -> [Tempo]
sensors tab = identify (reshape tab)
    where reshape :: Tabuleiro -> Bidimlist 
          reshape (h:t) = Bid ( length h , length t + 1 ) (concat (h:t) )

{-|

Esta função forne-se os argumentos necessários à função single. Começando por calcular a norma do carro.
A norma do será a velocidade a que todos os sensores usarão. 
-}
identify :: Bidimlist -> Carro -> [Double]
identify tab cc = single inSensors cc r ( retStone tab (get_p cc) , tab) w
        where norm (x,y) = sqrt (x^2 + y^2)
              w = outfix (direcao cc - 90 ) 
              r = aux (norm (velocidade cc) )
                  where aux :: Double -> Double
                        aux 0 = 1
                        aux n = n

{-| 
Esta função repete 'searchPath' em várias direções e guarda os valores numa lista. 
Com esta lista é possível construir uma representação primitiva do ambiente em volta da origem dos 'sensors'.

-}
single :: Int -> Carro -> Double -> (Peca,Bidimlist) -> Angulo -> [Tempo]
single 0 _ _ _ _      = []          
single n p norm (pec,tab) k = searchPath 0 pec (p,u) tab : single (n-1) p norm (pec,tab) ( k + ang)
            where u = scale' (norm*) $ simplify (get_vec k)

{-|
Esta função corrige possíveis erros com arredondamentos ao usar funções trignométricas e a constante pi.
-}            
simplify :: Velocidade -> Velocidade
simplify (vx,vy) |  vx ^2 < 1.0e-8  && vx ^ 2 > 0 = simplify (0,vy)
                 |  vy ^2 < 1.0e-8                = (vx, 0 )
                 | otherwise                      = (vx,vy )

-------------------------------------------------------

{-|
Recebe um Carro e retorna a coordenada no tabuleiro em que o carro se encontra. 

-}
get_p :: Carro -> Posicao
get_p c = scale' floor (posicao c)


{-|
Esta é a principal função para o calculo dos sensors. É uma função recursiva que vai acomulando
o tempo que um dado sensor demora até encontrar um limite numa dada direção específica. 

-}
searchPath :: Tempo ->Peca -> ( Carro, Velocidade ) -> Bidimlist -> Tempo
searchPath _ _ ( _ ,(0,0) ) _ = 1 
searchPath t (Peca (Rampa o) z ) (c , v) tab | pred      = searchPath ( t + dt ) (retStone tab (get_p nc)) (nc ,v) tab
                                             | otherwise =  t + dt

        where dt = if beJust stage == (0,0) then 0 else intersecT v (dif c) (beJust stage)
              nc = c { posicao = adj v (segue v dt (posicao c)) }
              pred = isBound $ gather stage (Peca (Rampa o) z ) c tab
              stage = compose (filter (boolA (dif c) v 100000 ) l1)

searchPath t (Peca (Curva o) z ) (c , v) tab | pred      = searchPath ( t + dt ) (retStone tab (get_p nc)) (nc ,v) tab
                                             | otherwise = t + dt 

        where  dt    = walltp o (dif c) v (beJust stage) 
               nc    = c { posicao = adj v (segue v dt (posicao c)) }
               pred  = isBound $ gather stage (Peca (Curva o) z ) c tab
               stage = compose (filter (boolC o (dif c) v 100000 ) l2) 

searchPath t (Peca Recta z ) (c , v) tab   | pred      = searchPath ( t + dt ) (retStone tab (get_p nc)) (nc ,v) tab
                                           | otherwise =  t + dt

        where dt    = if beJust stage == (0,0) then 0 else intersecT v (dif c) (beJust stage)
              nc    = c { posicao = adj v (segue v dt (posicao c)) }
              pred  = isBound $ gather stage (Peca Recta z ) c tab
              stage = compose (filter (boolA (dif c) v 100000 ) l1) 
searchPath t _ _ _ = t    
   

{-|
Verifica se uma dada 'Parede' é um limite.
-}
isBound :: Parede -> Bool
isBound Free = True
isBound _    = False

{-| 
Soma de dois vetores.
-}
soma' :: Num a => (a,a) -> (a,a) -> (a,a)                                         -- FALTA FORÇA Gravítica. 
soma' (x1,x2) (y1,y2) = (x1+y1, x2+y2) 

{-|
Soma um pequeno valor na direção da velocidade.
-}
adj :: Velocidade -> Ponto -> Ponto
adj v = soma' (scale' (1.0e-10 *) v ) 

{-|
Calcular o tempo de interseção com um limite numa Curva.
-}                    
walltp :: Orientacao ->  Ponto -> Velocidade-> (Int,Int)-> Tempo
walltp _ _ _ (0,0) = 0
walltp o (x,y) (vx,vy) v     | v /= ( 1 , 1 ) && v /= ( 1 , - 1 )  && (rot180 o == inloc v || rot270 o == inloc v) = intersecT (vx,vy) (x,y) v
walltp _ (x,y) (vx,vy) v                                                                                           = insecT (vx,vy) (x,y) v

{-|
Angulo entre cada sensor.
-}
ang :: Angulo
ang = 180 / fromIntegral (inSensors-1)


----------------  Tarefa 3 ---------------------------------------------------

{-|
      Recebe uma lista e devolve no caso de vazia Nothing e caso esta tenha elementes devolve o primeiro.
         == Exemplos de utilização:

      >>> compose [1,2,3,4,5]
      Just 1

      >>> compose []
      Nothing
-}
compose :: [ a ] -> Maybe a
compose []                                   = Nothing
compose l                                    = Just (head l) 

{-| 
      Lista de todas as possibilidades de resposta para a função 'searchPath', no caso do carro estar sobre a peca Rampa ou Recta.
 -}
l1 :: [(Int, Int)]             
l1 = [(0,1),(0,-1),(1,0),(-1,0),(0,0)]

{-| 
      Lista de todas as possibilidades de resposta para a função 'searchPath', no caso do carro estar sobre a peca Curva.
 -}
l2 :: [(Int, Int)]            
l2 = [(0,1),(0,-1),(1,0),(-1,0),(1,1),(1,-1),(0,0)]

{-|
      boolA é um filtro para ser usado como argumento da função filter sobre a lista l1 ou l2 quando o carro está a dirigir-se para uma das quatro arestas da peca. 

      == Exemplos de utilização:

      >>> boolA (0.9999999999,0.499999999999) (-1 , 0 ) 0.5 (0, -1) 
      False

      >>> boolA (0.5,0.5) (1,0) 1 (1,0) 
      True
-} 
boolA :: Ponto -> Velocidade-> Tempo -> (Int,Int)  -> Bool
boolA (x,y) (vx,vy) dt (v1,v2)   | v1== 0 && v2== 0 = True 
                                 | otherwise        = mu >0 && dt >= mu
            where mu  = intersecT (vx,vy) (x,y) (v1,v2)

{-|
      boolC é também um filtro para ser usado como argumento da função filter sobre a lista l2 quando o carro está a dirigir-se para uma das 2 diagonais da Peca Curva. 

      == Exemplos de utilização:

      >>> boolC Sul (0.7, 0.233) (3,1) 1 (1,1)
      False

      >>> boolC Norte (0.7, 0.233) (3,1) 1 (1,-1)
      True
      
-} 
boolC :: Orientacao ->  Ponto -> Velocidade-> Tempo -> (Int,Int)-> Bool
boolC _ _ _ _ (0,0)                                                  = True
boolC o (x,y) (vx,vy) dt v     | v /= ( 1 , 1 ) && v /= ( 1 , - 1 )  = (rot180 o == inloc v || rot270 o == inloc v) && boolA (x,y) (vx,vy) dt v 
boolC o (x,y) (vx,vy) dt v     | inWall o /= v                       = False
boolC _ (x,y) (vx,vy) dt (v1,v2)                                     = (\ a t -> a<= t && a >0 ) (insecT (vx,vy) (x,y) (v1,v2) ) dt   

{-|
      Esta função com a orientação de uma peça do tipo Curva diz qual é perede interior dessa curva ("inner wall").

      == Exemplos de utilização:

      >>> inWall Norte 
      (1,-1)

      >>> inWall Este
      (1,1)

-}
inWall :: Orientacao -> (Int, Int)
inWall teta | teta == Norte || teta == Sul  = (1, -1)
inWall teta | teta == Oeste || teta == Este = (1,1) 

{-|
      Esta função trata de decidir após de indentificada a primeira etapa do sensor qual é a função ou funções que deve calcular as restantes etapas. 
-}
gather :: Maybe (Int,Int) -> Peca -> Carro -> Bidimlist ->Parede
gather Nothing key vi _                     = Burn
gather ( Just (0,0) ) key vi _              = Wall
gather ( Just d ) (Peca (Rampa o) z) vi tab = smp upwardcheck d vi tab z o 
gather ( Just d ) (Peca (Curva o) z) vi tab = if d == (1,1) || d == (1,-1) then Wall else smp forwardcheck d vi tab z 
gather ( Just d ) (Peca Recta z) vi     tab = smp forwardcheck d vi tab z 

{-|
      Esta função de nivel superior é essencialmente usada para diminuir a complexidade da função 'gather'.
-}
smp :: (Orientacao -> (Int, Int) -> t) -> (Int, Int) -> Carro -> t
smp f d vi = f (inloc d ) ( (\(a,b) (c,d) -> (a+c, b+d) )(get_p vi) d ) 

{-|
      Esta função calcula a interseção do carro com uma das duas diagonais de uma peca Curva.

      == Exemplos de utilização:

      >>> insecT (0.2,-4.8) (0.1,0.7 )(1,1) 
      0.11999999999999997

      >>> insecT (0.2,-4.8) (0,0.7 )(1,-1) 
      -1.0 // Não interseta
-}
insecT :: Velocidade -> Ponto -> (Int,Int) -> Tempo
insecT (0 ,vy) (x,y) (1,1)                        = findT x y vy
insecT (0 ,vy) (x,y) (1,-1)                       = findT (1 - x) y vy
insecT (vx ,0) (x,y) (1,1)                        = findT y x vx 
insecT (vx ,0) (x,y) (1,-1)                       = findT (1 - y) x vx
insecT (vx,vy) (x,y) (1,1)   | (vy / vx ) ==  1   = -1
insecT (vx,vy) (x,y) (1,-1)  | (vy / vx ) == -1   = -1
insecT v x (1,1 )                                 = if (\ a -> a<= 1 && a >=0 ) (crX1 x v )                                             then findT (crX1 x v ) (fst x) (fst v) else (-1)
insecT v x ( 1,-1 )                               = if (\ f x y ->  f x && f y ) (\ a -> a<= 1 && a >=0 ) (crX2 x v ) (1 - crX2 x v  )  then findT (crX2 x v ) (fst x) (fst v) else (-1)

{-|
      Esta função limita-se a encontrar o tempo de interseção do carro com uma das quatro arestas. 
      
      == Exemplos de utilização:

      >>> intersecT (0.2,-4.8) (0.1,0.7 )(1,0) 
      -4.0 // Não chega a intersetar-se dentro da peca.

      >>> intersecT (0.2,-4.8) (0.1,0.7 )(0,-1) 
      0.1458333333333333333334
-}
intersecT :: Velocidade -> Ponto -> (Int,Int) -> Tempo
intersecT (0 ,vy) (x,y) (0,-1 ) = findT 0 y vy
intersecT (0 ,vy) (x,y) (0 ,1 ) = findT 1 y vy
intersecT (vx ,0) (x,y) (-1,0 ) = findT 0 x vx
intersecT (vx ,0) (x,y) ( 1,0 ) = findT 1 x vx
intersecT (vx,vy) (x,y) (-1,0 ) = if (\ a t -> a<= t && a >=0 )( y - (vy/vx)* x ) 1     then findT 0 x vx else (-4)
intersecT (vx,vy) (x,y) ( 1,0 ) = if (\ a t -> a<= t && a >=0 )( y + (1-x)* (vy/vx) ) 1 then findT 1 x vx else (-4)
intersecT (vx,vy) (x,y) ( 0,-1) = if (\ a t -> a<= t && a >=0 )( x - y* (vx/vy) ) 1     then findT 0 y vy else (-4)
intersecT (vx,vy) (x,y) ( 0,1 ) = if (\ a t -> a<= t && a >=0 )(x + (1-y)* (vx/vy)) 1   then findT 1 y vy else (-4)
intersecT _ _ _ = -4


{-|
      Calcula a primeira componente das cordenadas da interseção do carro com a diagonal da Curva de orientação Este e Oeste. 

      == Exemplos de utilização:

      >>> crX1 (4,2) (1, 9)
      4.25

      >>> crX1 (4,2) (1,- 9)
      3.8
      
-}
crX1 :: Ponto -> Velocidade -> Double
crX1 (x,y) (vx,vy) = (y * vx - x * vy) / (vx -vy)

{-|
      Calcula a primeira componente das cordenadas da interseção do carro com a diagonal da Curva de orientação Norte e Sul. 

      == Exemplos de utilização:

      >>> crX2 (3,2) (1,2)
      1.6666666666667

      >>> crX2 (0.11,2) (900,80)
      -0.9093877551020408
-}
crX2 :: Ponto -> Velocidade -> Double
crX2 (x,y) (vx,vy) = (x * vy + (1-y) * vx) / (vx +vy) 

{-|
      Recebe uma posicão final, uma posição inicial e uma velocidade e devolve o tempo que demora a chegar à posição final partindo da inicial à velocidade recebida.

      == Exemplos de utilização:

      >>> findT 4 2 1 
      2

      >>> findT 9.3232 3.22221 4.525299
      1.3481959976567293
-}
findT:: Double -> Double -> Double -> Double
findT f i v = (f - i)/v

{-|
      Esta função recebe um carro como argumento e devolve a sua posição relativa na Peca.

      == Exemplos de utilização:

      >>> dif (Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (3.5,11.1), direcao = 45, velocidade = (1,0)})
      (0.5,0.1)
     
-}            
dif :: Carro -> Ponto
dif vi = relative (get_p vi) (posicao vi)
      where relative :: Posicao -> Ponto -> Ponto
            relative (xo,yo) (x,y) = ( x - fromIntegral xo , y - fromIntegral yo)

{-|
      Recebe uma aresta de uma dada 'Peca' e devolve a orientação da parede relativa ao centro da Peca.

      ==Exemplos de utilização:

      >>> inloc (-1, 0)
      Oeste

      >>> inloc (0 ,-1)
      Norte
-}
inloc :: (Int,Int) -> Orientacao
inloc (0 ,-1) = Norte 
inloc (0 , 1) = Sul    
inloc (1 , 0) = Este 
inloc (-1, 0) = Oeste 

{-|
      Recebe uma 'Orientacao' e devolve a rotação de 180 graus dessa Orientação . 

      ==Exemplos de utilização:

      >>> rot180 Este
      Oeste

      >>> rot180 Sul
      Norte

-}
rot180 :: Orientacao -> Orientacao
rot180 Norte = Sul
rot180 Sul   = Norte
rot180 Este  = Oeste
rot180 Oeste = Este

{-|
      Recebe uma 'Orientacao' e devolve a rotação de 270 graus dessa Orientação . 

      ==Exemplos de utilização:

      >>> rot270 Norte
      Este

      >>> rot270 Este
      Sul

-}
rot270 :: Orientacao -> Orientacao 
rot270 Norte = Este
rot270 Sul   = Oeste
rot270 Este  = Sul
rot270 Oeste = Norte

{-|
      Esta função recebe um carro como argumento e devolve a sua posição relativa na Peca.

      == Exemplos de utilização:

      >>> dif (Carro {posicao = (3.5,11.5), direcao = 45, velocidade = (1,0)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (2.5,11.5), direcao = 45, velocidade = (-1,-0.23)})
      (0.5,0.5)

      >>> dif (Carro {posicao = (3.5,11.1), direcao = 45, velocidade = (1,0)})
      (0.5,0.1)
     
-}

forwardcheck :: Orientacao-> Posicao -> Bidimlist -> Altura -> Parede 
forwardcheck teta (x,y) l z | nextH > z                                                                                                 = Wall 
                            | nextP == Recta && nextH == z                                                                              = Free 
                            |(nextP == Rampa teta && nextH == z)  || (nextP == Rampa (rot180 teta) && nextH == (z-1))                   = Free
                            |(nextH ==(z-1) || nextH == z) && (nextP == Rampa (rot180 (rot270 teta)) || nextP == Rampa (rot270 teta)  ) = Wall -- Free 
                            | nextP == Rampa (rot180 teta) && nextH == z                                                                = Wall  
                            | nextH == z &&   (nextP == Curva teta || nextP == Curva (rot270 teta) )                                    = Free 
                            | nextH < z                                                                                                 = Fall 
                            | otherwise                                                                                                 = Burn 
                                          where nextP = peek (x,y) l
                                                nextH = peekHei (x,y) l

{-|
      Esta função tem o mesmo objetivo que a função forwardcheck no entanto o seu domínio é apenas Pecas do tipo Rampa. Em certos casos fá-lo com a função forward check.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> upwardcheck Este (0,1) t 1 Norte
      Trap

      >>> upwardcheck Sul (1,1) t 1 Oeste
      Fall


-}
upwardcheck :: Orientacao -> Posicao -> Bidimlist -> Altura -> Orientacao -> Parede
upwardcheck teta (x,y) l z dirR   | teta == dirR                                                                           = forwardcheck teta (x,y) l (z+1)                                     
                                  | teta == rot180 dirR                                                                    = forwardcheck teta (x,y) l z                                         
                                  | nextP == Recta && (nextH == z || nextH == (z+1) )                                      = Free 
                                  | (nextP == Curva teta || nextP == Curva (rot270 teta) )&& (z== nextH || (z+1) == nextH) = Free 
                                  | nextP == Rampa teta && (nextH == z || nextH == (z+1) )                                 = Wall -- Free
                                  | nextP == Rampa (rot180 teta) && ( nextH == z || nextH == (z-1) )                       = Wall -- Free           
                                  | (nextP == Rampa dirR || nextP == Rampa (rot180 dirR) )&& nextH == z                    = Wall -- Free 
                                  | nextP == Rampa dirR && nextH == z-1                                                    = Fall
                                  | ( nextP == Rampa (rot180 dirR) ) && nextH == z-1                                       = Trap
                                  | nextH < z                                                                              = Fall 
                                  | nextH > z                                                                              = Wall
                                  | otherwise                                                                              = Burn 
                                                       where nextP = peek (x,y) l
                                                             nextH = peekHei (x,y) l
{-|
      Esta função multiplica o tempo pelo vetor velocidade e soma o vetor posição para obter a nova posição.

      == Exemplos de utilização:

      >>> segue (3,4) 0.5 (2,1)
      (3.5, 3.0)

      >>> segue (1.2,0.3333333) 7.8 (0,1)
      (9.36,3.59999974)
      
-}

segue :: Num a => (a,a) -> a -> (a,a) -> (a,a)   
segue (vx,vy) t (xo,yo) = (vx*t + xo ,  vy*t + yo )

{-|
      Esta função recebe a 'Posicao' de uma Peca e uma 'Bidimlist' e diz qual é o tipo dessa 'Peca'.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> peek (0,0) t
      Curva Este

      >>> peek (0,1) t
      Rampa Sul 


-}
peek :: Posicao -> Bidimlist -> Tipo
peek (x,y) tab = (\ (Peca y z) -> y) ( retStone  tab ( x, y ) )

{-|
      Esta função recebe a 'Posicao' de uma Peca e uma 'Bidimlist' e diz qual é a Altura dessa 'Peca'.

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 3,Peca Lava 1,Peca (Rampa Sul) 0,Peca Lava 5,Peca Recta 0]

      >>> peekHei (0,0) t
      0

      >>> peekHei (1,1) t
      5 

-}
peekHei :: Posicao -> Bidimlist -> Altura
peekHei (x,y) tab = (\ (Peca y z) -> z) ( retStone tab ( x, y ) )

---------------------------------------------------------------------

--------- Tarefa 4
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

{-|
Esta função é uma função de ordem superior que recebe uma função e um tuplo e aplica essa função a cada elemento desse tuplo.

      ==Exemplos de utilização:

      >>> scale' (4*) (1,1)
      (4,4)

      >>> scale' (length) ([3,2,1],[6])
      (3,1)

-}
scale' :: (a->b) -> (a, a)->(b,b)
scale' f (x,y) = (f x , f y)   

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
Esta função recebe um ângulo e devolve o vetor de comprimento 1 com essa direção. 

      ==Exemplos de utilização:

      >>> get_vec 37
      (0.7986355100472928,-0.6018150231520483)

      >>> get_vec 62.05
      (0.46870086699128183,-0.8833569478311816)

-}
get_vec :: Angulo -> (Double,Double)
get_vec w = ( cos ( w /180 * pi ) , - sin ( w / 180 * pi ) )

------------------------------------------------------------------------
                                    
                  