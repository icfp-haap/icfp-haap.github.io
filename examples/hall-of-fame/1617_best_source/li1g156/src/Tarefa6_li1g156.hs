{-|
Module : Tarefa6_li1g156
Description : Implementar estratégia de combate.
Copyright : Pedro Pinto <a80741@alunos.uminho.pt.com>;
            Pedro Lima <a80328@alunos.uminho.pt>

O objetivo desta tarefa é o desenvolvimento de um @bot@ que jogasse @Bomberman@ sem qualquer apoio humano.

-}

module Tarefa6_li1g156 where
import Data.Char
import Data.List
import System.Random
import Tarefa2
import TarefaSqrt16
import Tarefa1

type Mapa     = [String]
type Cords    = (Abcissa, Ordenada)                                       -- ^ (Abcissa,Ordenada)
type State    = ( Mapa, [Cords], [Cords], [Powers], [Bombs] , [Players])  -- ^ (Mapa, [lista de espaços vazios, por onde se pode andar], [lista de tijolos], [lista de powerups], [lista de bombas], [lista de players])
type Bombs    = (Int,[Cords])                                             -- ^ (Instantes que faltam para explodir, [lista de espaços que vão arder])
type Players  = (Cords,Int,Int)                                           -- ^ (Coordenadas, numero de bombas que pode pôr, range das bombas).
type Powers   = (Char,Cords)                                              -- ^ (Tipo de powerup (1=bombs,2=flames), coordenadas).
type Caminho  = String                                                    -- ^ Caminho desde um espaço do mapa a outro, sob a forma de uma lista de instruções (chars).
type Abcissa  = Int                                                       -- ^ Abcissa de um espaço no mapa.
type Ordenada = Int                                                       -- ^ Ordenada de um espaço no mapa.
type Range    = Int                                                       -- ^ Raio de uma bomba colocada no mapa.
type Time     = Int                                                       -- ^ Instantes de tempo para o jogo terminar.
type Dim      = Int                                                       -- ^ Dimensão do mapa.
type Jogador  = Int                                                       -- ^ Número que representa um dado jogador.
type Linha    = String                                                    -- ^ Uma linha da representação doe stado do jogo no tipo @Mapa@


data RPath a = C a [RPath a] deriving (Eq, Show)
-- ^ Novo tipo de dados para tornar possível a criação de um variado conjunto de movimentos "óptimos" para o bot.






bot :: Mapa -> Time -> Jogador -> Maybe Char
bot mapa p t = if t > 49 then botState1 (mkState mapa) p else botState2 (mkState mapa) p t












botState1 :: State -> Jogador -> Maybe Char 
-- ^ Função que determina a ação do bot quando o mapa ainda não tem um tamanho demasiado reduzido.
botState1 (a,b,c,d,e,f) p   | immaDie s (m,n)                                                           = if st1 /= "" then  Just     (head st1)       else Just 'B' 
                            | (m,n) /= (findPoint (rmDups (network 2 b (m,n))) (lastSquare s) (m,n))    = if st2 /= "" then  safe s p (head st2)       else Just 'B'
                            | (m,n) == lastSquare s                                                     =                                                   Nothing                                                       
                            | otherwise                                                                 =                                                   Just 'B'
                         where (m,n)  = cordsPlayer a p
                               s      = (a,b,c,d,e,f)
                               st1    = runAway 1 s (m,n)
                               st2    = findWay 1 s (m,n) (findPoint (rmDups (network 6 b (m,n))) (lastSquare s) (m,n))
 

botState2 :: State -> Time -> Jogador -> Maybe Char 
-- ^ Função que determina a ação do bot quando o mapa se encontra bastante reduzido devido à espiral.
botState2 (a,b,c,d,e,f) p t | t <= 1                                                                                       =                                                     Nothing
                            | t < 25 && (m,n) == head (unsafeSpiral t o)                                                   = if st3 /= "" then Just (head st3)            else   Just (head st4)
                            | t < 11 && ((m,n) == lastSquare s) && (immaDie1 t s (m,n) == False || timers (m,n) s > t)     =                                                     Just 'B'
                            | t < 11 && ((m,n) == lastSquare s) && timers (m,n) s <= t + (length $ runAway1 t 1 s (m,n))   =                                                     Nothing
                            | immaDie1 t s (m,n)                                                                           = if st1 /= "" then  Just     (head st1)       else   Just 'B' 
                            | (m,n) /= (findPoint (rmDups (network 2 b (m,n))) (lastSquare s) (m,n))                       = if st2 /= "" then  safe s p (head st2)       else   Just 'B'
                            | (m,n) == lastSquare s                                                                        =                                                     Nothing                                                       
                            | otherwise                                                                                    =                                                     Just 'B'
                         where (m,n)  = cordsPlayer a p
                               s      = (a,b,c,d,e,f)
                               st1    = runAway1 t 1 s (m,n)
                               st2    = findWay 1 s (m,n) (findPoint (rmDups (exc (unsafeSpiral t o)(network 6 b (m,n)))) (lastSquare s) (m,n))
                               o      = length $ head a
                               st3    = (findWay 1 s (m,n) (head $ drop 1 (unsafeSpiral t o)))
                               st4    = if (findWay 1 s (m,n) (lastSquare s)) /= "" then (findWay 1 s (m,n) (lastSquare s)) else "B"





















safe :: State -> Jogador -> Char -> Maybe Char
-- ^ Função que verifica se uma jogada leva um jogador a estar em risco de morrer para uma bomba.
safe (a,b,c,d,e,f) p k | k == 'U'                    = if immaDie s (m,n-1) then Nothing else Just k
                       | k == 'D'                    = if immaDie s (m,n+1) then Nothing else Just k
                       | k == 'L'                    = if immaDie s (m-1,n) then Nothing else Just k
                       | k == 'R'                    = if immaDie s (m+1,n) then Nothing else Just k
                     where (m,n) = cordsPlayer a p
                           s     = (a,b,c,d,e,f)












timers :: Cords -> State -> Int
-- ^ Função que averigua quanto tempo falta para a próxima bomba que explode num determinado local explodir.
timers _ (_,_,_,_,[],_) = 100
timers x (a,b,c,d,e,f)  = minimum $ auxtimers e x

auxtimers :: [Bombs] -> Cords -> [Int]
-- ^ Auxiliar da função @timers@
auxtimers [] _        = [100]
auxtimers ((a,b):t) x = if elem x b then a:auxtimers t x else auxtimers t x






saferSpots :: [Cords] -> State -> Time -> Cords -> [Cords]
-- ^ Função que tem em conta quais espaços serão engolidos primeiro pela espiral.
saferSpots [] _ _ _     = []
saferSpots (h:hs) s t x = if timers h s >= timers x s                                                        then
                          if returnIndex ls h  > returnIndex ls x  || immaDie1 t s h == False                then h:saferSpots hs s t x 
                                                                                                             else   saferSpots hs s t x
                                                                                                             else   saferSpots hs s t x
                      where (a,b,c,d,e,f) = s
                            o  = (length (head a)) - 2
                            ls = (listaSpiral (1,1) o)




returnIndex :: (Eq a) => [a] -> a ->Int
-- ^ Função que devolve o índice de um elemento numa lista.
returnIndex [] _ = 10000
returnIndex (h:t) x = if h == x then 0 else 1 + returnIndex t x



worseTimerWay :: State -> Caminho -> Cords -> Bool
-- ^ Função que testa se um dado caminho leva a um maior risco para  jogador.
worseTimerWay _ "" _ = False 
worseTimerWay s (k:t) (m,n) | k == 'U'                    = if timers (m,n-1) s < timers (m,n) s then True else False
                            | k == 'D'                    = if timers (m,n+1) s < timers (m,n) s then True else False
                            | k == 'L'                    = if timers (m-1,n) s < timers (m,n) s then True else False
                            | k == 'R'                    = if timers (m+1,n) s < timers (m,n) s then True else False









unsafeSpiral :: Time -> Dim -> [Cords]
-- ^ Função que verifica os espaços a ser esmagados pela espiral nos próximos instantes.
unsafeSpiral t d = take 3 (drop o (listaSpiral (1,1) (d-2)))
                where o = (d-2)^2 - t








lastSquare :: State -> Cords
-- ^ Função que determina o último espaço do mapa e que um jogador pode estar antes de ser exmagado pela espiral.
lastSquare (a,b,c,d,e,f) = if even h then (h-1,h) else (h,h)
                        where h = div x 2
                              x = (length $ head a) 









----------------------------------------------------------------------FUGA DE BOMBAS--------------------------------------------------




immaDie :: State -> Cords -> Bool 
-- ^ Função que testa se uma dada posição está em risco de ser abrangida por uma explosão.
immaDie (a,b,c,d,[],f) _             = False
immaDie (a,b,c,d,((t,l):bs),f) (x,y) = if elem (x,y) l then True else immaDie (a,b,c,d,bs,f) (x,y)

immaDie1 :: Time -> State -> Cords -> Bool 
-- ^ Função que testa se uma dada posição está em risco de ser abrangida por uma explosão, quando o tempo está a acabar.
immaDie1 t (a,b,c,d,[],f) x = if elem x (take 3 (drop o (listaSpiral (1,1) (d-2)))) then True else False 
                          where o = (d-2)^2 - t
                                d = length $ head a
immaDie1 r (a,b,c,d,((t,l):bs),f) x = if elem x l then True else immaDie1 r (a,b,c,d,bs,f) x


safeSpots :: State -> [Cords] -> [Cords]
-- ^ Função que encontra numa lista de coordenadas um par de coordenadas que corresponda a um sítio que não esteja em risco de explosão.
safeSpots _ [] = []
safeSpots s l  = if immaDie s (last l) == False then (last l):safeSpots s (init l) else safeSpots s (init l)



chooseSpot :: State -> [Cords] -> Cords -> Cords
-- ^ Função que seleciona o espaço seguro mais próximo.
chooseSpot _ [] _    = (1,1)
chooseSpot _ [a] _   = a
chooseSpot s (h:t) x = if worseTimerWay s (findWay 1 s x h) x then chooseSpot s t x else h


chooseSpot1 :: Time -> State -> [Cords] -> Cords -> Cords
-- ^ Função que seleciona o espaço seguro mais próximo.
chooseSpot1 _ _ [] _  = (1,1)
chooseSpot1 _ _ [a] _ = a
chooseSpot1 t s (h:hs) x | elem x (unsafeSpiral t o)           =  if elem h (unsafeSpiral t o) then chooseSpot1 t s hs x else h
                         | worseTimerWay s (findWay 1 s x h) x =                                    chooseSpot1 t s hs x 
                         | otherwise                           =                                                              h
                     where (a,b,c,d,e,f) = s
                           o             = length $ head a






findSafeSpot :: Int -> State -> Cords -> Cords
-- ^ Função  que encontra as coordenadas seguras mais próximas de umas certas coordenadas.
findSafeSpot n (a,b,c,d,e,f) x = chooseSpot s (safeSpots s (rmDups (network n b x))) x
                            where s = (a,b,c,d,e,f)


findSafeSpot1 :: Time -> Int -> State -> Cords -> Cords
-- ^ Função  que encontra as coordenadas seguras mais próximas de umas certas coordenadas, quando o tempo está  a acabar.
findSafeSpot1 t n (a,b,c,d,e,f) x = chooseSpot1 t s (safeSpots s (rmDups (exc (unsafeSpiral t o)(network n b x)))) x
                               where s = (a,b,c,d,e,f)
                                     o = length $ head a



runAway :: Int -> State -> Cords -> Caminho
-- ^Função que elabora um caminho para fugir às zonas prestes a ser abrangidas por explosões.
runAway 11 _ _  = ""
runAway  n s x  | elem False (map (\h -> immaDie s h) (rmDups (network n b x)))                           =   findWay 1 s x (findSafeSpot n s x) 
                |otherwise                                                                                =   runAway (n+1) s x 
              where (a,b,c,d,e,f) = s


runAway1 :: Time -> Int -> State -> Cords -> Caminho
-- ^Função que elabora um caminho para fugir às zonas prestes a ser abrangidas por explosões, quando o tempo está a acabar.
runAway1 _ 11 _ _  = ""
runAway1 t n s x   | elem False (map (\h -> immaDie1 t s h) (saferSpots (rmDups (network n b x)) s t x))      =  findWay 1 s x (findSafeSpot1 t n s x) 
                   |otherwise                                                                                 =  runAway1 t (n+1) s x 
                 where (a,b,c,d,e,f) = s
                       o = length $ head a




----------------------------------------------------------------------------LOCOMOÇÃO-----------------------------------------------

findPoint :: [Cords] ->  Cords -> Cords -> Cords
-- ^ Encontra o ponto mais próximo de um destino numa certa rede. 
findPoint [] x y = y
findPoint (h:t) x y = if dist y x == minimum ls then y else 
                      if dist h x == minimum ls then h else findPoint t x y
                  where ls = map (\u -> dist u x) (h:t) 


dist :: Cords -> Cords -> Float 
-- ^ Distancia entre 2 espaços, dadas as suas coordenadas.
dist (a,b) (c,d) = sqrt (((fromIntegral a) - (fromIntegral c))^2 + ((fromIntegral b) - (fromIntegral d))^2)


findWay :: Int -> State -> Cords -> Cords -> Caminho
-- ^ Função que encontra um caminho entre umas dadas coordenadas e outras coordenadas.
findWay 15 _ _ _ = ""
findWay n (m,a,b,c,d,e) p x = if bla /= ""  then bla else findWay (n+1) (m,a,b,c,d,e) p x
                           where bla = mkDirections ( pTreeToCords (choosePath (prunePaths x (mkPathTree n p x (rmDups (network n a x ))))))



mkDirections :: [Cords] -> Caminho
-- ^ Função que converte uma lista de coordenadas numa lista de movimentos do jogador.
mkDirections [a] = ""
mkDirections ((x1,y1):(x2,y2):t) | x1 == x2                  = if y1 - y2 == 1 then 'U':rest else 'D':rest
                                 | otherwise                 = if x1 - x2 == 1 then 'L':rest else 'R':rest
                               where rest = (mkDirections ((x2,y2):t))





pTreeToCords :: RPath Cords -> [Cords]
-- ^ Função que converte uma árvore de caminhos (com apenas um ramo) numa lista de coordenadas.
pTreeToCords (C c [])  = [c]
pTreeToCords (C c [a]) = c:pTreeToCords a









mkPathTree :: Int -> Cords -> Cords -> [Cords] -> RPath Cords
-- ^ Função que cria uma árvore de caminhos numa rede que partem de um nodo.
mkPathTree _ _ _ [] = C (0,0) []
mkPathTree i (x1,y1) (x2,y2) net | elem (x2,y2) (neighbours (x1,y1) net)                     = C (x1,y1) [C (x2,y2) []]
                                 | neighbours (x1,y1) net == []                              = C (x1,y1) []
                                 | i <= 0                                                    = C (x1,x2) []
                                 | otherwise                                                 = C (x1,y1) (map (\h1 -> mkPathTree (i-1) h1 (x2,y2) (exc ((x1,y1):n) net)) n)
                               where n = (neighbours (x1,y1) net)





choosePath :: RPath Cords -> RPath Cords
-- ^ Fução que escolhe o caminho mais curto.
choosePath (C a [])      = C a []
choosePath (C a [x])     = C a [choosePath x]
choosePath (C a (h:t))   = if pathHeight h == minimum (map (\h -> pathHeight h) (h:t)) then C a [choosePath h] else choosePath (C a t)




pathHeight :: RPath Cords -> Int 
-- ^ Função que calcula o numero de nodos de um caminho.
pathHeight (C a [])  = 0
pathHeight (C a [x]) = 1 + pathHeight x
pathHeight (C a l)   = 1 + minimum (map (\h -> pathHeight h) l)





prunePaths :: Cords -> RPath Cords -> RPath Cords
-- ^ Função que retira de uma lista de caminhos os becos sem saída.
prunePaths c rp = if prunePaths1 c rp == rp then rp else prunePaths c (prunePaths1 c rp)


prunePaths1 :: Cords -> RPath Cords -> RPath Cords
-- ^ Função que retira de uma lista de caminhos os becos sem saída.
prunePaths1 c (C x []) = C x []
prunePaths1 c (C x ((C y l):t)) | c == y               =               C x [C y []]
                                | l == []              = prunePaths1 c (C x t)
                                | otherwise            =   concatPTree (C x [prunePaths1 c (C y l)]) (prunePaths1 c (C x t))    












concatPTree :: RPath a -> RPath a -> RPath a 
-- ^ Função que junta duas árvores de caminhos.
concatPTree (C x l1) (C y l2) = C x (l1 ++ l2)









exc :: Eq a => [a] -> [a] -> [a]
-- ^ Função que excui os elementos de uma lista de outra.
exc [] l = l
exc (h:t) l = if elem h l then exc t (delete h l) else exc t l 




neighbours :: Cords -> [Cords] -> [Cords]
neighbours _ [] = []
-- ^ Função que encontra os vizinhos deum espaço numa rede.
neighbours (x,y) ((a,b):t)  = if elem (x-a) [-1,0,1]  && elem (y-b) [-1,0,1]   && elem ((x-a) + (y-b) ) [-2,0,2] == False then (a,b):neighbours (x,y) t
                                                                                                                          else       neighbours (x,y) t




network :: Int -> [Cords] -> Cords -> [Cords]
-- ^ Função que cria uma rede de espaços ligados entre si a partir de uma lista de epaços. Note-se que o @Int@ que esta função leva como argumento serve para limitar os cálculos a efetuar, dado que estes crescem a um ritmo exponencial, mediante a quantidade de nodos de uma rede.
network n b x = network1 n x b []


network1 :: Int -> Cords -> [Cords] -> [Cords] -> [Cords]
-- ^ Função auxiliar da @network@
network1 _ _ [] net = rmDups net
network1 n x (h:t) net | n == 0                                                     = rmDups net
                       | isNode h [x]                                               = (network1 (n-1) h t [] ++ network1 n x t (h:net))
                       | elem True (map (\i -> isNode i [x]) (h:t)) == False        = rmDups net
                       | otherwise                                                  = (network1 n x (t++[h]) net)




isNode :: Cords -> [Cords] -> Bool
-- ^ Função que testa se um par de coordenadas está numa rede de espaços ligados entre si.
isNode _ [] = False
isNode (x,y) ((a,b):t) | elem (x-a) [-1,0,1]  && elem (y-b) [-1,0,1]   && (x==a) /= (y==b)                                    = True
                       | otherwise                                                                                            = isNode (x,y) t














testWay :: Mapa -> Jogador -> Cords -> Caminho -> Bool
-- ^ Função que testa se uma string com instruções para um caminho a ser seguido por um jogador o levam a determnadas coordenadas.
testWay mapa p (x,y) st = if cordsPlayer (walkWay mapa p st) p == (x,y) then True else False


walkWay :: Mapa -> Jogador -> Caminho -> Mapa
-- ^ Função que testa um caminho utilizando a função move da Tarefa2.
walkWay mapa _ [] = mapa 
walkWay mapa p (h:t) = walkWay (move mapa p h) p t










---------------------------------------CONSTRUÇÃO DO ESTADO------------------------------------------------------------------------



mkState :: Mapa -> State
-- ^ Função que a partir de uma lista de strings cria uma estrutura de dados representativa do estado do jogo mais conveniente para trabalhar.                
mkState mapa = (                     mapa,
                 takeCordsChar ' ' 0 mapa,
                 takeCordsChar '?' 0 mapa,
                 retiraPowers        mapa,
                 takeBombs     mapa  mapa,
                 takePlayers   mapa  mapa   )








takeCordsChar :: Char -> Ordenada -> Mapa -> [Cords]
-- ^ Função que retira do mapa elementos representados pelo @Char x@ em forma de túpulo.
takeCordsChar x l (h:t) = if l + 1 == length h then [] else (takeCordsCharRow x l 0 h)        ++ takeCordsChar x (l+1) t



takeCordsCharRow :: Char -> Ordenada -> Abcissa -> Linha -> [Cords]
-- ^ Função que retira de cada linha do mapa um dado elemento representado por dado @Char@, em forma de túpulo.
takeCordsCharRow _ _ _ []    = []
takeCordsCharRow x l c (h:t) = if    h ==  x         then (c,l):takeCordsCharRow x l (c+1) t
                                                     else       takeCordsCharRow x l (c+1) t







retiraPowers :: Mapa -> [Powers]
-- ^ Função que a partir do estado do jogo obtém informação dos powerups.
retiraPowers [] = []
retiraPowers (h:t) = if elem (h!!0) "+!" then (tuplePowers h):retiraPowers t else retiraPowers t

tuplePowers :: Linha -> Powers
-- ^ Função que converte num túpulo uma string com informação de dum dado powerup.
tuplePowers string = (    head string,
                     (  pAbcis string,
                        pOrden string  )
                                       )








takeBombs :: Mapa -> Mapa -> [Bombs]
-- ^ Função que retira do mapa informação acerca das bombas.
takeBombs _ [] = []
takeBombs mapa (h:t) = if head h == '*' then (read [last h], cordFlames2 mapa h):takeBombs mapa t else  takeBombs mapa t








cordFlames2 :: Mapa -> Linha -> [Cords]
-- ^ Função que cria uma lista de coordenadas em que são propagadas as chamas.
cordFlames2 mapa string = rmDups ( (cordFlamesUp2 mapa x y r) ++ (cordFlamesDown2 mapa x y r) ++ (cordFlamesLeft2 mapa x y r) ++ (cordFlamesRight2 mapa x y r) ++ [(x,y)] )
                       where x = pAbcis1 string
                             y = pOrden1 string
                             r = range  string


cordFlamesUp2 :: Mapa -> Range -> Abcissa -> Ordenada -> [Cords]
cordFlamesUp2 mapa x y n     | ((mapa !! y) !! x) == '#'            =                                 []
                             | ((mapa !! y) !! x) == '?'            =                        [(x,y)]
                             | isTherePower  mapa x y               =                        [(x,y)]
                             | n == 1                               =                        [(x,y)]
                             | otherwise                            =                         (x,y) :cordFlamesUp2 mapa x (y-1) (n-1) 


cordFlamesDown2 :: Mapa -> Range -> Abcissa -> Ordenada -> [Cords]
cordFlamesDown2 mapa x y n   | ((mapa !! y) !! x) == '#'            =                                 []
                             | ((mapa !! y) !! x) == '?'            =                        [(x,y)]
                             | isTherePower  mapa x y               =                        [(x,y)]
                             | n == 1                               =                        [(x,y)]
                             | otherwise                            =                         (x,y) :cordFlamesDown2 mapa x (y+1) (n-1)


cordFlamesLeft2 :: Mapa -> Range -> Abcissa -> Ordenada -> [Cords]
cordFlamesLeft2 mapa x y n   | ((mapa !! y) !! x) == '#'            =                                 []
                             | ((mapa !! y) !! x) == '?'            =                        [(x,y)]
                             | isTherePower  mapa x y               =                        [(x,y)]
                             | n == 1                               =                        [(x,y)]
                             | otherwise                            =                         (x,y) :cordFlamesLeft2 mapa (x-1) y (n-1)


cordFlamesRight2 :: Mapa -> Range -> Abcissa -> Ordenada -> [Cords]
cordFlamesRight2 mapa x y n  | ((mapa !! y) !! x) == '#'            =                                 []
                             | ((mapa !! y) !! x) == '?'            =                        [(x,y)]
                             | isTherePower  mapa x y               =                        [(x,y)]
                             | n == 1                               =                        [(x,y)]
                             | otherwise                            =                         (x,y) :cordFlamesRight2 mapa (x+1) y (n-1)






takePlayers :: Mapa -> Mapa -> [Players]
-- ^ Função que obtem informação relativa aos jogadores em jogo.
takePlayers _ [] = []
takePlayers mapa (h:t) = if isDigit (head h) then (cordsPlayer mapa (read [head h]) , 1 + howMany '+' h, 1 +  howMany '!' h):takePlayers mapa t   
                                             else                                                                            takePlayers mapa t   





cordsPlayer :: Mapa -> Jogador -> Cords
-- ^ Função que a partir de um estado e de um numero de jogador obtém um túpulo com as coordenadas do jogador.
cordsPlayer [] _ = (1,1)
cordsPlayer (h:t) n = if head h == head (show n)  then (pAbcis h, pOrden h) 
                                                  else cordsPlayer t n



howMany :: Char -> Linha -> Int
-- ^ Função que conta o número de vezes que um certo caracter ocorre numa string, com utilididade, por exemplo, no caso de saber quantos powerups de determinado tipo tem um jogador.
howMany _ [] = 0
howMany c (h:t) = if  h == c      then 1 + howMany c t
                                  else     howMany c t







rmDups :: Eq a => [a] -> [a]
-- ^ Função que remove elementos repetidos numa lista.
rmDups []  = []
rmDups [h] = [h]
rmDups (h:t) = if elem h t then rmDups t else h:rmDups t



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



