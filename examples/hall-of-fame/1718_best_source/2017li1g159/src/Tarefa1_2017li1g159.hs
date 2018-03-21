{-|
Module : Tarefa 1
Description : Tarefa 1 Trabalho prático.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

Este módolo contêm a solução da Tarefa 1 do trabalho prático de Laboratórios de Informática 1.
-}

module Tarefa1_2017li1g159 where

import LI11718


{-| 
         O nome é a composição das palavras Bidimensional list (lista bidimensional). 

         Este tipo de dados irá representar matrizes de forma semelhante à que é representada no C.
         Onde basicamente não interessa ão computador se é uma matriz ou lista. O compilador éE
         que tem de tratar de saber onde na lista está posicionado o elemente do matriz, neste caso este processo de conversão será feito pelos programadores.
-}

data Bidimlist = Bid Dimensao [Peca]
------------------------------------------------------------------------------------------

{-|
        Esta é essencialmente a função mais importante do programa. Pois condensa a informação valiosa do 'Caminho' numa lista de de Tuplos 
        em que a primeira componente é o indice na Bidimentional list que a segunda componente ,uma 'Peca', vai ocupar. 
        Esta função completa esta tarefa usando as funções, cordToBidC, findkey, novaAltura, novaOrientacao e move.
        
        
        == Exemplos de utilização:
        >>> extrai (5,6) 0 Este [Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq,Sobe,Sobe] (1,3)
        [
        (16,Peca Recta 0),
        (17,Peca Recta 0),
        (18,Peca (Curva Sul) 0),
        (13,Peca Recta 0),
        (8,Peca (Curva Este) 0),
        (7,Peca Recta 0),
        (6,Peca (Curva Norte) 0),
        (11,Peca (Rampa Sul) 0),
        (16,Peca (Rampa Sul) 1)
        ]

        >>> extrai (5,5) 0 Este [Avanca,Avanca,CurvaDir, Avanca, CurvaDir ,Avanca] (1,1)
        [
        (6,Peca Recta 0),
        (7,Peca Recta 0),
        (8,Peca (Curva Este) 0),
        (13,Peca Recta 0),
        (18,Peca (Curva Sul) 0),
        (17,Peca Recta 0)
        ]
       
        
-}
extrai ::Dimensao -> Altura -> Orientacao -> Caminho -> Posicao ->[ ( Int , Peca ) ]
extrai  _ _ _ [] _                                  = []
extrai (0,0) _ _ _ _                                = []
extrai dimT z oRi (h:t) v                           = ( cordToBidC dimT v , findkey z h oRi  ): extTail   
                where extTail = extrai dimT ( novaAltura z h) ( novaOrientacao oRi h ) t (move v ( novaOrientacao oRi h ) )

{-|
        Foram escolhidos para testes os exemplos que mais foram problemáticos durante o processo de construção do programa.  
-}
testesT1 :: [Caminho]
testesT1 = [
            []
           ,[Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,CurvaEsq,Sobe,Sobe]
           ,[Avanca, CurvaDir, Avanca, Sobe, Sobe, CurvaDir, Avanca, Avanca, CurvaDir ,Avanca, CurvaDir, Avanca , Desce, Desce , Avanca]
           ,[Sobe, Avanca,  Sobe, CurvaDir, Sobe, Avanca, Desce]
           ,[Avanca, Avanca, CurvaDir, Avanca , CurvaDir, Avanca, CurvaDir, Avanca, Avanca, Avanca, CurvaDir , Avanca, CurvaDir, Avanca]
           ,[Avanca,Avanca,CurvaDir, Avanca, CurvaDir ,Avanca]
           ,[Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,CurvaDir]
           ,[Avanca,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,CurvaDir]
           ,[Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaDir]
           ,[Avanca,Sobe,Avanca,Avanca,Avanca,Avanca,Avanca,Desce,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Sobe,Avanca,Avanca,Avanca,Desce,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Sobe,Avanca,Desce,CurvaDir]
           ,[CurvaDir ,CurvaDir,CurvaDir,CurvaDir]
           ]
{-|
        Completar esta função é o objetivo da Tarefa1 . Esta função tem como funções auxiliares a função: bidToMap , solveByList, dimensao e partida. 
-}
constroi :: Caminho -> Mapa
constroi  cam =  bidToMap ( solveByList (dimensao cam) cam ) (partida cam)

{-|
    Esta função serve para ordenar a lista de tuplos dada pela função extrai, com a função qsort, e também usar a função genBiDimList para criar uma 'Bidimlist'
    com o resultado da função qsort.  
    
    == Exemplos de utilização:
        >>> solveByList (6,7) [Sobe, Avanca,  Sobe, CurvaDir, Sobe, Avanca, Desce]
        Bid (6,7) [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Lava 0,Peca Lava 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,
        Peca (Curva Este) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca (Rampa Sul) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Recta 3,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca (Rampa Norte) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]

        >>> solveByList (4,4) [Avanca, CurvaDir]
        Bid (4,4) [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,
        Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]

-}
solveByList :: Dimensao -> Caminho -> Bidimlist
solveByList sz cam = genBiDimList sz arg 
        where arg  = qsort ( extrai sz 0 Este cam (partida cam) ) 

{-|
        Recebendo um tuplo com a primeira componente um indice e a segunda uma peca cria uma lista bidimensional 
        com cada 'Peca' no seu respetivo indice. Os indices que não estão defenidos terão a 'Peca' lava.

        == Exemplos de utilização:
        >>> genBiDimList (5,5) [(6,Peca Recta 0),(7,Peca Recta 0),(8,Peca (Curva Este) 0),(13,Peca Recta 0),(18,Peca (Curva Sul) 0),(17,Peca Recta 0)]
        Bid (5,5) [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,
        Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Sul) 0,
        Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
-}        
genBiDimList :: Dimensao -> [(Int, Peca)] -> Bidimlist 
genBiDimList (x,y) c = Bid (x,y) (fLOrOP 0 (x*y) (keepfstRep c ))     
        -- code name para for loop
        where fLOrOP :: Int -> Int -> [(Int, Peca)]->[Peca]
              fLOrOP steps top  _    | steps >= top      = [] 
              fLOrOP steps top []                        = Peca Lava 0:fLOrOP (steps+1) top [] 
              fLOrOP steps top (h:t) | steps == fst h  =       snd h :fLOrOP (steps+1) top t
                                     | otherwise         = Peca Lava 0:fLOrOP (steps+1) top (h:t)  

{-|
        Recebe uma lista de tuplos ordenados. Se existirem varios elementos seguidos cuja primeira componete é igual apaga todos menos o ultimo.
        Isto porque o ultimo corresponderá à ultima peça colocada.(Dado que está implentação do quicksort é estável ) 

        == Exemplos de utilização:
        >>> keepfstRep [(1, 'a') , (2, 'b'),(2, 'c'), (3,'k')]
        [(1, 'a') , (2, 'c'), (3,'k')]

        >>> keepfstRep [(1, "stringy") , (2, "stringy1"),(2, "stringy"), (3,"stringy"), (3,"stringy2")]
        [(1,"stringy"),(2,"stringy"),(3,"stringy2")]
-}
keepfstRep :: [(Int, a)] -> [(Int,a)]
keepfstRep []      = []
keepfstRep [h]     = [h]
keepfstRep (h:c:t) = if fst h == fst c then c:keepfstRep t else h:keepfstRep (c:t) 

{-|
        Chama a função shape ao seu argumento de forma a com o tabuleira resultante criar um map.

        == Exemplos de utilização:
        >>> bidToMap Bid (2,2) [(0,Peca Recta 0),(1,Peca Recta 0),(2,Peca (Curva Este) 0),(3,Peca Recta 0)] (1,1) 
        Mapa ((1,1),Este) [[(0,Peca Recta 0),(1,Peca Recta 0)],[(2,Peca (Curva Este) 0),(3,Peca Recta 0)] ]
-}
bidToMap :: Bidimlist -> Posicao -> Mapa
bidToMap mat pos= Mapa (pos,Este) (shape mat) 

{-|
        Recebe um 'Bidimlist' que contêm uma dimensão (x,y) e uma lista de todas as x*y 'Peca'. Cria um 'Tabuleiro'(lista de listas)
        com a dimensão (x,y). 

        == Exemplos de utilização:
        >>> shape (Bid (2,2) [(Peca Recta 0),(Peca Recta 0),(Peca (Curva Este) 0),(Peca Recta 0)] )
        [[Peca Recta 0,Peca Recta 0],[Peca (Curva Este) 0,Peca Recta 0]]
-}
shape :: Bidimlist -> Tabuleiro
shape (Bid _ [])     = []
shape (Bid (x,y) c )  =   take x c : shape arg 
    where arg = Bid (x,y-1) (drop x c)
                             -- LISTA DE LISTA COM dimy elementos.
                             -- Cada elemente tem dimx elementos.

{-|
        Recebe uma 'Posicao' que representa uma coordenada num tabuleira e devolve o indice na 'Bidimlist' a que corresponde essa coordenada.

        == Exemplos de utilização:
        >>> cordToBidC (7,3) (5,2)
        19

        >>> cordToBidC (4,2) (1,1)
        5

-}
cordToBidC :: Dimensao-> Posicao -> Int    -- Traduz uma coordenada num indice da minha Bidementional list
cordToBidC (dimx,dimy) (x,y) | x<dimx && y<dimy = y * dimx + x
                             
{-|
         Recebe uma ‘Altura’, um ‘Passo’ e uma ‘Orientacao’  e determina qual é a ‘Peca’ que permite executar esse 'Passo'.

         == Exemplos de utilização:
         >>> findkey 1 Sobe Norte
         Peca (Rampa Norte) 1

         >>> findkey 4 Desce Oeste
         Peca (Rampa Este) 3

-}
findkey:: Altura -> Passo -> Orientacao -> Peca
findkey z Avanca orint                                      = Peca Recta z
findkey z Desce orint                                       = Peca (Rampa (inv180 orint) ) (z-1)                    
findkey z Sobe orint                                        = Peca (Rampa orint) z 
findkey z CurvaEsq orint                                    = forCurEsq z orint
findkey z CurvaDir orint                                    = forCurDir z orint

{-|
      Recebe uma 'Orientacaoo' e devolve a Orientação oposta. 

      ==Exemplos de utilização:

      >>> inv180 Este
      Oeste

      >>> inv180 Sul
      Norte

-}
inv180 :: Orientacao -> Orientacao
inv180 Norte = Sul
inv180 Sul = Norte
inv180 Este = Oeste
inv180 Oeste = Este

{-|
        Encontra a peça que permite fazer a Curva para a Esquerda tendo em conta a Altura e Orientação.

        ==Exemplos de utilização:
        >>> forCurEsq 2 Norte
        Peca (Curva Este) 2

        >>> forCurEsq 2 Este
        Peca (Curva Sul) 2
-}                      
forCurEsq :: Altura -> Orientacao -> Peca
forCurEsq z Norte = Peca (Curva Este) z
forCurEsq z Sul   = Peca (Curva Oeste) z
forCurEsq z Este  = Peca (Curva Sul) z
forCurEsq z Oeste = Peca (Curva Norte) z

{-|
        Encontra a peça que permite fazer a Curva para a Direita tendo em conta a Altura e Orientação.

        ==Exemplos de utilização:
        >>> forCurDir 2 Norte
        Peca (Curva Norte) 2

        >>> forCurDir 2 Este
        Peca (Curva Este) 2
-}
forCurDir :: Altura -> Orientacao -> Peca
forCurDir z x = Peca (Curva x) z

{-|
        Esta função recebe uma determinada ‘Orientacao’ e um ‘Passo’ a tomar, e determina a próxima ‘Orientação’.

        ==Exemplos de utilização:
        >>> novaOrientacao Norte Sobe
        Norte

        >>> novaOrientacao Oeste CurvaEsq
        Sul
-}
novaOrientacao :: Orientacao -> Passo -> Orientacao
novaOrientacao u v | v == Sobe || v == Desce || v == Avanca = u
novaOrientacao Norte CurvaDir                               = Este
novaOrientacao Norte CurvaEsq                               = Oeste
novaOrientacao Este CurvaDir                                = Sul
novaOrientacao Este CurvaEsq                                = Norte
novaOrientacao Sul CurvaDir                                 = Oeste
novaOrientacao Sul CurvaEsq                                 = Este
novaOrientacao Oeste CurvaDir                               = Norte
novaOrientacao Oeste CurvaEsq                               = Sul
{-|
        A função novaAltura recebe a 'Altura' de num estado e um 'Passo' 
        e Calcula a 'Altura' final após executar o passo. 

        ==Exemplos de utilização:
        >>> novaAltura 4 Sobe
        5

        >>> novaAltura 7 CurvaEsq
        7
-}
novaAltura :: Altura -> Passo -> Altura
novaAltura n p | p == Avanca || p == CurvaEsq || p == CurvaDir = n 
novaAltura n Sobe                                              = n + 1
novaAltura n Desce                                             = n - 1
{-|
        O move recebe uma coordenada e uma ‘Orientacao’ , que irá determinar as coordenas da posição seguinte.

        ==Exemplos de utilização:
        >>> move (4,1) Este
        (5,1)

        >>> move (3,2) Sul
        (3,3)

-}
move :: Posicao -> Orientacao -> Posicao
move (x,y) Norte = ( x  , y-1 )
move (x,y) Sul   = ( x  , y+1 )
move (x,y) Este  = ( x+1, y   )
move (x,y) Oeste = ( x-1, y   )

{-|
        O “quick sort” é um algoritmo de ordenação foi escolhido pois é facil de implementar.
        Foi implementado desta maneira especifica, pois desta form ele é estável. Ou seja, preserva a ordem de registo dos elementos iguais. 
        Esta parte final é fundamental para o bom funcionamento de todo o programa,pois esta é a forma de prevenir erros em 'Caminhos' que geram 'Pecas' que ficam "sobrepostas". 
        Sem esta propriedade o programa não funcionava para casos em que são introduzidos "cruzamentos".
        *O Partition é uma subrotina desta função*

        ==Exemplos de utilização:
        >>> qsort [(7,'o'),(4,'a'),(2,'o'),(9,'o'),(7,'l'),(4,'c'),(3,'n'),(2,'g')]
        [(2,'g'),(2,'o'),(3,'n'),(4,'c'),(4,'a'),(7,'l'),(7,'o'),(9,'o')]

        >>> qsort [ (5,'t'), (3,'o') , (1 , 'p') , (9, 't'),(15 , 'o'), (9, 'a')]
        [(1,'p'),(3,'o'),(5,'t'),(9,'a'),(9,'t'),(15,'o')]
-}
qsort :: [(Int , a)] -> [(Int , a)]
qsort []     = []
qsort [h]    = [h]
qsort (h:t)  =  qsort l ++ h:m ++ qsort r 
    where (l,m,r) =partition (fst h ) t

{-|
        Esta função faz a partição dos elementos de um array em função do primeiro elemento deste.  Sobre esta função foi usada uma técnica de otimização designada tupling. 
        
        ==Exemplos de utilização:
        >>> partition 4 [(2,'g'),(2,'o'),(3,'n'),(4,'c'),(4,'a'),(7,'l'),(7,'o'),(9,'o')]
        ([(2,'g'),(2,'o'),(3,'n')],[(4,'c'),(4,'a')],[(7,'l'),(7,'o'),(9,'o')])

        >>> partition 3 [ (5,'t'), (3,'o') , (1 , 'p') , (9, 't'),(15 , 'o'), (9, 'a')]
        ([(1,'p')],[(3,'o')],[(5,'t'),(9,'t'),(15,'o'),(9,'a')])
-}
partition ::  Int -> [(Int , a)] -> ( [(Int , a)], [(Int , a)] , [(Int , a)] )
partition _ []                         = ([],[],[]) 
partition h (x:xs) | fst x <  h        = (x:l, m , r )
                   | fst x ==  h       = (l  , x:m, r )
                   | otherwise         = (l  , m ,x:r)
           where (l,m,r) = partition h xs 

