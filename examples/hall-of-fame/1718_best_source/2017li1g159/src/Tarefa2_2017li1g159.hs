{-|
Module : Tarefa 2
Description : Tarefa 2 Trabalho prático.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

Este módolo contêm a solução da Tarefa 2 do trabalho prático de Laboratórios de Informática 1.
-}

module Tarefa2_2017li1g159 where
    
import LI11718

{-| 
         O nome é a composição das palavras Bidimensional list (lista bidimensional). 

         Este tipo de dados irá representar matrizes de forma semelhante à que é representada no C.
         Onde basicamente não interessa ão computador se é uma matriz ou lista. O compilador é
         que tem de tratar de saber onde na lista está posicionado o elemente do matriz, neste caso este processo de conversão será feito pelos programadores.
-}

data Bidimlist = Bid Dimensao [Peca] 

{-|
      Estado é o nome de um tipo que vai representar uma posição altura e orientação. 
-}
type Estado = (Posicao, Orientacao,Altura)

testesT2 :: [Tabuleiro]
testesT2 = [ []
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 2,Peca Recta 2,Peca (Rampa Oeste) 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 2,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Recta 8,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca (Curva Este) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 3,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0 ,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Este) 0,Peca Lava 0]]
            ,[[Peca (Rampa Este) 1,Peca Lava 0,Peca (Rampa Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Oeste) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca (Curva Este) 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 1,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) 1,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 1,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) 1,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Rampa Este) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1, Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0], [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0], [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Oeste) (-1), Peca (Rampa Oeste) (-2), Peca (Rampa Este) (-2), Peca (Rampa Oeste) (-1), Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca (Rampa Oeste) (-1), Peca (Rampa Oeste) (-2), Peca (Rampa Este) (-2), Peca (Rampa Oeste) (-1), Peca (Curva Sul) 0, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]  
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0]]
            ,[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
            ]    

      {- as ultimas 2 entradas
            -}
{-|
      valida é a função que recebendo um mapa verifica que este é valido ou não.
-}
valida :: Mapa -> Bool
valida (Mapa (_ , _    ) []) = False
valida (Mapa (v , teta ) t ) = molduraDeLava t  && mapRec (reshape t) && vBelongTodim v (reshape t) && repository (reshape t) arg
    where arg = stepbystep (v , teta , zStart ) (v , teta , zStart ) (reshape t) (-1)
                where zStart = findStart v (reshape t)
                      findStart :: Posicao -> Bidimlist -> Int
                      findStart v l = final
                            where (Peca _ final) = retStone l v
{-|
      Esta função verfica se o mapa é rodeado por lava, ou seja, a primeira e última linha,assim como a primeira e última coluna são constituídas por peças necessariamente do tipo lava.
-}
molduraDeLava :: Tabuleiro -> Bool
molduraDeLava t = f (head t)  &&  f (last t) && g t
                  where f = foldr (\ h r -> peek h && r ) True
                        g = foldr (\ h r -> peek (head h)  && peek (last h) && r ) True
{-|
      Recebe uma Peca e indica se é lava
-}
peek :: Peca -> Bool
peek (Peca a _ ) = a == Lava

{-|   
      Esta função verifica se o tabuleiro é rectangular -}
mapRec :: Bidimlist -> Bool
mapRec (Bid (x,y) l) = x*y == length l 

{-|
      Recebe uma 'Bidimlist' e um tuplo que contêm no primeiro componente um Bool  e no segundo um inteiro. 
      Se o boleano for False devolve False se o boleano for True conta o numero de peças
      que não são Lava na 'Bidimlist' e se não for igual ao inteiro na segunda componente devolve False se for True.

      == Exemplos de utilização:
      Seja a = Bid (3,3) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> repository a (False,3)
      False
      
      >>> repository a (True,3 )
      True
      
      >>> repository a (True,5 )
      False
      
-}
repository :: Bidimlist -> (Bool, Int) -> Bool
repository (Bid _ _) (False , _  ) = False -- cnt == foldr (\ a r -> if not (peek a) then 1 + r else r ) 0 l
repository (Bid _ l) (True , cnt ) = cnt == f l
      where f = foldr (\ a r -> if not (peek a) then 1 + r else r ) 0

{-|
      Recebe 2 estados e vai recursivamente alterando o segundo em função das peças do tabuleiro nesse estado até que ou encontre uma peça invalida 
      ou então volte ao inicio. Devolverá ( Bool, Int ) o Bool será True se há um caminho válido e falso se não há. No caso em devolve true o Inteiro conterá o numero de 
      peças que foram percurridas.

-}
stepbystep :: Estado -> Estado -> Bidimlist -> Int -> (Bool, Int)
stepbystep _ (v, _ , _) l _                | not (vBelongTodim v l) || isLava l v                        = ( False, 0 )
stepbystep start current l n               | n /= -1 && start == current                                 = ( True,  0 )
                                           | notvalid current l                                          = ( False, 0 )
stepbystep start (v, teta , z) l n                                                                       = (a ,1+b)
                where (a,b) = stepbystep start ( validNextStep (v, teta , z) (retStone l v) ) l 0
{-|
      Recebe uma 'Bidimlist' e uma 'Posicao' e acede à 'Peca' da Bidimentional list que corresponde a essa posição.   

      == Exemplos de utilização:
      Seja a = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> isLava a (0,0)
      False

      >>> isLava a (1,0)
      True

      >>> isLava a (2,0)
      True

-}
isLava :: Bidimlist -> Posicao -> Bool 
isLava l v  =  peek (retStone l v )
{-|
      Esta função Recebe uma Bidemsional list e uma posição e verifica se essa posição pode pertencer à Bidimensional lista.
            

      == Exemplos de utilização:
      Seja t uma qualquer lista de 'Peca'.

      >>> vBelongTodim (5,5) (Bid (3,3) t )
      False

      >>> vBelongTodim (2,2) (Bid (3,3) t )
      True

      >>> vBelongTodim (14,1) (Bid (3,3) t )
      False

-}

vBelongTodim :: Posicao -> Bidimlist -> Bool
vBelongTodim (x,y) (Bid (dimx,dimy) _ ) | x<dimx && y<dimy && x>(-1) && y>(-1) = True
                                        | otherwise                            = False            
{-|
      Recebe um Estado que corresponde a uma peça que não é lava e uma 'Bidimlist' e diz se a peça é compatível com estado atual.       

      == Exemplos de utilização:

      >>> notvalid ( (0,0) , Norte , 0) a 
      False

      >>> notvalid ( (0,0) , Sul , 0) a
      True

-}
notvalid :: Estado -> Bidimlist -> Bool
notvalid (v,teta,z) l = not (vBelongTodim (move v teta) l) || policy ( inv180 teta ,z) (retStone l v )  
            where policy :: (Orientacao, Altura) -> Peca -> Bool
                  policy (vindode,z) (Peca x b ) | x == Recta                                                  = z /= b
                                                 | x == Rampa vindode                                          = (z-1) /= b 
                                                 | x == Rampa (inv180 vindode)                                 = z /=b
                                                 | z /= b                                                      = True
                                                 | x == Curva Este  && (vindode == Oeste || vindode == Sul )   = False
                                                 | x == Curva Norte && (vindode == Sul   || vindode == Este )  = False
                                                 | x == Curva Oeste && (vindode == Norte || vindode == Este )  = False
                                                 | x == Curva Sul   && (vindode == Norte || vindode == Oeste ) = False
                                                 | otherwise                                                   = True

{-|
      Recebe uma 'Bidimlist' e uma 'Posicao' e devolve a 'Peca' que corresponde a essa posição. 

      == Exemplos de utilização:
      Seja t = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> retStone t (0,0)
      Peca (Curva Este) 0

      >>> retStone a (0,1)
      Peca (Rampa Sul) 0

-}
retStone :: Bidimlist -> Posicao -> Peca 
retStone (Bid dim l) v =  l !! cordToBidC dim v 
            where cordToBidC :: Dimensao-> Posicao -> Int    -- Traduz uma coordenada num indice da minha Bidementional list
                  cordToBidC (dimx,dimy) (x,y) | x<dimx && y<dimy = y * dimx + x


{-|
      Supõe-se que a Peca é compativel com o estado. 

      == Exemplos de utilização:

      >>> validNextStep ( (4,1) , Norte , 0) (Peca (Curva Este) 1)
      ((3,1),Oeste,0)

      >>> validNextStep ( (3,4) , Oeste , 4 ) (Peca (Rampa Este) 1  )
      ((2,4),Oeste,3)

      >>> validNextStep ( (2,1) , Este , -1) (Peca (Curva Sul) 3 )
      ((2,0),Norte,-1)

-}                  
validNextStep :: Estado -> Peca -> Estado
validNextStep ( v, teta , z ) (Peca (Rampa x) _ ) | teta == inv180 x   = ( move v teta , teta , z-1 )
                                                  | teta == x          = ( move v teta , teta , z+1 )
validNextStep ( v, teta , z ) (Peca Recta  _    )                      = ( move v teta , teta ,  z  ) 
validNextStep ( v, Norte , z) (Peca (Curva x) _)  | x == Norte         = ( move v Este , Este ,  z )
                                                  | x == Este          = ( move v Oeste , Oeste , z )
validNextStep ( v, Este , z) (Peca (Curva x) _ )  | x == Sul           = ( move v Norte , Norte , z  )
                                                  | x == Este          = ( move v  Sul, Sul ,  z   )
validNextStep ( v, Sul  ,  z) (Peca (Curva x) _ ) | x == Oeste         = ( move v Este , Este ,  z   )
                                                  | x == Sul           = ( move v Oeste , Oeste , z   )
validNextStep ( v, Oeste , z) (Peca (Curva x) _ ) | x == Norte         = ( move v Sul , Sul ,  z  )
                                                  | x == Oeste         = ( move v Norte , Norte , z )  
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
      Recebe uma 'Orientacaoo' e devolve a Orientação oposta. 

      ==Exemplos de utilização:

      >>> inv180 Este
      Oeste

      >>> inv180 Sul
      Norte

-}
inv180 :: Orientacao -> Orientacao
inv180 Norte = Sul
inv180 Sul   = Norte
inv180 Este  = Oeste
inv180 Oeste = Este

{-|
      Recebe um 'Tabuleiro' e transforma-o numa lista Bidimensional.

      ==Exemplos de utilização:

      >>> reshape [[Peca (Curva Este) 0,Peca Lava 0],[Peca (Curva Este) 0,Peca Lava 0]]
      Bid (2,2) [Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Este) 0,Peca Lava 0]

      >>> reshape [[Peca (Curva Este) 0,Peca Lava 0],[Peca (Curva Este) 0,Peca Lava 0],[Peca Recta 0, Peca Recta 1 ]]
      Bid (2,3) [Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Este) 0,Peca Lava 0,Peca Recta 0,Peca Recta 1]

-}
reshape :: Tabuleiro -> Bidimlist 
reshape (h:t) = Bid ( length h , length t + 1 ) (concat (h:t) )
