{-|
Module : Direction
Description : Este módulo serve para auxiliar a Tarefa6.
Copyright : Gonçalo Faria <gonca2372@gmail.com> & Gonçalo Pereiera <goncalosantiago99@gmail.com>;

O objetivo deste módulo é o de com a função 'vecFile' recebendo um mapa devolver um caminho que
se inicia no inicio do mapa e terminha no fim deste.

Esse caminho será representado como um conjunto de posições de peças desse mapa. 
-}
module Direction where
    
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

vecFile ::  Mapa ->[(( Int,Int), Orientacao) ]
vecFile (Mapa (v,o) t) = stepbystep (v,o, get_h v l ) (v,o, get_h v l ) l (-1) 
            where l = reshape t
                  (Bid ans _ ) = l
{-|
      Esta função recebe a coordenada de uma peça no tabuleiro e devolve a altura desta. 
-}
get_h :: Posicao -> Bidimlist -> Altura
get_h v l = z
            where  ( Peca _ z) = retStone l v

{-|
      Recebe 2 estados e vai recursivamente alterando o segundo em função das peças do tabuleiro nesse estado até que ou encontre uma peça invalida 
      ou então volte ao inicio. Devolverá ( Bool, Int ) o Bool será True se há um caminho válido e falso se não há. No caso em devolve true o Inteiro conterá o numero de 
      peças que foram percurridas.

-}
stepbystep :: Estado -> Estado -> Bidimlist -> Int -> [(( Int,Int), Orientacao) ]
stepbystep start current l n               | n /= -1 && start == current   = []
stepbystep start (v, teta , z) l _         | isCurv (retStone l v)         = ( v ,  nteta) :r
                                           | otherwise                     = ( v ,  nteta) : r
                where r = stepbystep start  newst  l 0
                      (Bid w _ ) = l
                      newst =  validNextStep (v, teta , z) (retStone l v)
                      (_,nteta,_) = newst
                      (Peca (Curva ghi ) _ ) = retStone l v
 
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
      Recebe uma 'Bidimlist' e uma 'Posicao' e acede à 'Peca' da Bidimentional list que corresponde a essa posição   
      e devolve o valor lógico corresponde da resposta à pergunta, "É uma curva?".

      == Exemplos de utilização:
      Seja a = Bid (3,2) [Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0]

      >>> isCurv a (0,0)
      True

      >>> isCurv a (1,0)
      False

      >>> isCurv a (2,0)
      False

-}
isCurv :: Peca -> Bool
isCurv (Peca (Curva _) _ ) = True
isCurv _                   = False

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


