{- |
Module: Main
Description: Tarefa 4 da 2ª Fase do projeto de LI1.
Copyright: Mariana Miranda <a77782@alunos.uminho.pt>;
           Helena Poleri <a78633@alunos.uminho.pt>

Tarefa 6 da 2ª Fase do projeto de LI1.
-}

module Main where

import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.IO.Game
import Data.List
import GlossExtras
import System.Directory
import Data.Char
import System.Random
import System.FilePath.Find
{-import Test.HUnit hiding (Path)
import Test.Tasty
import Test.Tasty.HUnit.Adapter
import Graphics.Gloss.Rendering


-- * Testes

tests :: IO Test
tests = do
    let hunit = TestLabel "HUnit" $ TestList [testsTF]
    return $ TestList [hunit]

main = do
    tt <- tests
    defaultMain $ testGroup "Tests" $ hUnitTestToTestTree tt


-- ** Testes unitários

testsTF = TestLabel "Tarefa F" $ TestList [t1,t2,t3]
   where t1 = TestCase (assertEqual "t1" (mapa {score = (1,999), memoria = [[(11,3),(5,8),(7,7),(5,6),(7,6),(2,3),(5,3)],[(11,2),(5,8),(7,7),(5,6),(7,6),(2,3),(5,3)] ], coord =[(11,3),(5,8),(7,7),(5,6),(7,6),(2,3),(5,3)]}) (moveBoneco 'U' mapa))
         t2 = TestCase (assertEqual "t2" (mapa) (moveBoneco 'L' mapa))
         t3 = TestCase (assertEqual "t3" (mapa {nome = "namea"}) (name 'a' mapa))



mapa = Mapa {score = (0,999)
              ,memoria = [[(11,2),(5,8),(7,7),(5,6),(7,6),(2,3),(5,3)] ]
              ,nome = "name"
              ,imagens = []
              ,coord = [(11,2),(5,8),(7,7),(5,6),(7,6),(2,3),(5,3)]                
              ,mapi = ["###################","#####   ###########","#####   ###########","#####   ###########","###      ##########","### # ## ##########","#   # ## #####  ..#","#               ..#","##### ### # ##  ..#","#####     #########","###################"]
              ,nivel = 1
              ,tempo = (0,999*60)
              ,sh= 3} 



-}

-- | Função principal que invoca o jogo.


main :: IO ()
main = do
    tabuleiro1 <- readFile "Levels/001.lvl"
    let (mapinho,c) = processaMapa tabuleiro1  
    boneco <- loadBMP "Images/a2.bmp"  
    caixas <- loadBMP "Images/a1.bmp"
    inicio <- loadBMP "Images/a3.bmp"
    muda <- loadBMP "Images/a10.bmp"
    coods <- tarefa2 mapinho 1
    tab <- tarefa1 
    let imagens = [inicio,tab,boneco,caixas,muda,coods]
    joga Mapa {score = (0,999)
              ,memoria = [c]
              ,nome = "name"
              ,imagens = imagens
              ,coord = c
              ,mapi = mapinho
              ,nivel = 1
              ,tempo = (0,999*60)
              ,sh= 3} 
              desenhaMapa reageEvento reageTempo  
              

------------
-- * Função que cria o jogo
-- | Esta função interage com todos os componentes do jogo.

joga :: mundo -> (mundo -> IO Picture) -> (Event -> mundo -> IO mundo) -> (Float -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage tempo = playIO
    (InWindow "Sokoban" (1100, 600) (0, 0)) -- Tamanho da janela do jogo
    (greyN 0.5) -- Côr do fundo da janela
    60 -- refresh rate
    mapaInicial -- mapa inicial
    (\x -> desenha x) -- função que desenha o mapa
    (\x y -> reage x y) -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    (\x y -> return (tempo x y))-- função que reage ao passar do tempo 


data Mapa = Mapa  {score :: (Int,Int)
    , memoria:: [[(Float,Float)]]
    , nome :: String
    , imagens :: [Picture]
    , coord :: [(Float,Float)]
    , mapi :: [String]
    , nivel :: Int
    , tempo :: (Int,Int)
    , sh :: Int
    } 
    deriving (Eq,Show)

-- ^ Define o tipo de 'Mapa', permitindo o uso de /Records/.


-- * Função que desenha o mapa

desenhaMapa :: Mapa ->  IO Picture 
desenhaMapa m = do f <- finalpicture
                   return (Pictures [(imagens m)!!1,coorde,figura,f,moves,bestscore,(imagens m)!!0,name, temporizador])
    where
    -- bola dentro do mapa do jogo
    (xMapa,yMapa) = (toEnum (length(head (mapi m))*30),toEnum (length (mapi m))*30)   

    figura = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate ((fst(head (coord m)))*30) ((snd(head (coord m)))*30) ((imagens m)!!2)

    coorde  = Pictures [(imagens m)!!5,coords4,coords5]
      where
      coords4 = Pictures (map r ((\\) (tail (coord m)) (coordenadas '.' (mapi m))))
            where r (xCoord,yCoord) = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate (xCoord*30) (yCoord*30) ((imagens m)!!3)
      coords5 = Pictures (map r (intersect (coordenadas '.' (mapi m) ) (tail (coord m))))
            where r (xCoord,yCoord) = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate (xCoord*30) (yCoord*30) ((imagens m)!!4)


    finalpicture = if concluido m && ((nivel m)==30)
                   then do final <- loadBMP "Images/final.bmp" 
                           return (Pictures[final,Translate 120 (-55) $ Scale 0.3 0.3 $ Color white $ text (maislindo $ show (fst (score m))) ])
                   else if concluido m 
                   then do congrat <- loadBMP "Images/a7.bmp"
                           return (Pictures [congrat ,Translate 120 (-55) $ Scale 0.3 0.3 $ Color white $ text (maislindo $ show (fst (score m))) ] )
                   else return Blank

    name = if best m
              then Pictures [Translate (-125) (65) $ Color white (Polygon [(0,0),(0,130),(230,130),(230,0)]),
                             Translate (-120) (70) $ Color black (Polygon [(0,0),(0,120),(220,120),(220,0)]),
                             Translate (-110) (80) $ Color (greyN 0.8) (Polygon [(0,0),(0,40),(200,40),(200,0)]),
                             Translate (-100) (160) $ Scale 0.12 0.12 $ Color white $ text "New record set!",
                             Translate (-100) (130) $ Scale 0.12 0.12 $ Color white $ text "Write down your name:",
                             Translate  70 (100) $ Scale 1.2 1.2 $ (imagens m )!!2,
                             Translate (-100) (90) $ Scale 0.20 0.20 $ text (nome m)] 
              else Blank

    moves = Translate 420 90 $ Scale 0.30 0.30  $ Color black $ text (maislindo $ show (fst (score m)))
    
    bestscore = Pictures [Translate 415 (-115) $ Scale 0.12 0.12  $ Color black $ text (nome m),
                          Translate 430 (-135) $ Scale 0.12 0.12  $ Color black $ text (maislindo $ show (div (snd (tempo m)) 60)),
                          Translate 460 (-135) $ Scale 0.12 0.12  $ Color black $ text "s" ,
                          Translate 420 (-175) $ Scale 0.30 0.30  $ Color black $ text (maislindo $ show (snd (score m)))]

    temporizador = Translate 415 (-20) $ Scale 0.30 0.30  $ Color black $ text $ maislindo $ show (div (fst(tempo m)) 60)

-- ^ Transforma o 'Mapa' numa IO Picture, ou seja, na sua representação gráfica.


-- * Função que reage a um evento (carregar numa tecla, mover o rato, etc)

reageEvento :: Event -> Mapa -> IO Mapa
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) mapa = return (moveBoneco 'U' mapa)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) mapa = return (moveBoneco 'D' mapa)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) mapa = return (moveBoneco 'L' mapa)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) mapa = return (moveBoneco 'R' mapa)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) mapa = return (restart mapa)
reageEvento (EventKey (SpecialKey KeyTab) Down _ _) mapa = return (undo mapa)
reageEvento (EventKey (Char 'a') Down _ _) mapa = return (name 'a' mapa)
reageEvento (EventKey (Char 'b') Down _ _) mapa = return (name 'b' mapa)
reageEvento (EventKey (Char 'c') Down _ _) mapa = return (name 'c' mapa)
reageEvento (EventKey (Char 'd') Down _ _) mapa = return (name 'd' mapa)
reageEvento (EventKey (Char 'e') Down _ _) mapa = return (name 'e' mapa)
reageEvento (EventKey (Char 'f') Down _ _) mapa = return (name 'f' mapa)
reageEvento (EventKey (Char 'g') Down _ _) mapa = return (name 'g' mapa)
reageEvento (EventKey (Char 'h') Down _ _) mapa = return (name 'h' mapa)
reageEvento (EventKey (Char 'i') Down _ _) mapa = return (name 'i' mapa)
reageEvento (EventKey (Char 'j') Down _ _) mapa = return (name 'j' mapa)
reageEvento (EventKey (Char 'k') Down _ _) mapa = return (name 'k' mapa)
reageEvento (EventKey (Char 'l') Down _ _) mapa = return (name 'l' mapa)
reageEvento (EventKey (Char 'm') Down _ _) mapa = return (name 'm' mapa)
reageEvento (EventKey (Char 'n') Down _ _) mapa = return (name 'n' mapa)
reageEvento (EventKey (Char 'o') Down _ _) mapa = return (name 'o' mapa)
reageEvento (EventKey (Char 'p') Down _ _) mapa = return (name 'p' mapa)
reageEvento (EventKey (Char 'q') Down _ _) mapa = return (name 'q' mapa)
reageEvento (EventKey (Char 'r') Down _ _) mapa = return (name 'r' mapa)
reageEvento (EventKey (Char 's') Down _ _) mapa = return (name 's' mapa)
reageEvento (EventKey (Char 't') Down _ _) mapa = return (name 't' mapa)
reageEvento (EventKey (Char 'u') Down _ _) mapa = return (name 'u' mapa)
reageEvento (EventKey (Char 'v') Down _ _) mapa = return (name 'v' mapa)
reageEvento (EventKey (Char 'w') Down _ _) mapa = return (name 'w' mapa)
reageEvento (EventKey (Char 'x') Down _ _) mapa = return (name 'x' mapa)
reageEvento (EventKey (Char 'y') Down _ _) mapa = return (name 'y' mapa)
reageEvento (EventKey (Char 'z') Down _ _) mapa = return (name 'z' mapa)
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) mapa = return ( name ' ' mapa) --
reageEvento (EventKey (SpecialKey KeyDelete) Down _ _) mapa = return (name '|' mapa)
reageEvento (EventKey (SpecialKey KeyF1) Down _ _) mapa = shuffle mapa
reageEvento (EventKey (SpecialKey KeyF2) Down _ _) mapa = nextlevel mapa  
reageEvento (EventKey (SpecialKey KeyF3) Down _ _) mapa = previouslevel mapa
reageEvento _ mapa = return mapa -- ignora qualquer outro evento

-- ^ Reage ao pressionar das setas do teclado, modificando o 'Mapa', e por consequência, a representação gráfica. 


{- ^
 == Funções que controlam o movimento do boneco e das caixas.
-}

moveBoneco :: Char -> Mapa -> Mapa
moveBoneco a m = m {score =((fst (score m)) + conta,snd (score m)), memoria= guard, coord = movimento }                                                 
                       where
                          movimento = move a (coord m) (reverse (mapi m)) 
                          conta = if equiv movimento (head(memoria m)) then 0 else 1 
                          guard = if conta== 0 then (memoria m) else movimento : (memoria m)

-- ^ Esta função recebe um movimento e um 'Mapa' e retorna o 'Mapa' resultante desse movimento.


-- | A função /move/ recebe um comando, as coordenadas do boneco e das caixas e o tabuleiro e devolve as coordendas do boneco e da caixas depois das alterações resultantes desse comando.

move :: Char -> [(Float,Float)] -> [String] -> [(Float,Float)]
move x l l1  | equiv (coordenadas '.' (reverse l1)) l2= ((a,b):l2) 
             | x == 'L' &&  moveM (a-1,b) (a-2,b) l2 l1 = ((a-1,b):(moveCaixa (a,b) (a-1,b) l2)) 
             | x == 'R' &&  moveM (a+1,b) (a+2,b) l2 l1 = ((a+1,b):(moveCaixa (a,b) (a+1,b) l2)) 
             | x == 'U' &&  moveM (a,b+1) (a,b+2) l2 l1 = ((a,b+1):(moveCaixa (a,b) (a,b+1) l2))
             | x == 'D' &&  moveM (a,b-1) (a,b-2) l2 l1 = ((a,b-1):(moveCaixa (a,b) (a,b-1) l2))
             | otherwise = ((a,b):l2)
                      where (a,b) = head l
                            l2 = tail l 


-- | Conforme a posição anterior e a atual do boneco verifica se houve movimento de uma caixa e devolve uma lista com as coordenadas novas das caixas.

moveCaixa :: (Float,Float)->(Float,Float) -> [(Float,Float)]-> [(Float,Float)]
moveCaixa (f,e) (a,b) [] = []
moveCaixa (f,e) (a,b) ((h,i):t) | (a,b) == (h,i)  && (f,e) == (h-1,i) = ((h+1,i):t)  
                                | (a,b) == (h,i)  && (f,e) == (h+1,i) = ((h-1,i):t) 
                                | (a,b) == (h,i)  && (f,e) == (h,i+1) = ((h,i-1):t)
                                | (a,b) == (h,i)  && (f,e) == (h,i-1) = ((h,i+1):t)
                                | otherwise = (h,i): moveCaixa (f,e) (a,b) (t)


moveM :: (Float,Float) -> (Float,Float ) -> [(Float,Float)] -> [String] -> Bool
moveM a b l2 l1 | isParede a l1  = False
                | isCaixa a l2 && isParede b l1 = False
                | isCaixa a l2 && isCaixa b l2 = False
                | otherwise = True
                      where isCaixa (a,b) l2 = or (map (\(x,y) -> (a==x) && (b==y)) l2) 
                            isParede (a,b) l1 = ((l1!! (fromEnum b))!! (fromEnum a)) == '#' 

-- ^ Verifica se um determinado movimento é possível.


------
{- ^
 == Função que permite fazer /restart/.
-}

restart :: Mapa -> Mapa
restart m = if best m 
            then m {score = (0,fst (score m)),memoria = [last (memoria m)], coord = last (memoria m),  tempo = (0,fst (tempo m)),sh= 3 }
            else m {score = (0,snd (score m)), memoria = [last (memoria m)], imagens = (Blank: tail (imagens m)), coord = last (memoria m), tempo = (0,snd (tempo m)), sh = 3 }

-- ^ Esta função faz o jogo voltar ao seu estado inicial.

{- ^
 == Função que permite fazer /undo/.
-}

undo :: Mapa -> Mapa
undo m = if fst (score m)== 0 || concluido m 
         then m 
         else m {score = (fst (score m) -1 ,snd(score m)), memoria = tail (memoria m), coord = head (tail (memoria m)) }

-- ^ Esta função faz o jogo retroceder uma jogada.


{- ^
 == Função que permite escrever o nome.
-}

name :: Char -> Mapa -> Mapa
name a m | (worse m) = m
         | a== '|' && length (nome m) >0 = m {nome = init (nome m)}
         | length (nome m)==8 = m
         | otherwise = m {nome = delete '|' ((nome m) ++ [a])}

-- ^ Esta função permite escrever o nome se obtiver-se uma melhor pontuação.


{- ^
 == Funções que permitem avançar de nivel.
-}

nextlevel :: Mapa -> IO Mapa
nextlevel m = if nivel m == 30 || (nivel m ==1 && head (imagens m) /= Blank) then return m
              else do let mapa = m {nivel =  nivel m + 1} 
                      tabuleiro <- readFile ("Levels/"++ (maislindo $ show ((nivel m)+1))++".lvl")
                      constroimapa mapa tabuleiro

-- ^ Constrói o nível seguinte, exceto de se estiver no último.


-- | Constrói o nível anterior, exceto de se estiver no primeiro.

previouslevel :: Mapa -> IO Mapa
previouslevel m = if (nivel m)==1 then return m
                  else do let mapa = m {nivel = (nivel m)-1}
                          tabuleiro <- readFile ("Levels/"++ (maislindo $ show ((nivel m)-1))++".lvl")
                          constroimapa mapa tabuleiro

constroimapa :: Mapa -> String -> IO Mapa
constroimapa  m tab = do let (mapinh,c) = processaMapa  tab
                             d = tarefa2  mapinh (nivel m)
                         f<- d
                         return m   {score =(0,999)
                                    ,memoria =[c]
                                    ,nome ="name"
                                    ,imagens =((init (imagens m))++[f])
                                    ,coord = c
                                    ,mapi = mapinh
                                    ,tempo = (0,999*60)
                                    ,sh = 3}

-- ^ Atravês de um 'Mapa' e um ficheiro, contrói um novo nível de acordo com a informação desse ficheiro.


{- ^
 == Função que permite fazer /shuffle/.
-}

shuffle :: Mapa -> IO Mapa
shuffle m = if (sh m) == 0 then return m else 
            do x <- readFile ("Shuffle/"++show (nivel m) ++".sh")
               let y = leCoordenadas  $ inStr x 
                   z = (\\) (delete (head (coord m)) y) b 
                   b =  intersect (coordenadas '.' (mapi m) ) (tail (coord m))
               y <- aux ((length (tail(coord m)))-(length b))  z 
               return m {coord =[head (coord m)]++y++b, score = (if concluido m then fst(score m) else fst (score m)+1, snd (score m)), sh = sh m-1, memoria = ([head (coord m)]++y++b) : (memoria m)}
                       where 
                          aux 0 x = return []
                          aux n x = do o <- randomRIO (0, (length x)-1)
                                       let (x1,x2) = splitAt o x 
                                       z <- aux (n-1) (x1 ++ (tail x2))
                                       return ((head x2) : z)

-- ^ A função escolhe aleatoriamente novos locais para as caixas, preservando aquelas que já estão num local de arrumação.


-- * Função que reage ao passar do tempo 

reageTempo :: Float -> Mapa -> Mapa
reageTempo x m | (concluido m || fst (tempo m) == 999*60) = m
               | otherwise = m {tempo = (fst (tempo m)+1, snd (tempo m))} 

-- ^ Esta função permite contabilizar oo tempo. 


{- ^
 = Funções auxiliares
-}

inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

-- ^ Esta função recebe uma string e transforma-a numa lista de strings, dividindo-a quando encontrar um @__'\n'__@.


-- | Recebe um caractere e um tabuleiro e devolve as coordenadas onde ocorre aquele carater.

coordenadas :: Char -> [String]-> [(Float,Float)]
coordenadas z l1 = lugar 0 0  (reverse l1)
                     where lugar a b [] = []
                           lugar a b ([]:xs) = lugar 0 (b+1) xs
                           lugar a b ((c:d):xs) | c==z = (fromIntegral a, fromIntegral b): lugar (a+1) b ((d):xs) 
                                                | otherwise = lugar (a+1) b ((d):xs) 

-- | Função que recebe um ficheiro e processa-o de modo a ficar com um tuplo cujo o primeiro elemento é o tabuleiro e o segundo as coordenadas.

processaMapa :: String -> ([String],[(Float,Float)])
processaMapa linhas  = let (x,y) = parteMapa $ inStr linhas
                       in (x, leCoordenadas y) 


-- | Função que divide a parte das coordenadas da parte do tabuleiro.

parteMapa :: [String] -> ([String],[String])
parteMapa linhas = splitAt (comprimento linhas) linhas
      where comprimento [] = 0  --  Função que calcula o comprimento do tabuleiro (sem a parte das coordenadas).
            comprimento ([]:xs) = comprimento xs
            comprimento ((a:b):xs) = if isDigit a then 0 else 1 + comprimento xs


{- | Função que recebe a parte das coordenadas e as transforma para o formato (x,y).

@
 >>> leCoordenadas ["1 1",\"3 4"]
[(1,1),(3,4)]
@

-}

leCoordenadas :: [String] -> [(Float,Float)]
leCoordenadas xs = map leCoordenada ( processa xs)
      where processa [] = []
            processa (x:xs) = if  isEspaco x then [] else x:processa xs
                      where  isEspaco [] = True                   
                             isEspaco x = and (map (\a -> a==' ' ) x)
            leCoordenada s = (read x,read y) -- Função que transforma uma coordenada para o formato (x,y).
             where [x,y] = words s 


{- | Retira os caracteres @__'#'__@ que forem redundantes.

@
>>>  rmov  ["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"] ["\#\#\#\#\#\#\#\#\#\#","\#\#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \#\#","\#\#\#\#\#\#\#\#\#\#"]
[" \#\#\#\#\#\#\#  "," \#     \#\#\#","\#\# \#\#\#   \#","\#        \#","\# ..\#   \#\#","\#\#..\#   \# "," \#\#\#\#\#\#\#\# "]
@

-}

rmov :: [String]-> [String]
rmov x = remove (0,0) x x 
              where
                 remove (a,b) [] l = []
                 remove (a,b) (x:xs) l = remove1 x (a,b) l ++ remove (a,b+1) xs l
                    where remove1 [] (c,d) l = []      --  Esta função recebe uma linha do tabuleiro e remove os carateres '#' que são redundantes nessa linha.
                          remove1 (a:b) (c,d) l = [add x]
                              where x = [(remove2 a (c,d) l)] ++ remove1 b (c+1,d) l
                                    add [] = []      --  Esta função transforma uma lista de strings numa só string.
                                    add (x:xs) = x ++ add xs


{- | Verifica se a remoção do caractere @__'#'__@ é possível. Caso seja possível, remove-o; caso contrário, mantém-no.


@
>>>  remove2 \'#'\ (1,1) ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 
"#"
@

@
>>>  remove2 \'#'\ (0,0) ["\#\#\#\#\#","\#\#\ \#\#","\#\#\#\#\#"] 
" "
@

-}

remove2 a (c,d) l = if (a == '#' && ((isOcto (c+1,d) l) ==  '#') && ((isOcto (c-1,d) l) == '#') && ((isOcto (c,d+1) l) == '#') && ((isOcto (c,d-1) l) == '#') && ((isOcto (c-1,d-1) l) == '#') && ((isOcto (c+1,d-1) l) == '#')  && ((isOcto (c-1,d+1) l) == '#') && ((isOcto (c+1,d+1) l) == '#')) then " " else [a]
                         where 
                               isOcto (a,-1) l1 = '#'  -- Dá-nos o caractere num determinado local do tabuleiro e caso este esteja fora deste, assume que é um '#'.
                               isOcto (-1, b) l1 = '#'
                               isOcto (a,b) l1 = if b == (length l1) then '#' else ncoluna2 a (l1!!b)
                                            where  -- Recebe uma posição e uma linha, e devolve o caractere que se encontra nessa posição.
                                                ncoluna2 a [x] = x -- Este caso faz que no final de uma linha, mesmo que o ponto esteja fora do mapa, devolva '#'.
                                                ncoluna2 a (x:xs) | a == 0 = x
                                                                  | otherwise = ncoluna2 (a-1) xs 


-- | Verifica se duas listas de coordenadas são iguais.

equiv :: [(Float,Float)] ->  [(Float,Float)] -> Bool
equiv poli1 poli2 = and (map (\(a,b) -> elem (a,b) poli1) poli2)


-- | Adiciona @__'0'__@ à esquerda de um número para uniformizar a sua apresentação.

maislindo :: String -> String
maislindo x | length x == 1 = "00" ++ x
            | length x == 2 = "0" ++  x
            | otherwise = x


-- | Verfica se o jogo está completo, ou seja , concluido.

concluido m = equiv (coordenadas '.' (mapi m)) (tail (coord m))


-- | Verifica se teve uma pior prestação que um jogador anterior.

worse m = (concluido m && (fst (score m)) == (snd (score m)) && fst (tempo m)>snd(tempo m)) || (concluido m && fst (score m)>snd (score m))


-- | Verifica se teve uma melhor prestação que qualquer jogador anterior.

best m = (concluido m && fst (score m) == snd (score m) && fst (tempo m)<=snd (tempo m)) || (concluido m && fst (score m)<snd (score m))


-- | Cria a parte da interface gráfica que não se altera no decorrer do jogo.

tarefa1 :: IO Picture
tarefa1  = do fundo <- loadBMP "Images/a4.bmp"
              best <- loadBMP "Images/bestscore.bmp"
              moves <- loadBMP "Images/moves.bmp"
              time <- loadBMP "Images/time.bmp"
              let
                                      --tabuleiro = Translate (-xMapa/2) (-yMapa/2) $ Color white (Polygon [(0,0),(0,yMapa),(xMapa,yMapa),(xMapa,0)])
                                      --borda = Translate (-(xMapa+20)/2) (-(yMapa+20)/2) $ Color black (Polygon [(0,0),(0,yMapa + 20),(xMapa + 20,yMapa + 20),(xMapa + 20,0)])
                 move = Pictures [Translate 390 65 $ Color black (Polygon [(0,0),(0,100),(130,100),(130,0)]),
                                  Translate 400 75 $ Color white (Polygon [(0,0),(0,60),(110,60),(110,0)]),
                                  Translate 455 145 $ moves] 
                 bestscore = Pictures [Translate 390 (-200) $ Color black (Polygon [(0,0),(0,140),(130,140),(130,0)]),
                                       Translate 400 (-190) $ Color white (Polygon [(0,0),(0,100),(110,100),(110,0)]),
                                       Translate 455 (-80) $ best] 
                 tempo = Pictures [Translate 390 (-45) $ Color black (Polygon [(0,0),(0,100),(130,100),(130,0)]),
                                   Translate 400 (-35) $ Color white (Polygon [(0,0),(0,60),(110,60),(110,0)]),
                                   Translate 485 (-20) $ Scale 0.30 0.30 $ Scale 0.5 0.5 $ Color black $ text "s",
                                   Translate 455 35 $ time]
              return $ Pictures [fundo,move,bestscore,tempo]


-- | Cria a parte da interface gráfica que apenas só se altera quando se muda de nivel.

tarefa2 :: [String] -> Int  -> IO Picture 
tarefa2 mapinho x = do parede <- loadBMP "Images/a5.bmp"
                       ponto <- loadBMP "Images/a6.bmp"
                       chao <- loadBMP "Images/a9.bmp"
                       level <- loadBMP ("Images/l" ++ show x ++ ".bmp")
                       esq <- if x==1 then loadBMP "Images/esq1.bmp" 
                              else if x==30 then loadBMP "Images/esq3.bmp"
                              else loadBMP "Images/esq2.bmp"
                       let
                        esqu = Scale 0.7 0.7 $ Translate (-550) (0) esq
                        lev = Translate 0 250 $ level
                        (xMapa,yMapa) = (toEnum (length(head mapinho)*30),toEnum (length mapinho)*30)
                        coords1 = Pictures (map r (coordenadas '#' (rmov mapinho)))
                                 where r (xCoord,yCoord) = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate (xCoord*30) (yCoord*30) parede 
                        coords2 = Pictures (map r (coordenadas ' ' mapinho))
                                 where r (xCoord,yCoord) = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate (xCoord*30) (yCoord*30) chao
                        coords3 = Pictures (map r (coordenadas '.' mapinho))
                                 where r (xCoord,yCoord) = Translate ((-xMapa/2)+15) ((-yMapa/2)+15) $ Translate (xCoord*30) (yCoord*30) ponto
                       return $ Pictures [esqu,coords1,coords2,coords3,lev]