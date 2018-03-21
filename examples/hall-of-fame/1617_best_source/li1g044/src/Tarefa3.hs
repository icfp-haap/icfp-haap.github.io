{-|
Module      : Main
Description : Módulo Haskell que codifica e descodifica o estado de jogo
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Um módulo capaz de comprimir o estado de um jogo quando recebe como parâmetros @-e@ (usando a função @'encode'@) e o estado de jogo.

Este módulo é ainda capaz de reverter o estado de jogo comprimido quando recebe os parâmetros @-d@ (usando a função @'decode'@) e o estado de jogo codificado anteriormente com o parâmetro @-e@
-}
module Tarefa3 where

import System.Environment
import Data.Char
import Data.List

-- | A função @'divisaoJogo'@ recebe o estado de jogo e dá um par com o mapa no 1º elemento e as restantes informações no 2º elemento
divisaoJogo :: [String] -> ([String],[String])
divisaoJogo l = splitAt (length (head l)) l

-- | Esta função divide numa lista as strings de uma lista de strings nos locais onde exista um espaço, devolvendo uma lista de listas de strings.
substituiEspacos :: [String] -> [[String]]
substituiEspacos [] = []
substituiEspacos (l:ls) = words l : substituiEspacos ls

-- | Esta função divide uma string com power ups, bombas e jogadores num triplo de strings com power ups no 1º elemento, bombas no 2º e jogadores no 3º elemento (power ups,bombas,jogadores)
pUpsEResto :: [String] -> ([[String]],[[String]],[[String]])
pUpsEResto l = aux (substituiEspacos$snd$divisaoJogo l) ([],[],[])
        where aux [] (a,b,c) = (reverse a,reverse b,c)
              aux (l:ls) (a,b,c) | head l == "+" || head l == "!" = aux ls ((l:a),b,c)
                                 | head l == "*" = aux ls (a,(l:b),c)
                                 | otherwise = (reverse a,reverse b,(l:ls))



{-| Esta função coloca os power ups no código do mapa com o seguinte código:

* @+@ -> power up bomba coberto por um tijolo;
* @!@ -> power up flame coberto por um tijolo;
* @%@ -> power up bomba descoberto;
* @&@ -> power up flame descoberto.
-}
codificacaoPUpsnoMapa :: Int -> String -> [[String]] -> String
codificacaoPUpsnoMapa _ m [] = m
codificacaoPUpsnoMapa c m ([p,x,y]:ls) | head (drop (a*c+b) m) == '?' = codificacaoPUpsnoMapa c ((take (a*c+b) m) ++ p ++ (drop (a*c+b+1) m)) ls
                             | head (drop (a*c+b) m) == ' '  && p=="+" = codificacaoPUpsnoMapa c ((take (a*c+b) m) ++ '%' : (drop (a*c+b+1) m)) ls
                             | otherwise = codificacaoPUpsnoMapa c ((take (a*c+b) m) ++ '&' : (drop (a*c+b+1) m)) ls
                            where (a,b) = (read x,read y)

-- | Esta função retira os elementos comuns a todos os mapas, isto é, as pedras e os espaços vazios nos cantos
filtracaoMapa :: Int -> String -> String
filtracaoMapa c l = filter ('#'/=) (aux l c 1)
        where aux [] _ _ = []
              aux l c k | k==(c+2) || k==(2*c-2) || k==(c*(c-2)+2) || k==(c*(c-1)-2) = aux (drop 2 l) c (k+2)
                        | k==(2*c+2) || k==(3*c-1) || k==(c*(c-3)+2) || k==(c*(c-2)-1) = aux (tail l) c (k+1)
                        | otherwise = head l : aux (tail l) c (k+1)

-- | Esta função substitui caracteres repetidos seguidos
substituiRepetidos :: String -> String
substituiRepetidos l = aux$group l
            where aux [] = []
                  aux ((h:t):ls) | length (h:t) == 1 = h : aux ls
                                 | otherwise = h : show (length t) ++ aux ls

-- | Esta função recebe o estado de jogo e devolve o mapa codificado com os power ups
mapaComPUps :: [String] -> String
mapaComPUps l = substituiRepetidos$filtracaoMapa s (codificacaoPUpsnoMapa s (concat$fst$divisaoJogo l) a)
                    where (a,b,c) = pUpsEResto l
                          s = length$head l

-- | Esta função converte as coordenadas e outros valores representados por números para um caracter corresponente
converteParaChr :: [[String]] -> String
converteParaChr [] = []
converteParaChr ((h:t):ls) = h ++ (map (\x -> chr (44 + read x)) a) ++ concat b ++ converteParaChr ls
            where (a,b) = span (\x -> isDigit (head x)) t

-- | Função de codificação do mapa alternativa, esta função recebe uma string do mapa filtrado e substitui os caracteres repetidos por caracteres correspondentes ao número de vezes que um caracter é repetido
codificacaoMapaAlternativo :: String -> String
codificacaoMapaAlternativo l = x : aux (group l)
        where aux [] = []
              aux (l:ls) = chr (44 + length l) : aux ls
              x = if head l == ' ' then '$' else '#'

-- | Esta função recebe o estado de jogo e devolve o mapa com a codificação alternativa
mapaAlternativo :: [String] -> String
mapaAlternativo l =  codificacaoMapaAlternativo (filtracaoMapa (length$head l) (concat$fst$divisaoJogo l))

-- | Função que irá receber strings com power ups o bombas e irá manter apenas o 1º identificador de power up ou bomba
retiraDesnecessario :: String -> String
retiraDesnecessario [] = []
retiraDesnecessario (h:t) = h : filter (/=h) t

-- | Função que decide qual o método de compressão a usar (@'mapaComPUps'@ ou @'mapaAlternativo'@)
decisao :: [String] -> String
decisao l =  if length b < length c then b else c
            where (a,_,_) = pUpsEResto l
                  b = mapaComPUps l
                  c = mapaAlternativo l ++ retiraDesnecessario (converteParaChr d) ++ retiraDesnecessario (converteParaChr e)
                  (d,e) = span (\h -> head h /= "!") a

-- | Esta função recebe o estado de jogo e devolve-o codificado
encode :: [String] -> String
encode l | s == '1' = s : converteParaChr b ++ '(' : converteParaChr c
         | otherwise = s : decisao l ++ retiraDesnecessario (converteParaChr b) ++ '(' : converteParaChr c
        where (a,b,c) = pUpsEResto l
              s = chr (44 + (length$head l))





-- | Divide o código em (tamanho do mapa,mapa e power ups,bombas,jogadores)
divisaoCodigo :: String -> (Int,String,String,String)
divisaoCodigo (h:t) = (a,b,c,d)
        where a = (ord h) - 44
              b = takeWhile (\h -> '*'/=h && '(' /=h) t
              c = takeWhile ('('/=) (dropWhile ('*'/=) t)
              d = tail (dropWhile ('('/=) t)



-- | Divide num par uma string com o mapa e os power ups codificados
divideMapaEPowerUpsAlternativo :: String -> (String,String)
divideMapaEPowerUpsAlternativo l = span (\h -> '+'/=h && '!'/=h) l


-- | Descodifica uma string referente ao mapa
descodificaMapaAlternativo :: String -> String
descodificaMapaAlternativo (h:t) = aux h t
    where aux _ [] = []
          aux x (h:t) | x=='$' = replicate (ord h -44) ' ' ++ aux '#' t
                      | otherwise = replicate (ord h -44) '?' ++ aux '$' t





-- | Função inversa de @'substituiRepetidos'@
multiplicaRepetidos :: String -> String
multiplicaRepetidos [] = []
multiplicaRepetidos (h:t) = aux h (takeWhile isDigit t) ++ multiplicaRepetidos (dropWhile isDigit t)
        where aux k n | n == [] = [k]
                      | otherwise = replicate ((read n)+1) k

-- | Esta função recebe a String referente ao mapa e reconstroi o mapa, esta função insere os elementos retirados em @'filtracaoMapa'@
construcaoMapa :: String -> Int -> Int -> [String]
construcaoMapa l c k | k==0 || k==(c-1) = (replicate c '#') : construcaoMapa l c (k+1)
                     | l==[] = []
                     | k==1 || k==(c-2) = aux2 l (c-1) 0 : construcaoMapa (drop (c-6) l) c (k+1)
                     | k==2 || k==(c-3) = aux1 l (c-1) 0 : construcaoMapa (drop ((div c 2)-2) l) c (k+1)
                     | odd k = auxodd l (c-1) 0 : construcaoMapa (drop (c-2) l) c (k+1)
                     | otherwise = auxeven l (c-1) 0 : construcaoMapa (drop (div c 2) l) c (k+1)
        where auxeven l c k = '#' : intersperse '#' (take (div c 2) l) ++ "#"
              auxodd l c k = '#' : take (c-1) l ++ "#"
              aux1 l c k | k==0 = '#':' ':'#' : aux1 l c (k+3)
                         | k==(c-1) = " #"
                         | otherwise = (head l):'#' : aux1 (tail l) c (k+2)
              aux2 l c k | k==0 = '#':' ':' ' : aux2 l c (k+3)
                         | k==(c-2) = "  #"
                         | otherwise = (head l) : aux2 (tail l) c (k+1)



-- | Esta função recebe o mapa construido e retira a posição dos powerUps de bomba
posicaoBPower :: [String] -> (Int,Int) -> [Int]
posicaoBPower [] _ = []
posicaoBPower (l:ls) (x,y) =baux l (x,y) (length l) ++ posicaoBPower ls (x+1,y)
        where baux [] _ _ = []
              baux (h:t) (x,y) c | h=='+' || h=='%' = (x + y*c) : baux t (x,y+1) c
                                 | otherwise = baux t (x,y+1) c

-- | Esta função recebe o mapa construído e retira a posição dos powerUps de flames
posicaoFPower :: [String] -> (Int,Int) -> [Int]
posicaoFPower [] _ = []
posicaoFPower (l:ls) (x,y) = faux l (x,y) (length l) ++ posicaoFPower ls (x+1,y)
        where faux [] _ _ = []
              faux (h:t) (x,y) c | h=='!' || h=='&' = (x+c*y) : faux t (x,y+1) c
                                 | otherwise = faux t (x,y+1) c

-- | Cria as strings indentificadoras de power ups de bomba a partir do tamanho do mapa e das posições ordenadas dos power ups de bomba
descodificacaoPUpB :: Int -> [Int] -> [String]
descodificacaoPUpB _ [] = []
descodificacaoPUpB c (h:t) = ('+' : ' ' : show (mod h c) ++ ' ' : show (div h c)) : descodificacaoPUpB c t

-- | Cria as strings indentificadoras de power ups de flames a partir do tamanho do mapa e das posições ordenadas dos power ups de flames
descodificacaoPUpF :: Int -> [Int] -> [String]
descodificacaoPUpF _ [] = []
descodificacaoPUpF c (h:t) = ('!' : ' ' : show (mod h c) ++ ' ' : show (div h c)) : descodificacaoPUpF c t


-- | Agrupa e organiza as strings dos power ups
agrupaPowerUps :: [String] -> [String]
agrupaPowerUps l = descodificacaoPUpB (length l) (sort (posicaoBPower l (0,0))) ++ descodificacaoPUpF (length l) (sort (posicaoFPower l (0,0)))

-- | Função que recebe uma string referente ao mapa com os power ups e devolve o mapa construído sem os power ups
mapaSemPUps :: String -> [String]
mapaSemPUps l = construcaoMapa (map aux (multiplicaRepetidos b)) a 0
            where aux x | x=='+' || x=='!' = '?'
                        | x=='%' || x=='&' = ' '
                        | otherwise = x
                  (a,b,c,d) = divisaoCodigo l




-- | Função que recebe uma string codificada referente aos power ups e devolve os power ups numa lista de strings
descodificaPups :: String -> [String]
descodificaPups [] = []
descodificaPups (h:t) = if h == '+' then auxb t else auxf t
            where auxb [] = []
                  auxb l | head l=='!' = auxf (tail l)
                         | otherwise = ("+ " ++ unwords (map (\h -> show (ord h -44)) (take 2 l))) : auxb (drop 2 l)
                  auxf [] = []
                  auxf l =  let (a,b) = splitAt 2 l in ("! " ++ unwords (map (\h -> show (ord h -44)) a)) : auxf b

-- | Função que recebe uma string codificada referente às bombas e devolve as bombas numa lista de strings
descodificaBombas :: String -> [String]
descodificaBombas [] = []
descodificaBombas (h:t) = aux h t
            where aux _ [] = []
                  aux x l = ("* " ++ unwords (map (\h -> show (ord h -44)) a)) : aux x b
                        where (a,b) = splitAt 5 l
-- | Função que recebe uma string codificada referente aos jogadores e devolve os jogadores numa lista de strings
descodificaJogadores :: String -> [String]
descodificaJogadores [] = []
descodificaJogadores (h:t) = (h : ' ' : aux a ++ e) : descodificaJogadores d
            where aux l = unwords (map (\h -> show (ord h -44)) l)
                  (a,b) = splitAt 2 t
                  (c,d) = span (\h -> not$isDigit h) b
                  e = if length c == 0 then [] else ' ' :c

-- | Esta função identifica o tipo de codificação e descodifica a string fornecida e devolve o estado de jogo
decode :: String -> [String]
decode l | a==5 = ["#####","#   #","# # #","#   #","#####"] ++ descodificaBombas c ++ descodificaJogadores d
         | head b == '$' || head b == '#' = construcaoMapa (descodificaMapaAlternativo e) a 0 ++ descodificaPups f ++ descodificaBombas c ++ descodificaJogadores d
         | otherwise = mapaSemPUps l ++ agrupaPowerUps (construcaoMapa (multiplicaRepetidos b) a 0) ++ descodificaBombas c ++ descodificaJogadores d
    where (a,b,c,d) = divisaoCodigo l
          (e,f) = divideMapaEPowerUpsAlternativo b





main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"

