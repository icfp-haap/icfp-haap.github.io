module Tarefa4_Safewhere
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

type Mapa = [String]

type Linha = String

type PowerType = Char

type Range = Int

type Abcissa = Int

type Ordenada = Int

type Tempo = Int

type Cords = (Abcissa, Ordenada)

type Dim = Int

type Jogador = Int
ln x = mapM_ putStrLn x

avanca :: Mapa -> Tempo -> Mapa
avanca mapa n
  = if n > time then
      mergeMaps (explosionsMap (reduce mapa) (reduce mapa)) [] else
      espiral (mergeMaps (explosionsMap (reduce mapa) (reduce mapa)) [])
        n
  where time = (d - 2) ^ 2
        d = length (mapa !! 0)
mapa1
  = ["#########", "##      #", "# #?# # #", "#     ? #", "#?# # #?#",
     "# ?  ?  #", "# #?#?# #", "#  ??   #", "#########", "+ 3 3",
     "! 5 5", "* 7 7 1 1 1", "0 4 3 +", "1 7 7"]

main :: IO ()

reduce :: Mapa -> Mapa
reduce [] = []
reduce (h : t)
  = if (h !! 0) == '*' then (tickBomb h) : reduce t else h : reduce t

tickBomb :: Linha -> Linha
tickBomb h
  = if last h == '0' then (take (length h - 2) h) ++ "9" else
      (take (length h - 1) h) ++
        (show (read (drop (length h - 1) h) - 1))

explosionsMap :: Mapa -> Mapa -> [Mapa]
explosionsMap mapa [] = [mapa]
explosionsMap mapa (h : t)
  = if (h !! 0) == '*' && last h == '0' then
      (explode mapa h) : explosionsMap mapa t else explosionsMap mapa t

mergeMaps :: [Mapa] -> Mapa -> Mapa
mergeMaps [] m = m
mergeMaps [a] _ = a
mergeMaps ([] : ([] : t)) m = mergeMaps (m : t) []
mergeMaps (a : ([] : t)) m = mergeMaps (m : t) []
mergeMaps ([] : (b : t)) m = mergeMaps (m : t) []
mergeMaps ((a : as) : ((b : bs) : t)) m
  | a == b = mergeMaps (as : bs : t) (m ++ [a])
  | head a == '#' =
    if howMany ' ' a > howMany ' ' b then
      mergeMaps (as : bs : t) (m ++ [a]) else
      mergeMaps (as : bs : t) (m ++ [b])
  | head a == '*' && head b == '*' && cordsT a == cordsT b =
    if last a == '1' then mergeMaps (as : bs : t) (m ++ [a]) else
      mergeMaps (as : bs : t) (m ++ [b])
  | head a == '*' && last a == '1' =
    mergeMaps (as : (b : bs) : t) (m ++ [a])
  | head b == '*' && last b == '1' =
    mergeMaps ((a : as) : bs : t) (m ++ [b])
  | elem a (b : bs) = mergeMaps (as : bs : t) (m ++ [a])
  | elem b (a : as) = mergeMaps (as : bs : t) (m ++ [b])
  | otherwise = mergeMaps (as : bs : t) m

howMany :: PowerType -> Linha -> Int
howMany _ [] = 0
howMany c (h : t) = if h == c then 1 + howMany c t else howMany c t

explode :: Mapa -> Linha -> Mapa
explode mapa string
  = removeBomb
      (flameUp
         (flameDown (flameRight (flameLeft mapa x y r) x y r) x y r)
         x
         y
         r)
      x
      y
  where x = pAbcis string
        y = pOrden string
        r = range string

flameUp :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
flameUp mapa _ _ 0 = mapa
flameUp mapa x y n
  | ((mapa !! y) !! x) == '#' = mapa
  | ((mapa !! y) !! x) == '?' =
    (take y mapa) ++
      [(take x (mapa !! y)) ++ " " ++ (drop (x + 1) (mapa !! y))] ++
        (drop (y + 1) mapa)
  | isTherePower mapa x y = removePower mapa x y
  | isTherePlayer mapa x y = (flameUp (removePlayer mapa x y) x y n)
  | isThereBomb mapa x y =
    (flameUp (blowTimer mapa x y) x (y - 1) (n - 1))
  | otherwise = flameUp mapa x (y - 1) (n - 1)

flameDown :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
flameDown mapa _ _ 0 = mapa
flameDown mapa x y n
  | ((mapa !! y) !! x) == '#' = mapa
  | ((mapa !! y) !! x) == '?' =
    (take y mapa) ++
      [(take x (mapa !! y)) ++ " " ++ (drop (x + 1) (mapa !! y))] ++
        (drop (y + 1) mapa)
  | isTherePower mapa x y = removePower mapa x y
  | isTherePlayer mapa x y =
    (flameDown (removePlayer mapa x y) x y n)
  | isThereBomb mapa x y =
    (flameDown (blowTimer mapa x y) x (y + 1) (n - 1))
  | otherwise = flameDown mapa x (y + 1) (n - 1)

flameRight :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
flameRight mapa _ _ 0 = mapa
flameRight mapa x y n
  | ((mapa !! y) !! x) == '#' = mapa
  | ((mapa !! y) !! x) == '?' =
    (take y mapa) ++
      [(take x (mapa !! y)) ++ " " ++ (drop (x + 1) (mapa !! y))] ++
        (drop (y + 1) mapa)
  | isTherePower mapa x y = removePower mapa x y
  | isTherePlayer mapa x y =
    (flameRight (removePlayer mapa x y) x y n)
  | isThereBomb mapa x y =
    (flameRight (blowTimer mapa x y) (x + 1) y (n - 1))
  | otherwise = flameRight mapa (x + 1) y (n - 1)

flameLeft :: Mapa -> Abcissa -> Ordenada -> Range -> Mapa
flameLeft mapa _ _ 0 = mapa
flameLeft mapa x y n
  | ((mapa !! y) !! x) == '#' = mapa
  | ((mapa !! y) !! x) == '?' =
    (take y mapa) ++
      [(take x (mapa !! y)) ++ " " ++ (drop (x + 1) (mapa !! y))] ++
        (drop (y + 1) mapa)
  | isTherePower mapa x y = removePower mapa x y
  | isTherePlayer mapa x y =
    (flameLeft (removePlayer mapa x y) x y n)
  | isThereBomb mapa x y =
    (flameLeft (blowTimer mapa x y) (x - 1) y (n - 1))
  | otherwise = flameLeft mapa (x - 1) y (n - 1)

espiral :: Mapa -> Tempo -> Mapa
espiral mapa n = eraseObj (placeRock mapa n) n

cordLeft :: Cords -> Abcissa -> [Cords]
cordLeft c 0 = [c]
cordLeft (a, b) n = (a, b) : cordLeft (a - 1, b) (n - 1)

cordRight :: Cords -> Abcissa -> [Cords]
cordRight c 0 = [c]
cordRight (a, b) n = (a, b) : cordRight (a + 1, b) (n - 1)

cordUp :: Cords -> Ordenada -> [Cords]
cordUp c 0 = []
cordUp (a, b) n = (a, b) : cordUp (a, b - 1) (n - 1)

cordDown :: Cords -> Ordenada -> [Cords]
cordDown c 0 = [c]
cordDown (a, b) n = (a, b) : cordDown (a, b + 1) (n - 1)

quadrado :: Cords -> Int -> [Cords]
quadrado (a, b) d = l1 ++ l2 ++ l3 ++ l4
  where l1 = (cordRight (a, b) (d - 1))
        l2 = tail (cordDown (last l1) (d - 1))
        l3 = tail (cordLeft (last l2) (d - 1))
        l4 = tail (cordUp (last l3) (d - 1))

listaSpiral :: Cords -> Dim -> [Cords]
listaSpiral (a, b) 1 = [(a, b)]
listaSpiral (a, b) d = q ++ listaSpiral (a + 1, b + 1) (d - 2)
  where q = (quadrado (a, b) d)

placeRock :: Mapa -> Tempo -> Mapa
placeRock mapa n
  = (take y mapa) ++
      [(take x (mapa !! y)) ++ "#" ++ (drop (x + 1) (mapa !! y))] ++
        (drop (y + 1) mapa)
  where (x, y) = (listaSpiral (1, 1) d) !! (d * d - n)
        d = length (mapa !! 0) - 2

eraseObj :: Mapa -> Tempo -> Mapa
eraseObj mapa n
  = removePower (removePlayer (removeBomb mapa x y) x y) x y
  where (x, y) = (listaSpiral (1, 1) d) !! (d * d - n)
        d = length (mapa !! 0) - 2

removePower :: Mapa -> Abcissa -> Ordenada -> Mapa
removePower [] _ _ = []
removePower (h : t) x y
  = if elem (h !! 0) "+!" && pAbcis h == x && pOrden h == y then t
      else h : removePower t x y

removePlayer :: Mapa -> Abcissa -> Ordenada -> Mapa
removePlayer [] _ _ = []
removePlayer (h : t) x y
  = if elem (h !! 0) "0123" && pAbcis h == x && pOrden h == y then t
      else h : removePlayer t x y

blowTimer :: Mapa -> Abcissa -> Ordenada -> Mapa
blowTimer [] _ _ = []
blowTimer (h : t) x y
  = if (h !! 0) == '*' && pAbcis h == x && pOrden h == y then
      (stringTimer h) : t else h : blowTimer t x y

stringTimer :: Linha -> Linha
stringTimer st
  = if last st == '0' then st else (take (length st - 1) st) ++ "1"

removeBomb :: Mapa -> Abcissa -> Ordenada -> Mapa
removeBomb [] _ _ = []
removeBomb (h : t) x y
  = if (h !! 0) == '*' && pAbcis h == x && pOrden h == y then t else
      h : removeBomb t x y

isTherePower :: Mapa -> Abcissa -> Ordenada -> Bool
isTherePower [] _ _ = False
isTherePower (h : t) x y
  = if elem (h !! 0) "+!" && pAbcis h == x && pOrden h == y then True
      else isTherePower t x y

isTherePlayer :: Mapa -> Abcissa -> Ordenada -> Bool
isTherePlayer [] _ _ = False
isTherePlayer (h : t) x y
  = if elem (h !! 0) "0123" && pAbcis h == x && pOrden h == y then
      True else isTherePlayer t x y

isThereBomb :: Mapa -> Abcissa -> Ordenada -> Bool
isThereBomb [] _ _ = False
isThereBomb (h : t) x y
  = if (h !! 0) == '*' && pAbcis h == x && pOrden h == y then True
      else isThereBomb t x y

pOrden :: Linha -> Ordenada
pOrden st = read (pOrdenSt st)

pAbcis :: Linha -> Abcissa
pAbcis st = read (pAbcisSt st)

pAbcisSt :: Linha -> String
pAbcisSt st
  | st !! 3 == ' ' = (st !! 2) : ""
  | st !! 4 == ' ' = take 2 (drop 2 st)
  | otherwise = take 3 (drop 2 st)

pOrdenSt :: Linha -> String
pOrdenSt st
  | last st /= ' ' = pOrdenSt (st ++ " ")
  | pAbcis st >= 100 =
    if st !! 7 == ' ' then (st !! 6) : "" else
      if st !! 8 == ' ' then take 2 (drop 6 st) else take 3 (drop 6 st)
  | pAbcis st >= 10 =
    if st !! 6 == ' ' then (st !! 5) : "" else
      if st !! 7 == ' ' then take 2 (drop 5 st) else take 3 (drop 5 st)
  | otherwise =
    if st !! 5 == ' ' then (st !! 4) : "" else
      if st !! 6 == ' ' then take 2 (drop 4 st) else take 3 (drop 4 st)

range :: Linha -> Int
range string
  = 1 +
      read
        (fst
           (span (isDigit)
              (drop (6 + (length (pAbcisSt string)) + (length (pOrdenSt string)))
                 string)))

cordsT :: Linha -> (Int, Int)
cordsT st = (pAbcis st, pOrden st)
itera_avanca_main mapa start end
  | start < end = mapa
  | otherwise =
    itera_avanca_main (avanca mapa start) (pred start) end
main
  = do a <- getArgs
       let start = readMaybe (a !! 0)
       let end = readMaybe (a !! 1)
       w <- getContents
       if isJust start && isJust end then
         putStr $
           unlines $
             itera_avanca_main (lines w) (fromJust start) (fromJust end)
         else putStrLn "Par\226metros inv\225lidos"
