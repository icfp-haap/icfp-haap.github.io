module Tarefa4 where
import Data.Char
import System.Environment
import Text.Read
import Data.Maybe

avanca :: [String] -> Int -> [String]
avanca m t
  = drawSpiral (next (listOfExplosion $ reduceTimeOfBombs m)) t

main :: IO ()

mapSize :: [String] -> Int
mapSize [] = 0
mapSize (h : t) = length h

radiusDir ::
          [String] -> (Int, Int) -> (Int, Int) -> Int -> Int -> Int
radiusDir m (x, y) (dx, dy) i r
  | i == r = 0
  | value == ' ' = 1 + radiusDir m (x, y) (dx, dy) (i + 1) r
  | value == '#' = 0
  | elem (x, y) (listOfPowerUps m) = 1
  | otherwise = 1
  where value = (m !! (y + dy * i)) !! (x + dx * i)

radius :: [String] -> (Int, Int) -> Int -> (Int, Int, Int, Int)
radius m (x, y) r = (rad right, rad left, rad down, rad up)
  where rad d = radiusDir m (x, y) d 1 (r + 1)
        right = (1, 0)
        left = (-1, 0)
        down = (0, 1)
        up = (0, -1)

reduceTimeOfBombs :: [String] -> [String]
reduceTimeOfBombs [] = []
reduceTimeOfBombs ((x : xs) : y)
  = if x == '*' then (unwords $ newBombLine) : reduceTimeOfBombs y
      else (x : xs) : reduceTimeOfBombs y
  where bombLine = words xs
        newBombLine
          = "*" : (init bombLine) ++ [show (read (last bombLine) - 1)]

drawAtPoint :: [[a]] -> (Int, Int) -> a -> [[a]]
drawAtPoint [] _ _ = []
drawAtPoint (l : ls) (x, 0) c
  = (take x l ++ [c] ++ drop (x + 1) l) : ls
drawAtPoint (l : ls) (x, y) c = l : drawAtPoint ls (x, y - 1) c

explosion ::
          (Int, Int) -> (Int, Int, Int, Int) -> [String] -> [String]
explosion (x, y) (0, 0, 0, 0) m
  = explodeBombs (x, y) $
      destroyBricks (x, y) $ killPlayers (x, y) 3 m
explosion (x, y) (r, l, d, u) m
  | r > 0 = explode (x + r, y) (r - 1, l, d, u)
  | l > 0 = explode (x - l, y) (r, l - 1, d, u)
  | d > 0 = explode (x, y + d) (r, l, d - 1, u)
  | u > 0 = explode (x, y - u) (r, l, d, u - 1)
  where explode a b
          = explodeBombs a $
              destroyBricks a $ killPlayers a 3 (explosion (x, y) b m)

getPositions :: Char -> [String] -> [(Int, Int)]
getPositions c [] = []
getPositions c ((x : xs) : y)
  | x == c =
    (read ((words xs) !! 0), read ((words xs) !! 1)) : getPositions x y
  | otherwise = getPositions c y

killPlayers :: (Int, Int) -> Int -> [String] -> [String]
killPlayers _ (-1) m = m
killPlayers (x, y) p m
  = if [(x, y)] == getPositions (intToDigit p) m then
      killPlayers (x, y) (p - 1) (removeLineOfPlayer p m) else
      killPlayers (x, y) (p - 1) m

removeLineOfPlayer :: Int -> [String] -> [String]
removeLineOfPlayer p ((x : xs) : y)
  = if show p == [x] then y else (x : xs) : removeLineOfPlayer p y

destroyBricks :: (Int, Int) -> [String] -> [String]
destroyBricks (x, y) m
  = if (m !! y) !! x == '?' then drawAtPoint m (x, y) ' ' else
      destroyPowerUps (x, y) m

destroyPowerUps :: (Int, Int) -> [String] -> [String]
destroyPowerUps _ [] = []
destroyPowerUps (x, y) ((h : hs) : t)
  = if b then t else (h : hs) : destroyPowerUps (x, y) t
  where a = words hs
        b = if h == '+' || h == '!' then
              a !! 0 == show x && a !! 1 == show y else False

destroyBombs :: (Int, Int) -> [String] -> [String]
destroyBombs _ [] = []
destroyBombs (x, y) ((h : hs) : t)
  = if b then t else (h : hs) : destroyPowerUps (x, y) t
  where a = words hs
        b = if h == '*' then a !! 0 == show x && a !! 1 == show y else
              False

explodeBombs :: (Int, Int) -> [String] -> [String]
explodeBombs _ [] = []
explodeBombs (x, y) ((h : hs) : t)
  = if b then
      unwords (["*"] ++ init a ++ ["1"]) : explodeBombs (x, y) t else
      (h : hs) : explodeBombs (x, y) t
  where a = words hs
        b = if h == '*' then a !! 0 == show x && a !! 1 == show y else
              False

listOfExplosion :: [String] -> ([(Int, Int, Int)], [String])
listOfExplosion [] = ([], [])
listOfExplosion ((h : hs) : t)
  = if b then ((read (a !! 0), read (a !! 1), read (a !! 3)) : c, d)
      else (c, (h : hs) : d)
  where a = words hs
        b = if h == '*' then last a == "0" else False
        (c, d) = listOfExplosion t

listOfPowerUps :: [String] -> [(Int, Int)]
listOfPowerUps [] = []
listOfPowerUps ((h : hs) : t)
  = if h == '+' || h == '!' then
      (read (a !! 0), read (a !! 1)) : listOfPowerUps t else
      listOfPowerUps t
  where a = words hs

startSpiralMap :: Int -> Int -> [[Bool]]
startSpiralMap 0 _ = []
startSpiralMap n s
  = if n == s || n == 1 then
      (replicate s False) : startSpiralMap (n - 1) s else
      (False : (replicate (s - 2) True) ++ [False]) :
        startSpiralMap (n - 1) s

spiral :: (Int, Int) -> [[Bool]] -> Int -> Char -> (Int, Int)
spiral (x, y) m n d
  | n == (size - 2) ^ 2 = (x, y)
  | d == 'R' =
    if empty (x + 1, y) then spiral (x + 1, y) s (n + 1) d else
      spiral (x, y) m n 'D'
  | d == 'D' =
    if empty (x, y + 1) then spiral (x, y + 1) s (n + 1) d else
      spiral (x, y) m n 'L'
  | d == 'L' =
    if empty (x - 1, y) then spiral (x - 1, y) s (n + 1) d else
      spiral (x, y) m n 'U'
  | d == 'U' =
    if empty (x, y - 1) then spiral (x, y - 1) s (n + 1) d else
      spiral (x, y) m n 'R'
  where empty (x, y) = (m !! y) !! x
        s = drawAtPoint m (x, y) False
        size = length $ head m

drawSpiral :: [String] -> Int -> [String]
drawSpiral m t
  = if t > (size - 2) ^ 2 then m else
      destroyBombs p $
        destroyPowerUps p $ killPlayers p 3 (drawAtPoint m p '#')
  where p = spiral (1, 1) (startSpiralMap size size) t 'R'
        size = length $ head m

next :: ([(Int, Int, Int)], [String]) -> [String]
next ([], m) = m
next (((x, y, t) : z), m)
  = next (z, (explosion (x, y) (radius m (x, y) t) m))
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