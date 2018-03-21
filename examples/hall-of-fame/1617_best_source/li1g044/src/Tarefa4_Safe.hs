module Tarefa4 where
import Bomberman
import Data.Char
import Data.List
import System.Environment
import Text.Read
import Data.Maybe

avanca :: [String] -> Int -> [String]
avanca s t
  = dataEstadoToString $avancaAux (stringToDataEstado s 0) t

main :: IO ()
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