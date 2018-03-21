module Tarefa4 where

import Bomberman
import Data.Char
import Data.List
import System.Environment
import Text.Read
import Data.Maybe


-- | 'avanca' recebe como input o estado atual do jogo e o número de ticks que faltam para o jogo terminar e devolve o estado do jogo
avanca :: [String] -> Int -> [String]
avanca s t = dataEstadoToString$avancaAux (stringToDataEstado s 0) t



{- 'main' aceita como parâmetro o número de instantes de tempo que faltam para o jogo terminar,
fica à espera do estado do jogo no ​ stdin ​ e invoca a função avanca,
imprimindo o resultado no stdout . -}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
