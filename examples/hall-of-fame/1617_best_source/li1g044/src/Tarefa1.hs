{-|
Module      : Main
Description : Módulo Haskell que cria um mapa de jogo
Copyright   : Miguel Brandão <a82349@alunos.uminho.pt>;
              Vítor Gomes <a75362@alunos.uminho.pt>
Um módulo que recebendo um tamanho de mapa ímpar e maior que 5 e uma seed, gera um mapa com tijolos, espaços vazios e power ups aleatórios
-}
module Tarefa1 where

import Bomberman
import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import System.Random



-- | Esta função que recebe m tamanho @n@ e uma seed @s@ e devolve um mapa com tamanho @n@
mapa :: Int -- ^ tamanho do mapa
        -> Int -- ^ número da semente
        -> [String] -- ^ mapa que será gerado
mapa n s = mapaaux n s



main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"

