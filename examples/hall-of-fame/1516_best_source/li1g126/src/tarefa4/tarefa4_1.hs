module Main where

import Control.Monad (liftM)
import System.Console.Haskeline
import System.IO
import System.Process
import System.Posix.Process

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c == 'q'
    then do return ()
    else do addcmd c
            hSetEcho stdin True
            callCommand "clear"
            callCommand "./tarefa4_modified < tab1.in"
            main
   where addcmd c
           | c == 'w' || c =='W' = appendFile "tab1.in" "U"
           | c == 'a' || c =='A' = appendFile "tab1.in" "L"
           | c == 's' || c =='S' = appendFile "tab1.in" "D"
           | c == 'd' || c =='D' = appendFile "tab1.in" "R"
           | otherwise           = appendFile "tab1.in" ""