---------------------------------------------------------------------
------            ________  __ ______  ___  ___   _  __       -------
------           / __/ __ \/ //_/ __ \/ _ )/ _ | / |/ /       -------
------          _\ \/ /_/ / ,< / /_/ / _  / __ |/    /        -------
------         /___/\____/_/|_|\____/____/_/ |_/_/|_/         -------
------                                                        -------
---------------------------------------------------------------------
-------------------          TAREFA 6             ------------------- 
---------------------------------------------------------------------
------------       A77789          MIGUEL MAGALHAES      ------------
------------       A78565          HUGO OLIVEIRA         ------------
---------------------------------------------------------------------
{-|
Module : Game
Description : Módulo para a parte gráfica do jogo /Sokoban./
Copyright : Miguel Magalhães <a77789@alunos.uminho.pt>;
            Hugo Oliveira <a78565@alunos.uminho.pt>;

Módulo para realização da interface gráfica do jogo. Para isso utilizámos a ferramenta
Gloss. Esta tarefa funciona quase como um tradutor para "Bitmaps" e permite a visualização 
do jogo.

-}

module Main where

import Data.Char
import qualified Data.Text as T
import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import System.Process (callCommand,
                       runCommand)
import System.Directory (getDirectoryContents,
                         doesFileExist,
                         renameFile,
                         removeFile)

-- | Lista com o tabuleiro.
type Tabuleiro  = [String]

-- | Lista com as coordenadas.
type Coords     = [String]

-- | Posições (x,y) do boneco.
type Posicao    = (Int,Int)

-- | Lista com todos os bitmaps.
type Bitmaps        = [Picture]

-- | Tabuleiro numa string apenas.
type NivelMapa      = String

-- | String com os comandos, ex: ULRD.
type Comandos       = String

-- | Apenas um comando, ex: U.
type ComandoSimples = Char

-- | Existência de uma bomba ou não.
type Bomba          = Bool

-- | Contador do tipo Float.
type Contador       = Float

-- | Contador do tipo Int.
type ContadorInt    = Int

-- | Número de um nível, ex: ”3”.
type Level          = String

-- | Nome de um tema, ex: ”Lego”.
type Tema           = String

-- | Melhor pontuação obtida no jogo.
type Highscore      = String

-- | Base do jogo.
type Mapa = (Bitmaps,NivelMapa,Comandos,Bomba)



---------------------------------------------------------------------
-- * __/Input/__
---------------------------------------------------------------------

-- | Função /Main/.
main :: IO ()
main = do updateSettings
          pu  <- loadBMP (theme 'a' ++ "/playerU.bmp")
          pd  <- loadBMP (theme 'a' ++ "/playerD.bmp")
          pl  <- loadBMP (theme 'a' ++ "/playerL.bmp")
          pr  <- loadBMP (theme 'a' ++ "/playerR.bmp")
          w   <- loadBMP (theme 'a' ++ "/wall.bmp")
          s   <- loadBMP (theme 'a' ++ "/storage.bmp")
          f   <- loadBMP (theme 'a' ++ "/neutral.bmp")
          b   <- loadBMP (theme 'a' ++ "/box.bmp")
          bs  <- loadBMP (theme 'a' ++ "/boxstored.bmp")
          psu <- loadBMP (theme 'a' ++ "/playerstoredU.bmp")
          psd <- loadBMP (theme 'a' ++ "/playerstoredD.bmp")
          psl <- loadBMP (theme 'a' ++ "/playerstoredL.bmp")
          psr <- loadBMP (theme 'a' ++ "/playerstoredR.bmp")
          bomb<- loadBMP "content/bitmaps/bomb.bmp"
          logo<- loadBMP "content/bitmaps/logo.bmp"
          runCommand "say -v Daniel Welcome to Sokoban!"
          if (read (lvl 'a') :: Int) >= (read (maxLevel) :: Int)
            then resetLvl 'a' 
            else callCommand ""
          writeFile "content/levels/cmds.in" "" -- limpa os comandos
          callCommand ("./move < " ++ level 'a' ++ " > content/levels/tab.temp") -- gera um tabuleiro para mostrar
          tab <- readFile "content/levels/tab.temp"
          cmd <- readFile "content/levels/cmds.in"
          if unsafePerformIO (doesFileExist ("content/levels/bombs" ++ lvl 'a' ++ ".bomb"))
            then joga ([pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo],tab,cmd,True) desenhaMapa regEv
            else joga ([pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo],tab,cmd,False) desenhaMapa regEv


-- pN   = player (n -> oritentação)
-- w   = wall
-- s   = storage     
-- f   = floor / neutral     
-- b   = box         
-- bs  = boxstored   
-- psN  = playerstored (n -> oritentação)
-- tab = tabuleiro


-- | Função fornecida.
joga :: world -> (world -> Picture) -> (Event -> world -> world) -> IO ()
joga mapainicial desenha reage =
  let window = (InWindow "Sokoban" (700, 500) (100, 100)) in
  play
    window
    (greyN 0.8) -- Côr do fundo da janela
    45
    mapainicial
    desenha
    reage
    reageTempo


---------------------------------------------------------------------
-- * __Interface do Jogo Sokoban__
---------------------------------------------------------------------

-- | Função que permite desenhar a interface do jogo. Consiste assim na parte visual.
desenhaMapa :: Mapa -> Picture
desenhaMapa (a,tab,cmd,n) = Pictures [border,bground,tabuleiro,tabela,title,bottombar,lastcmds,hidecmds]
 where hidecmds  = Pictures [Color white (Polygon [(350,-252),(350,252),(352,252),(352,-252)]),
                             Color (greyN 0.8) (Polygon [(352,-2500),(352,2500),(3500,2500),(3500,-2500)])]
       border    = Color white (Polygon [(-352,-252),(-352,252),(352,252),(352,-252)])
       bground   = Color (greyN 0.85) (Polygon [(-350,-250),(-350,250),(350,250),(350,-250)])
       transY    = (fromIntegral (length (lines tab)) * (-10)) - 50
       transX    = (fromIntegral (length (head (lines tab))) * (-10)) - 125
       tabuleiro = if cmd /= [] 
                    then Translate transX transY (mapshow (lines tab) a cmd) 
                    else Translate transX transY (mapshow (lines tab) a "U")
       tabela    = Translate 190 0 (Pictures [moves,bombs,separator,end,leveln,instruct,topscore])
       moves     = if lvl 'a' /= maxLevel 
                    then Translate 0 (40) (Scale 0.2 0.2 (Text ("Moves: " ++ show (length $ rmBombListMoves cmd))))
                    else Blank
       bombs     = if lvl 'a' /= maxLevel
                    then if n == True 
                          then Translate 22 10 (Scale 0.13 0.13 (Text ("Bombs: 1"))) 
                          else Translate 22 10 (Scale 0.13 0.13 (Text ("Bombs: 0")))
                    else Blank
       separator = Translate (-30) 0 (Color white (Polygon [(0,-250),(0,250),(2,250),(2,-250)]))
       topscore  = if lvl 'a' /= maxLevel 
                    then (Translate (-10) 100 (Scale 0.15 0.15 (Text ("Highscore: " ++ currentHighscore 'a')))) 
                    else Blank
       title     = Translate (-100) 170 (last a)
       leveln    = if lvl 'a' /= maxLevel 
                    then (Translate (-15) 170 (Scale 0.3 0.3 (Text ("Level " ++ lvl 'a')))) 
                    else (Translate (-10) 170 (Scale 0.25 0.25 (Text "THE END!")))
       bottombar = Pictures [Color white $ Line [(-350,-230),(350,-230)],
                             Color (makeColor 138 138 138 255) $ Polygon ([(-350,-250),(-350,-230),(350,-230),(350,-250)])]
       lastcmds  = if lvl 'a' /= maxLevel 
                    then (Translate (-345) (-245) (Scale 0.1 0.1 $ Pictures [Text "Last commands:", Translate 1100 0 (Text (reverse cmd))])) 
                    else (Translate (-345) (-245) (Scale 0.1 0.1 $ Text "Thank you for playing!")) 
       end       = if endgame tab
                    then if lvl 'a' == maxLevel
                          then Translate (-15) 0 (Scale 0.18 0.18 $ Text "Congratulations!!")
                          else Translate 20 (-40) (Scale 0.2 0.2 (Pictures [Text "LEVEL", Translate (-50) (-130) (Text "PASSED")]))
                    else Blank
       instruct  = Translate 0 (-120) (Scale 0.12 0.12 (Pictures [ Translate 0 130 (Text "COMMANDS"),
                                                                   Translate 0 0 (Text "b: bomb"),
                                                                   Translate 0 (-130) (Text "u: undo"),
                                                                   Translate 0 (-260) (Text "r: restart level"),
                                                                   Translate 0 (-390) (Text "n: next level"),
                                                                   Translate 0 (-520) (Text "p: previous level"),
                                                                   Translate 0 (-650) (Text "t: change theme"),
                                                                   Translate 0 (-780) (Text "ESC: close window")
                                                                  ]))

---------------------------------------------------------------------
-- ** __Mapa__
---------------------------------------------------------------------
     
-- | Função que permite após receber um tabuleiro de jogo, convertê-lo
--   num conjunto de pictures.
mapshow :: Tabuleiro -> Bitmaps -> Comandos -> Picture
mapshow tab bmps cmd = scale 1.2 1.2 (Pictures [Pictures (translateLines (mapshowaux (reverse tab) bmps cmd) 0),
                                                if elem 'B' cmd 
                                                   then Pictures (translateLines (bombshow (reverse tab) bmps cmd) 0) 
                                                   else Blank])

-- | Após todo o tabuleiro ser transcrito para Bitmaps,
--   posiciona-se linha a linha o mapa.
translateLines :: [Picture] -> Contador -> [Picture]
translateLines [] _ = []
translateLines (h:t) r = ((Translate (0) (r*20) h) : (translateLines t (r+1)))

-- | Converte o mapa para bitmaps correspondentes a cada símbolo.
mapshowaux :: Tabuleiro -> Bitmaps -> Comandos -> [Picture]
mapshowaux [] _ _ = []
mapshowaux (h:t) bmps cmd = (Pictures (assignbmp 0 bmps h cmd) : (mapshowaux t bmps cmd)) 

-- | Em caso da bomba ser ativa mostra-a no tabuleiro.
bombshow :: Tabuleiro -> Bitmaps -> Comandos -> [Picture]
bombshow [] _ _ = []
bombshow (h:t) bmps cmd = (Pictures (placeBomb 0 bmps h cmd) : (bombshow t bmps cmd)) 

-- | Traduz linhas do tabuleiro para uma lista de picture com os bitmaps correspondentes. Ficam 
--   automaticamente posicionados relativamente ao eixo Ox.
assignbmp :: Contador -> Bitmaps -> NivelMapa -> Comandos -> [Picture]
assignbmp _ _ [] _ = []
assignbmp r bmps@[pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] (h:t) cmd
    | h == 'o' && last cmd == 'U' = ((Translate (r*20) (0) pu ):(assignbmp (r+1) bmps t cmd))
    | h == 'o' && last cmd == 'D' = ((Translate (r*20) (0) pd ):(assignbmp (r+1) bmps t cmd))
    | h == 'o' && last cmd == 'L' = ((Translate (r*20) (0) pl ):(assignbmp (r+1) bmps t cmd))
    | h == 'o' && last cmd == 'R' = ((Translate (r*20) (0) pr ):(assignbmp (r+1) bmps t cmd))
    | h == 'O' && last cmd == 'U' = ((Translate (r*20) (0) psu):(assignbmp (r+1) bmps t cmd))
    | h == 'O' && last cmd == 'D' = ((Translate (r*20) (0) psd):(assignbmp (r+1) bmps t cmd))
    | h == 'O' && last cmd == 'L' = ((Translate (r*20) (0) psl):(assignbmp (r+1) bmps t cmd))
    | h == 'O' && last cmd == 'R' = ((Translate (r*20) (0) psr):(assignbmp (r+1) bmps t cmd))
    | h == '#' = ((Translate (r*20) (0) w ):(assignbmp (r+1) bmps t cmd))
    | h == '.' = ((Translate (r*20) (0) s ):(assignbmp (r+1) bmps t cmd))
    | h == ' ' = ((Translate (r*20) (0) f ):(assignbmp (r+1) bmps t cmd))
    | h == 'H' = ((Translate (r*20) (0) b ):(assignbmp (r+1) bmps t cmd))
    | h == 'I' = ((Translate (r*20) (0) bs):(assignbmp (r+1) bmps t cmd))
    | h == '@' = (assignbmp (r+1) [pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] t cmd)
    | last cmd == 'B' = (assignbmp (r+1) [pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] t cmd)


-- | Coloca a bomba em cima do boneco.
placeBomb :: Contador -> Bitmaps -> NivelMapa -> Comandos -> [Picture]
placeBomb _ _ [] _ = []
placeBomb r [pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] (h:t) cmd
    | (h == 'o' || h == 'O') && last cmd == 'B' = ((Translate (r*20) (0) bomb ):(placeBomb (r+1) [pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] t cmd))
    | otherwise = (placeBomb (r+1) [pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo] t cmd)




---------------------------------------------------------------------
-- ** __Ligação entre a interface gráfica e a movimentação__
---------------------------------------------------------------------

-- | Converte o /Output/ do 'reageEvento' de IO Mapa para Mapa.
regEv :: Event -> Mapa -> Mapa
regEv a b = unsafePerformIO (reageEvento a b)

-- | Fornece à função 'makeMov' o comando correspondente à movimentação.
reageEvento :: Event -> Mapa -> IO Mapa
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) m    = makeMov m 'U'
reageEvento (EventKey (Char 'w') Down _ _) m            = makeMov m 'U'
reageEvento (EventKey (Char 'W') Down _ _) m            = makeMov m 'U'
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) m  = makeMov m 'D'
reageEvento (EventKey (Char 's') Down _ _) m            = makeMov m 'D'
reageEvento (EventKey (Char 'S') Down _ _) m            = makeMov m 'D'
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) m  = makeMov m 'L'
reageEvento (EventKey (Char 'a') Down _ _) m            = makeMov m 'L'
reageEvento (EventKey (Char 'A') Down _ _) m            = makeMov m 'L'
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) m = makeMov m 'R'
reageEvento (EventKey (Char 'd') Down _ _) m            = makeMov m 'R'
reageEvento (EventKey (Char 'D') Down _ _) m            = makeMov m 'R'
reageEvento (EventKey (Char 'b') Down _ _) m            = makeMov m 'B'
reageEvento (EventKey (Char 'B') Down _ _) m            = makeMov m 'B'
reageEvento (EventKey (Char 'r') Down _ _) m            = restart m 0
reageEvento (EventKey (Char 'R') Down _ _) m            = restart m 0
reageEvento (EventKey (Char 'u') Down _ _) m            = undo m 
reageEvento (EventKey (Char 'U') Down _ _) m            = undo m
reageEvento (EventKey (Char 'n') Down _ _) m            = nextLvl m
reageEvento (EventKey (Char 'N') Down _ _) m            = nextLvl m
reageEvento (EventKey (Char 'p') Down _ _) m            = prevLvl m
reageEvento (EventKey (Char 'P') Down _ _) m            = prevLvl m
reageEvento (EventKey (Char 't') Down _ _) m            = nextTheme m
reageEvento (EventKey (Char 'T') Down _ _) m            = nextTheme m
reageEvento _ m                                         = return m

-- | Permite ser vísivel a movimentação do Sokoban.
makeMov :: Mapa -> ComandoSimples -> IO Mapa
makeMov (bmp,tab,cmds,False) 'B' = return (bmp,tab,cmds,False)
makeMov (bmp,tab,cmds,n) c = do appendFile "content/levels/cmds.in" [c]
                                callCommand ("./move < " ++ level 'a' ++ " > content/levels/tab.temp")
                                tabn <- readFile "content/levels/tab.temp"
                                if tab == tabn
                                  then return (bmp,tabn,cmds,n)  
                                  else do if endgame tabn
                                            then highscore ((length $ rmBombListMoves cmds) + 1)
                                            else return ()
                                          if c /= 'B' 
                                            then do runCommand "afplay content/sounds/move.aiff"
                                                    return (bmp,tabn,(cmds++[c]),n) 
                                            else do runCommand "afplay content/sounds/bomb.mp3"
                                                    return (bmp,tabn,(cmds++[c]),False) 
                                          
-- | Permite ao jogador a funcionalidade de recomeçar a jogar.
restart :: Mapa -> Contador -> IO Mapa
restart (bmp,tab,cmds,n) r = do if r == 0 
                                  then runCommand "say -v Daniel Restarting!" 
                                  else if lvl 'a' == maxLevel
                                          then runCommand "say -v Daniel Congratulations! You have completed the game!"
                                          else if (read (lvl 'a') :: Int) >= (read (maxLevel) :: Int)
                                                then runCommand ""
                                                else runCommand ("say -v Daniel Level " ++ lvl 'a')
                                removeFile "content/levels/cmds.in"
                                writeFile "content/levels/cmds.in" ""
                                callCommand ("./move < " ++ level 'a' ++ " > content/levels/tab.temp")
                                tabn <- readFile "content/levels/tab.temp"
                                if unsafePerformIO (doesFileExist ("content/levels/bombs" ++ lvl 'a' ++ ".bomb"))
                                  then return (bmp,tabn,[],True)
                                  else return (bmp,tabn,[],False)

-- | Permite ao jogador a funcionalidade de voltar à jogada anterior.                            
undo :: Mapa -> IO Mapa
undo (bmp,tab,[],n) = return (bmp,tab,[],n)
undo (bmp,tab,cmds,n) = do writeFile "content/levels/cmds.in" (init cmds) 
                           callCommand ("./move < " ++ level 'a' ++ " > content/levels/tab.temp") 
                           tabn <- readFile "content/levels/tab.temp"
                           runCommand "afplay content/sounds/move.aiff"
                           if last cmds == 'B'
                             then return (bmp,tabn,(init cmds),True)
                             else return (bmp,tabn,(init cmds),n)



---------------------------------------------------------------------
-- ** __Mudar de níveis__
---------------------------------------------------------------------

-- | Função que permite ao jogador a hipótese de seguir para o próximo nível.
nextLvl :: Mapa -> IO Mapa
nextLvl m = if (read (lvl 'a') :: Int) >= (read (maxLevel) :: Int)
              then exitSuccess
              else do writeLvl 'n'
                      restart m 1

-- | Função que permite ao jogador a hipótese de recuar um nível.
prevLvl :: Mapa -> IO Mapa
prevLvl m = if lvl 'a' == "1"
              then return m
              else do writeLvl 'p'
                      restart m 1


--------------------------------------------------------------------
-- ** __Mudar de temas__
---------------------------------------------------------------------

-- | Função que permite ao jogador a hipótese de mudar de tema. No caso de alterar
--   de tema, todos os Bitmaps são substituídos.
nextTheme :: Mapa -> IO Mapa
nextTheme (bmp,tab,cmds,n) = do newTheme 'a'
                                pu  <- loadBMP (theme 'a' ++ "/playerU.bmp")
                                pd  <- loadBMP (theme 'a' ++ "/playerD.bmp")
                                pl  <- loadBMP (theme 'a' ++ "/playerL.bmp")
                                pr  <- loadBMP (theme 'a' ++ "/playerR.bmp")
                                w   <- loadBMP (theme 'a' ++ "/wall.bmp")
                                s   <- loadBMP (theme 'a' ++ "/storage.bmp")
                                f   <- loadBMP (theme 'a' ++ "/neutral.bmp")
                                b   <- loadBMP (theme 'a' ++ "/box.bmp")
                                bs  <- loadBMP (theme 'a' ++ "/boxstored.bmp")
                                psu <- loadBMP (theme 'a' ++ "/playerstoredU.bmp")
                                psd <- loadBMP (theme 'a' ++ "/playerstoredD.bmp")
                                psl <- loadBMP (theme 'a' ++ "/playerstoredL.bmp")
                                psr <- loadBMP (theme 'a' ++ "/playerstoredR.bmp")
                                bomb<- loadBMP "content/bitmaps/bomb.bmp"
                                logo<- loadBMP "content/bitmaps/logo.bmp"
                                return ([pu,pd,pl,pr,w,s,f,b,bs,psu,psd,psl,psr,bomb,logo],tab,cmds,n)

-- | Função que reage com o tempo.
reageTempo :: Float -> world -> world
reageTempo t m = m


--------------------------------------------------------------------
-- ** __Ficheiro /Settings/__
---------------------------------------------------------------------

-- | Para uma melhor organização, o ficheiro /settings.txt/ contém toda a informação
--   relativa aos níveis e temas. Sempre que estes dois são alterados a função 'settings' altera o ficheiro
--   /settings.txt/.
settings :: t -> [String]
settings a = unsafePerformIO $ settingsFile a
settingsFile a = do inp <- readFile "content/settings.txt"
                    return (lines inp)


-- | Função quer permite identificar o número máximo de níveis presentes na pasta //content//levels.
maxLevel :: Level
maxLevel = show ((read lastLevel :: Int) + 1)
  where lastLevel = drop 3 $ takeWhile (/='.') (last $ unsafePerformIO (getDirectoryContents "content/levels"))

-- | Função que fornece o /current level/.
lvl :: t -> Level
lvl a = ((words $ settings a !! 1) !! 1)

-- | Fornece o path para o nível.
level :: t -> Level
level a = if lvl a == maxLevel
          then "content/levels/end.in"
          else ("content/levels/tab" ++ lvl a ++ ".in")

-- | Permite modificar no ficheiro /settings.txt/ o nível que o jogador está a jogar.
writeLvl :: Char -> IO ()
writeLvl a = do writeFile "content/settings2.txt" (unlines $ changelvl a)
                renameFile "content/settings2.txt" "content/settings.txt"

-- | Se o jogador alterar de nível, esta função altera o level no ficheiro /settings.txt/.
changelvl :: Char -> [Level]
changelvl a = (head $ settings a):(head (words $ settings a !! 1) ++ " " ++ upLevel ((words $ (settings a) !! 1) !! 1) a)
              :(tail $ tail $ settings a)
    where upLevel a 'n' = show ((read a :: Int) + 1)
          upLevel a 'p' = show ((read a :: Int) - 1)

-- | Função que altera o level no ficheiro /settings.txt/ para 1.
resetLvl :: t -> IO ()
resetLvl a = do writeFile "content/settings2.txt" (unlines $ reset a)
                renameFile "content/settings2.txt" "content/settings.txt"
    where reset :: t -> [String]        
          reset a = (head $ settings a):(head (words $ settings a !! 1) ++ " " ++ "1")
                    :(drop 2 $ settings a)


-- | Função que lê qual o tema a ser usado.
theme :: t -> Tema
theme a = ("content/bitmaps/themes/" ++ themeName a)

-- | Função que fornece o tema a ser usado no jogo.
themeName :: t -> Tema
themeName a = ((words $ settings a !! 2) !! 1)

-- | Em caso de o jogador alterar o tema, este modificado no ficheiro  /settings.txt/.
newTheme :: t -> IO ()
newTheme a = do writeFile "content/settings2.txt" (unlines $ changeTheme a 0)
                renameFile "content/settings2.txt" "content/settings.txt"

-- | Permite alterar o tema do jogo.
changeTheme :: t -> ContadorInt -> [Tema]
changeTheme a n
    | n == (length allThemes - 1) = changeToTheme $ allThemes !! 0
    | themeName a == allThemes !! n = changeToTheme $ allThemes !! (n+1)
    | otherwise = changeTheme a (n+1)
    where changeToTheme b = (take 2 $ settings b)
                            ++[head (words $ settings b !! 2) ++ " " ++ b]
                            ++(drop 3 $ settings b)


-- | Função que permite atualizar o ficheiro /settings.txt/.
updateSettings :: IO ()
updateSettings = do updateThemes
                    updateLevels

-- | Escreve todos os temas disponiveis no ficheiro /settings.txt/.
updateThemes :: IO ()
updateThemes = do writeFile "content/settings2.txt" (unlines $ listThemes)
                  renameFile "content/settings2.txt" "content/settings.txt"
    where listThemes :: [Tema]
          listThemes = (init $ settings 'a') ++ [head (words $ settings 'a' !! 5) ++ " " ++ show allThemes]

-- | Fornece uma lista com todos os temas presentes do diretório content//bitmaps//themes.
allThemes :: [Tema]
allThemes = drop 3 $ unsafePerformIO (getDirectoryContents "content/bitmaps/themes/")


-- | Escreve todos os níveis disponiveis no ficheiro /settings.txt/.
updateLevels :: IO ()
updateLevels = do writeFile "content/settings2.txt" (unlines $ listLevels)
                  renameFile "content/settings2.txt" "content/settings.txt"

-- | Fornece uma lista com todos os níveis disponíveis.
listLevels :: [Level]
listLevels = (take 4 $ settings 'a') ++ [head (words $ settings 'a' !! 4) ++ " [1.." ++ show ((read maxLevel :: Int) - 1) ++ "]"] ++ (drop 5 $ settings 'a')


--------------------------------------------------------------------
-- ** __/HighScore/__
---------------------------------------------------------------------

-- | Função que permite saber o highscore de uma determinado nível.
currentHighscore :: t -> Highscore
currentHighscore a
  | not $ unsafePerformIO (doesFileExist ("content/levels/highscore" ++ lvl a ++ ".sc")) = "-"
  | otherwise = unsafePerformIO $ scores a

-- | Compara e altera o ficheiro highscore do nível correspondente no caso de ser obtido
--   um melhor score.
highscore :: Int -> IO ()
highscore a
  | not $ unsafePerformIO (doesFileExist ("content/levels/highscore" ++ lvl a ++ ".sc")) 
  = writeFile ("content/levels/highscore" ++ lvl a ++ ".sc") (show a)
  | (read (unsafePerformIO $ scores a) :: Int) > a                                 
  = do writeFile "content/levels/highscore.sc" (show a)
       renameFile "content/levels/highscore.sc" ("content/levels/highscore" ++ lvl a ++ ".sc")
  | otherwise = return ()

-- | Permite a leitura do highscore do nível correspondente.
scores :: t -> IO Highscore
scores a = do inp <- readFile ("content/levels/highscore" ++ lvl a ++ ".sc")
              return inp

-- | Testa se o jogo terminou. Utiliza a função 'existemH' para testar.
endgame :: NivelMapa -> Bool
endgame tab = existemH tab == 0


-- | Verifica se o jogo terminou e, por isso, se todas as caixas estão nos locais de arrumação.
existemH :: String -> Int
existemH [] = 0
existemH (h:t)
    | h == 'H'  = 1 + existemH t
    | otherwise = existemH t
    

-- | Remove o movimento 'B' dos comandos, ou seja, usar a Bomba não conta como movimento.
rmBombListMoves :: String -> String
rmBombListMoves [] = []
rmBombListMoves (h:t)
    | h == 'B'  = rmBombListMoves t
    | otherwise = h : rmBombListMoves t

