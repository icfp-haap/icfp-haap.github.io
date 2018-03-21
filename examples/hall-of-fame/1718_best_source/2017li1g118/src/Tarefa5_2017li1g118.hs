{-|
Module      : Tarefa5_2017li1g118
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa3_2017li1g118
import Tarefa4_2017li1g118
import Tarefa6_2017li1g118
import Tarefa1_2017li1g118
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Mapas
import TestesT4
import DrawingLib
import VectorLib
import SliderLib
import Data.Fixed
{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}

data GameState = MainMenu (Int,[String]) SavedPictures (Mapa,Propriedades,Settings,(Jogo,Settings,Int))
                |PropertyMenu (Int,[(Slider,String)],[String]) SavedPictures (Mapa,Propriedades,Settings,(Jogo,Settings,Int))
                |MapSelectionMenu (Int,Int,Int,[Mapa],[String]) SavedPictures (Mapa,Propriedades,Settings,(Jogo,Settings,Int))
                |SettingsMenu (Int,[(Slider,String)],[String]) SavedPictures (Mapa,Propriedades,Settings,(Jogo,Settings,Int))
                |Game Jogo
                      [Acao]
                      (Float,(Float,Float)) -- (Escala, Valores de Translação)
                      ([[Picture]])   -- Imagens como o mapa e players
                      Settings     -- Multiplayer / No Bots
                      SavedPictures
                      Int
                      (Mapa,Propriedades,Settings)
                      [Tempo] 
                      deriving Show

type SavedPictures = [Picture]
type Settings = (Int,Int,Int)
{-|Resolução da janela de jogo.-}
res :: Float
res = 16/9
{-|Largura da janela de jogo-}
width :: Float
width = 1280
{-|Altura da janela de jogo.-}
height :: Float
height = width / res
{-|Escala da janela em relação à janela com largura 860.-}
baseScale :: Float
baseScale = width/860
{-|Os frames que o jogo calcula por segundo.-}
frameRate :: Int
frameRate = 300
{-|O tempo em segundos que demora a passar um frame.-}
frameStep :: Double
frameStep = 1/(fromIntegral frameRate)
{-|Propriedades iniciais do jogo.-}
changeProp :: Propriedades
changeProp  = terra 
{-|Mapa inicial do jogo.-}
changeMap :: Mapa
changeMap   = (mapas !! 0) !! 0
{-|Quantidade de nitro que os carros têm no inicio da corrida.-}
quantInicialNitro :: Double
quantInicialNitro = 5
{-|Mapas que estão disponivel para seleção no Menu de mapas.-}
mapasMapSelection :: [Mapa]
mapasMapSelection = (mapas !! 0) ++ (mapas !! 1) ++ (mapas !! 2) ++ (mapas !! 3)

{-|Janela de jogo-}
window :: Display
window = InWindow "Micro Machines" (round width, round height) (10,10)
{-|Background do jogo.-}
background :: Color
background = white
{-|Função que dá inicio ao jogo.-}
doPlay :: [Picture] -> IO()
doPlay sprites
  = play
    window
    background
    frameRate
    (setup sprites)
    draw
    event
    update
{-|Função que importa as imagens necessárias e inicia o jogo.-}
main :: IO ()
main
  = do lava0  <- loadBMP "sprites/lava0.bmp"
       carro0 <- loadBMP "sprites/carro0.bmp"
       carro1 <- loadBMP "sprites/carro1.bmp"
       carro2 <- loadBMP "sprites/carro2.bmp"
       carro3 <- loadBMP "sprites/carro3.bmp"
       nitro0 <- loadBMP "sprites/nitro0.bmp"
       doPlay [lava0,carro0,carro1,carro2,carro3,nitro0]

{-|Estado inicial do jogo.-}
setup :: [Picture] -> GameState
setup savedPictures
  = MainMenu (0,mainMenuOpts) savedPictures (changeMap,changeProp,(1,1,0),(jogoInicial,(1,1,0),0) )

{-|Função que transforma um Gamestate numa Picture para ser mostrada na Janela.-}
draw :: GameState -> Picture
draw (MainMenu (i,menuStr) _ _)
  = drawMainMenu i menuStr

draw (PropertyMenu (i,sliders,applyCancelString) _ _)
  = Pictures [slidersPic,optsPic]
  where
  slidersPic = drawSliders sliders (-width/4,height/3) (baseScale*400,baseScale*40) (baseScale*70) i
  optsPic = drawOpts applyCancelString (width/6,height/4) (baseScale*0.4) (baseScale*100) (i-6)

draw (MapSelectionMenu (x,i,j,maps,strin) a (selected,_,_,_))
  = Pictures [mapsImg,optsImg]
  where
  mapsImg = drawMapSelectionMenuMaps (x,i) (baseScale*250) maps selected
  optsImg = drawOptsMapSelect applyCancelString (width/6,height/4) (baseScale*0.4) (baseScale*100) (x,j)

draw (SettingsMenu (i,sliders,applyCancelString) _ _)
  = Pictures [slidersPic,optsPic,cPic]
  where
  slidersPic = drawSliders sliders (-width/4,height/3) (baseScale*400,baseScale*40) (baseScale*70) i
  optsPic = drawOpts applyCancelString (width/6,height/4) (baseScale*0.4) (baseScale*100) (i-3)
  cPic = Translate (-width/2.5) (-height/30) $ Scale (0.15*baseScale) (0.15*baseScale) $ Text controlos
  controlos = "0=WASD QERF 1=Keys JKLÇ"

draw (Game (Jogo mapa _ c nitTempos historicos) acs (scl,trs) imgs _ _ frame _ stuns)
  = Pictures [mapImg,carImg,playerNitroStatusPic,playerStatusPic,timePassedPic,stunsPic]
  where
  mapImg = Pictures [(mapAtFrame frame (imgs !! 0))]
  carImg = Pictures (carrosToPicture c (scl,trs) nitrosBool savedNitImg savedCarImgs)
  playerNitroStatusPic = playerNitroStatus nitTempos quantInicialNitro baseScale
  playerStatusPic = playerStatus c baseScale
  timePassedPic = timePassed frameStep frame baseScale
  savedCarImgs = (imgs !! 1)
  savedNitImg  = (imgs !! 2) !! 0
  nitrosBool = calcularNitrosAcoes acs nitTempos (replicate (length nitTempos) False)

  stunsPic = stunStatus stuns baseScale
  positionsPic = Scale 0.4 0.4 (Text (show lugares))
  positions = getIndexs c historicos (definePercurso mapa)
  lugares = getPosicoes positions

{-|Função que altera o estado do jogo de acordo com o Input do utilizador.-}
event :: Event -> GameState -> GameState
event (EventKey (SpecialKey sk) Down _ _) (MainMenu (i,opts)  a b)
  =case sk of
   KeyUp   -> (MainMenu (mLim (i-1),opts) a b)
   KeyDown -> (MainMenu (mLim (i+1),opts) a b)
   KeyEnter-> case i of
              0 -> resumeGame (MainMenu (i,opts)  a b)
              1 -> setupNewGame (MainMenu (i,opts)  a b)
              2 -> setupMapSelection (MainMenu (i,opts)  a b)
              3 -> toPropMenu (MainMenu (i,opts)  a b)
              4 -> toSettingsMenu (MainMenu (i,opts)  a b)
              5 -> error "Quit Game."
              otherwise -> (MainMenu (mLim i,opts) a b)
   otherwise -> (MainMenu (mLim i,opts) a b)
   where
   mLim = menuLimit (0,5)

event (EventKey (SpecialKey sk) Down _ _) (PropertyMenu (i,sliders,opts)  a b)
  =case sk of
   KeyUp   -> (PropertyMenu (mLim (i-1),sliders,opts)  a b)
   KeyDown -> (PropertyMenu (mLim (i+1),sliders,opts)  a b)
   KeyLeft -> mover Sleft
   KeyRight-> mover Sright
   KeyEnter-> if i<6 then (PropertyMenu (mLim i,sliders,opts)  a b)
              else if i == 6 then MainMenu (0,mainMenuOpts) a (mapa,nProps,opt,jogo)
                   else MainMenu (0,mainMenuOpts) a b

   otherwise->(PropertyMenu (i,sliders,opts)  a b)
   where
   mLim = menuLimit (0,7)
   nProps = menuToProps (PropertyMenu (i,sliders,opts)  a b)
   (mapa,props,opt,jogo) = b
   mover move = if i < 6 then
                (PropertyMenu (i,sMoveAt i move sliders,opts)  a b)
                else (PropertyMenu (i,sliders,opts)  a b)

event (EventKey (SpecialKey sk) Down _ _) (MapSelectionMenu (x,i,j,maps,strin) a b)
  =case (sk,x) of
   (KeyUp,0)    -> (MapSelectionMenu (x,limI (i-1),j,maps,strin) a b)
   (KeyUp,1)    -> (MapSelectionMenu (x,i,limJ (j-1),maps,strin) a b)
   (KeyDown,0)  -> (MapSelectionMenu (x,limI (i+1),j,maps,strin) a b)
   (KeyDown,1)  -> (MapSelectionMenu (x,i,limJ (j+1),maps,strin) a b)
   (KeyLeft,0)  -> (MapSelectionMenu (1,i,j,maps,strin) a b)
   (KeyLeft,1)  -> (MapSelectionMenu (0,i,j,maps,strin) a b)
   (KeyRight,0) -> (MapSelectionMenu (1,i,j,maps,strin) a b)
   (KeyRight,1) -> (MapSelectionMenu (0,i,j,maps,strin) a b)
   (KeyEnter,0) -> (MapSelectionMenu (x,i,j,maps,strin) a (maps !! i,props,opts,jogo))
   (KeyEnter,1) -> if j == 0 then MainMenu (0,mainMenuOpts) a (maps !! i,props,opts,jogo)
                   else MainMenu (0,mainMenuOpts) a b
   otherwise    -> (MapSelectionMenu (x,i,j,maps,strin) a b)
   where
   limI = menuLimit (0,length maps -1)
   limJ = menuLimit (0,1)
   (mapa,props,opts,jogo) = b

event (EventKey (SpecialKey sk) Down _ _) (SettingsMenu (i,sliders,opts)  a b)
  =case sk of
   KeyUp   -> (SettingsMenu (mLim (i-1),sliders,opts)  a b)
   KeyDown -> (SettingsMenu (mLim (i+1),sliders,opts)  a b)
   KeyLeft -> mover Sleft
   KeyRight-> mover Sright
   KeyEnter-> if i == 3 then MainMenu (0,mainMenuOpts) a (mapa,props,nSetts,jogo) 
              else if i==4 then MainMenu (0,mainMenuOpts) a b
              else (SettingsMenu (i,sliders,opts)  a b)
   otherwise->(SettingsMenu (i,sliders,opts)  a b)
   where
   nSetts = menuToSettings (SettingsMenu (i,sliders,opts)  a b)
   (mapa,props,setts,jogo) = b
   mLim = menuLimit (0,4)
   mover move = (SettingsMenu (i,sMoveAt i move sliders,opts)  a b)


event (EventKey key ks _ _) (Game j ((p1Act@(Acao ace trav esq dir nit)):as) (scl,trs) 
      imgs settings@(_,snPlayers,sController) savedImgs frame menuOpts@(menuMap,menuProps,menuSetts) stuns)
  = case (key,snPlayers,sController) of
      --PLAYER 1 ACTIONS
      (Char 'a',_,0)            -> (Game j ((Acao ace trav change dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'w',_,0)            -> (Game j ((Acao change trav esq dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'd',_,0)            -> (Game j ((Acao ace trav esq change nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 's',_,0)            -> (Game j ((Acao ace change  esq dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'q',_,0)            -> (Game j ((Acao ace trav esq dir (turnNit 0)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'e',_,0)            -> (Game j ((Acao ace trav esq dir (turnNit 1)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'r',_,0)            -> (Game j ((Acao ace trav esq dir (turnNit 2)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'f',_,0)            -> (Game j ((Acao ace trav esq dir (turnNit 3)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyLeft,_,1)  -> (Game j ((Acao ace trav change dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyUp,_,1)    -> (Game j ((Acao change trav esq dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyRight,_,1) -> (Game j ((Acao ace trav esq change nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyDown,_,1)  -> (Game j ((Acao ace change  esq dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'h',_,1)            -> (Game j ((Acao ace trav esq dir (turnNit 0)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'j',_,1)            -> (Game j ((Acao ace trav esq dir (turnNit 1)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'k',_,1)            -> (Game j ((Acao ace trav esq dir (turnNit 2)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'l',_,1)            -> (Game j ((Acao ace trav esq dir (turnNit 3)):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)

      --PLAYER 2 ACTIONS
      (Char 'a',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   change dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'w',2,1)            -> (Game j (p1Act:(Acao change trav2   esq2   dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'd',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   change nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 's',2,1)            -> (Game j (p1Act:(Acao ace2   change  esq2   dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'q',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 0)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'e',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 1)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'r',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 2)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'f',2,1)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 3)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyLeft,2,0)  -> (Game j (p1Act:(Acao ace2   trav2   change dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyUp,2,0)    -> (Game j (p1Act:(Acao change trav2   esq2   dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyRight,2,0) -> (Game j (p1Act:(Acao ace2   trav2   esq2   change nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (SpecialKey KeyDown,2,0)  -> (Game j (p1Act:(Acao ace2   change  esq2   dir2   nit2)       :p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'h',2,0)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 0)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'j',2,0)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 1)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'k',2,0)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 2)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
      (Char 'l',2,0)            -> (Game j (p1Act:(Acao ace2   trav2   esq2   dir2   (turnNit 3)):p2as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)

      (Char 'p',_,_)            -> MainMenu (0,mainMenuOpts) savedImgs (menuMap,menuProps,menuSetts,(j,settings,frame))
      otherwise -> (Game j ((Acao ace trav esq dir nit):as) (scl,trs) imgs settings savedImgs frame menuOpts stuns)
  where
  change = ks == Down
  ((Acao ace2 trav2 esq2 dir2 nit2):p2as) = as



  turnNit n = if change then (Just n)
              else Nothing
event _ gs = gs


---- Funções relativas ao MainMenu
{-|Função que devolve a imagem do menu principal.-}
drawMainMenu :: Int -> [String] -> Picture
drawMainMenu n menuStr = Translate (-width/3) 0 (Scale (0.2*baseScale) (0.2*baseScale) (Pictures (pics n menuStr 180)))
                where
                pics n []     y = []
                pics n (m:ms) y = case n of
                                  0 -> (Translate 0 (5*y) (Color red (Text m))) : (pics (n-1) ms (y-75))
                                  otherwise -> (Translate 0 (5*y) (Text m)) : (pics (n-1) ms (y-75))

{-|Opções contidas no menu principal.-}
mainMenuOpts ::  [String]
mainMenuOpts =["Continuar",
               "Novo Jogo",
               "Mapas",
               "Propriedades",
               "Opcoes de Jogo",
               "Sair"
               ]

---- Funções relativas ao menu de mapas

{-|Inicia o menu de seleção de mapas através do menu principal.-}
setupMapSelection :: GameState -> GameState
setupMapSelection (MainMenu _  savedPictures (mapa,props,sett,jogo))
  = MapSelectionMenu (0,0,0,mapasMapSelection,applyCancelString) savedPictures (mapa,props,sett,jogo)

{-|Devolve a imagem dos mapas no menu de seleção de mapas.-}
drawMapSelectionMenuMaps :: (Int,Int) -> Float -> [Mapa] -> Mapa -> Picture
drawMapSelectionMenuMaps (x,i) diff maps selected
  = Translate xOffset yOffset (Pictures (drawMapsH (x,i) diff maps 0))
  where
  drawMapsH _ _ [] _ = []
  drawMapsH (x,i) diff (m:ms) n
    = case (x,i) of
      (0,0) -> (Translate 0 (diff*n) (Scale 1.4 1.4 (mapaImg))) : (drawMapsH (1,i-1) diff ms (n-1))
      (_,_) -> (Translate 0 (diff*n) (mapaImg)) : (drawMapsH (x,i-1) diff ms (n-1))
      where
      mapaImg  = if m == selected then Pictures [lavaImg,boardImg,check]
                 else Pictures [lavaImg,boardImg]
      boardImg = (makeBoardMidOffset m scl)
      scl      = getScale m (escala)
      lavaImg  = Color corLava (Polygon [(escala/2,escala/2),(escala/2,-escala/2),(-escala/2,-escala/2),(-escala/2,escala/2)])
      (dimx,dimy) = dimensaoMapa m
      corLava = makeColorI 255 50 0 255
      corCheck= makeColorI 20 130 0 255
      check  = Translate (escala/2) (escala/2) (Color corCheck (ThickCircle 4 8))
      escala = (baseScale*200)

  xOffset = -width/4
  yOffset = (diff * fromIntegral i)
  square  = Polygon [(l,l),(l,-l),(-l,-l),(-l,l)]
  l = 100
  

{-|Devolve a imagem das opções no menu de seleção de mapas.-}
drawOptsMapSelect :: [String] -> (Float,Float) -> Float -> Float -> (Int,Int) -> Picture
drawOptsMapSelect s (x,y) scl diff (i,j)
  = Translate x y (Pictures (drawOptsH s diff 0 j_))
  where
  drawOptsH [] _ _ _ = [Blank]
  drawOptsH (strin:ss) diff i j
    = let f text = (Translate 0 (diff*i) (Scale scl scl (Text text))) in 
      case j of
      0 -> (Color red (f strin)) : drawOptsH ss diff (i-1) (j-1)
      otherwise -> (f strin) : drawOptsH ss diff (i-1) (j-1)
  j_ = if i==1 then j
       else (-1)

---- Funções relativas ao Menu de propriedades
{-|Função que devolve um slider iniciado com certas propriedades.-}
sliderProps :: Propriedades -> [Slider]
sliderProps (Propriedades atr pne acel pes nit rod)
            = [(Slider (0,5)   (realToFrac atr)   0.25),
               (Slider (0,5)   (realToFrac pne)   0.20),
               (Slider (0,10)  (realToFrac acel)  0.50),
               (Slider (0,4)   (realToFrac pes)   0.20),
               (Slider (0,20)  (realToFrac nit)   0.75),
               (Slider (0,360) (realToFrac rod)   20.0)
               ]
{-|String correspondente a cada slider de sliderProps.-}
stringProps :: [String]
stringProps = ["k_atrito",
               "k_pneus",
               "k_acel",
               "k_peso",
               "k_nitro",
               "k_roda"]
{-|Devolve uma lista de tuplos com os sliders iniciados relativamente às propriedades e a sua descrição correspondente.-}
stringSliderProps :: Propriedades -> [(Slider,String)]
stringSliderProps props = zip (sliderProps props) stringProps

{-|Extrai as propriedades contidas nos sliders de um Menu de propriedades.-}
menuToProps :: GameState -> Propriedades
menuToProps (PropertyMenu (_,s,_) _ _)
  = Propriedades atr pne acel pes nit rod
  where
  [atr,pne,acel,pes,nit,rod] = map realToFrac (map sGetValue (map fst s))

{-|Troca o estado de jogo de um menu principal para um menu de propriedades-}
toPropMenu :: GameState -> GameState
toPropMenu (MainMenu _  savedPictures (mapa,props,sett,jogo))
  = PropertyMenu (0,stringSliderProps props,applyCancelString) savedPictures (mapa,props,sett,jogo)


---- Funções relativas ao menu de opções
{-|Devolve uma lista de tuplos com os sliders iniciados relativamente às propriedades e a sua descrição correspondente.-}
stringSliderOpts :: Settings -> [(Slider,String)]
stringSliderOpts opts = zip (sliderOpts opts) stringOpts

{-|Função que devolve um slider iniciado com certas opcoes de jogo.-}
sliderOpts :: Settings -> [Slider]
sliderOpts (a,b,c)
            = [(Slider (0,2)   (fromIntegral a)   1),
               (Slider (0,2)   (fromIntegral b)   1),
               (Slider (0,1)   (fromIntegral c)   1)
               ]

{-|String correspondente a cada slider de sliderOpts.-}
stringOpts :: [String]
stringOpts = ["No de bots",
               "No de players",
               "Controlos"]

{-|Troca o estado de jogo de um menu principal para um menu de opcoes de jogo-}
toSettingsMenu :: GameState -> GameState
toSettingsMenu (MainMenu _  savedPictures (mapa,props,sett,jogo))
  = SettingsMenu (0,stringSliderOpts sett,applyCancelString) savedPictures (mapa,props,sett,jogo)

{-|Extrai as opcoes contidas nos sliders de um Menu de opcoes.-}
menuToSettings :: GameState -> Settings
menuToSettings (SettingsMenu (_,s,_) _ _)
  = (a,b,c)
  where
  [a,b,c] = map round (map sGetValue (map fst s))

---- Funções gerais de menus

{-|String que contém as opções de aplicar/cancelar.-}
applyCancelString :: [String]
applyCancelString = ["aplicar",
                     "cancelar"]

{-|Função que devolve a imagem de uma lista de tuplos contendo sliders e a sua descrição.-}
drawSliders :: [(Slider,String)] -> (Float,Float) -> (Float,Float) -> Float -> Int -> Picture
drawSliders sliders (x,y) (l,h) diff j
  = Translate x y (Pictures (drawSliderH sliders (l,h) diff 0 j))
  where
  drawSliderH :: [(Slider,String)] -> (Float,Float) -> Float -> Int -> Int -> [Picture]
  drawSliderH [] _ _ _ _ = [Blank]
  drawSliderH (s:ss) (l,h) diff i j = (drawSlider s (l,h) diff i j):(drawSliderH ss (l,h) diff (i-1) (j-1))
  drawSlider :: (Slider,String) -> (Float,Float) -> Float -> Int -> Int -> Picture
  drawSlider (slid,strin) (l,h) diff i 0 = Color red (sGetPic (0,diff*(fromIntegral i)) (l,h) slid strin)
  drawSlider (slid,strin) (l,h) diff i j = sGetPic (0,diff*(fromIntegral i)) (baseScale*300,h) slid strin

{-|Função que devolve a imagem das opcoes dos menus de propriedades e opcoes de jogo.-}
drawOpts :: [String] -> (Float,Float) -> Float -> Float -> Int -> Picture
drawOpts s (x,y) scl diff j
  = Translate x y (Pictures (drawOptsH s diff 0 j))
  where
  drawOptsH [] _ _ _ = [Blank]
  drawOptsH (strin:ss) diff i j
    = let f text = (Translate 0 (diff*i) (Scale (scl*baseScale) (scl*baseScale) (Text text))) in 
      case j of
      0 -> ((Color red (f strin))) : drawOptsH ss diff (i-1) (j-1)
      otherwise -> (f strin) : drawOptsH ss diff (i-1) (j-1)


{-|Limita um valor entre um valor minimo e máximo, sendo que se for maior que o maximo/menor que o minimo
  será normalizado para minimo/maximo, respetivamente.-}
menuLimit :: (Int,Int) -> Int -> Int
menuLimit (min,max) x
  |x<min = max
  |x>max = min
  |otherwise = x

------ Funções de Jogo
{-|Função que inicia um jogo novo através do menu principal.-}
setupNewGame :: GameState -> GameState
setupNewGame (MainMenu _ savedPictures (mapa,props,setts,(jogo,settings,frames)))
  = Game (Jogo mapa props carros nitros historicos)
         acoes
         (scl,(diffx,diffy))
         [mapSprites,pCarros,pNitro]
         setts
         savedPictures
         0
         (mapa,props,setts)
         stuns
  where
  replics = carAmount setts
  carros  = carroInit mapa replics
  nitros  = replicate replics quantInicialNitro
  historicos = replicate replics ([])
  acoes = replicate replics (Acao False False False False Nothing)
  stuns = replicate replics 1.5
  (Jogo _ _ c _ _) = jogo
  scl         = getScale mapa height
  diffx       = -(width/2) + (height/2 - ((fromIntegral dimx)*scl)/2)
  diffy       = ((fromIntegral dimy)*scl)/2
  pista       = Translate diffx diffy (makeBoard mapa scl)
  lava        = Translate (-width/2+(scl/2)) (height/2-scl) (makeLava (height) scl scaledLava)
  mapSprites  = makeLavaSprites pista lava 16 (scl/16)
  scaledLava  = scaleLava (scl/16) (plava)
  (dimx,dimy) = dimensaoMapa mapa
  plava  = savedPictures !! 0
  pCarros = [savedPictures !! 1,savedPictures !! 2,savedPictures !! 3,savedPictures !! 4]
  pNitro = [savedPictures !! 5]

{-|Jogo inicializado em setup.-}
jogoInicial :: Jogo
jogoInicial = Jogo 
            { mapa        = changeMap
            , pista       = changeProp
            , carros      = (carroInit changeMap 2)
            , nitros      = [quantInicialNitro,quantInicialNitro]
            , historico   = [[],[]]
            }

{-|Calcula a quantidade de carros que estarão na corrida.-}
carAmount :: Settings -> Int
carAmount (a,b,_) = a+b

{-|Reseta o jogo (quando um dos carros ganha a corrida).-}
resetGame :: GameState -> GameState
resetGame (Game (Jogo mapa props _ _ _) acoes (scl,trs) imgs settings savedImgs frame menuOpts stuns)
  = (Game (Jogo mapa props carros nitros historicos) acoes (scl,trs) imgs settings savedImgs 0 menuOpts stuns)
  where
  replics = carAmount settings
  carros  = carroInit mapa replics
  nitros  = replicate replics quantInicialNitro
  historicos = replicate replics ([])
  stuns = replicate replics 1.5

{-|Volta ao jogo que estáva pausado anteriormente.-}
resumeGame :: GameState -> GameState
resumeGame (MainMenu _ savedPictures (mapaO,propsO,optsO,(jogo,settings,frames)))
  = Game jogo
         (replicate (length c) (Acao False False False False Nothing))
         (scl,(diffx,diffy))
         [mapSprites,pCarros,pNitro]
         settings
         savedPictures
         frames
         (mapaO,propsO,optsO)
         (replicate (length c) 1)
  where
  (Jogo map _ c _ _) = jogo
  scl         = getScale map height
  diffx       = -(width/2) + (height/2 - ((fromIntegral dimx)*scl)/2)
  diffy       = ((fromIntegral dimy)*scl)/2
  pista       = Translate diffx diffy (makeBoard map scl)
  lava        = Translate (-width/2+(scl/2)) (height/2-scl) (makeLava (height) scl scaledLava)
  mapSprites  = makeLavaSprites pista lava 16 (scl/16)
  scaledLava  = scaleLava (scl/16) (plava)
  (dimx,dimy) = dimensaoMapa map
  plava  = savedPictures !! 0
  pCarros = [savedPictures !! 1,savedPictures !! 2,savedPictures !! 3,savedPictures !! 4]
  pNitro = [savedPictures !! 5]




{-|Função que atualiza o estado de jogo.-}
update :: Float -> GameState -> GameState
update t game@(Game jogo acoes (scl,trs) imgs settings savedImgs frame menuOpts stuns)
  |checkWonGame mapa historicos = resetGame game
  |otherwise = addFrame nGame
  where
  (Jogo mapa _ _ _ historicos) = jogo
  nGame = (Game nJogo newActions (scl,trs) imgs settings savedImgs frame menuOpts nStuns)

  newActions = updateActionBots jogo acoes settings
  (nJogo,nStuns) = updateGame frameStep jogo acoes stuns
update _ gs = gs


{-|Devolve a ação dos bots consoante o jogo.-}
updateActionBots :: Jogo -> [Acao] -> Settings -> [Acao]
updateActionBots jogo acoes (botNumber,_,_)
  = case botNumber of
    0 -> acoes
    1 -> swap1
    2 -> swap2
    where
    index1 = length acoes -1
    index2 = index1 -1
    bot1 = bot frameStep jogo index1
    bot_2 = bot2 frameStep jogo index2
    swap1 = swapAt index1 bot1 acoes
    swap2 = swapAt index2 bot_2 swap1

    swapAt :: Int -> a -> [a] -> [a]
    swapAt n y [] = []
    swapAt n y (x:xs)
      |n == 0 = y : xs
      |otherwise = x : (swapAt (n-1) y xs) 

{-|Devolve a Picture que corresponde á posição da lava num dado frame.-}
mapAtFrame :: Int -> [Picture] -> Picture
mapAtFrame f imgs
  = imgs !! imgIndex
  where
  imgIndex = floor  (realToFrac (i * (16)))
  i = mod' time (1)
  time = frameStep * (fromIntegral f) * speed
  speed = 0.75

{-|Adiciona um frame a um estado de jogo.-}
addFrame :: GameState -> GameState
addFrame (Game j a e i opts sp frames menuOpts stuns) = (Game j a e i opts sp (frames+1) menuOpts stuns)

{-|Devolve os carros na posição inicial de um mapa.-}
carroInit :: Mapa -> Int -> [Carro]
carroInit (Mapa ((x,y),ori) _) n
  = case ori of
    Norte -> rep (Carro (fIx+0.5,fIy    ) (-90) (0,0))
    Sul   -> rep (Carro (fIx+0.5,fIy+1  ) 90    (0,0))
    Este  -> rep (Carro (fIx    ,fIy+0.5) 0     (0,0))
    Oeste -> rep (Carro (fIx+1  ,fIy+0.5) 180   (0,0))
  where
  (fIx,fIy) = (fromIntegral x, fromIntegral y)
  rep = replicate n

{-|Atualiza os carros, nitros e históricos de um Jogo.-}
updateGame :: Tempo -> Jogo -> [Acao] -> [Tempo] -> (Jogo,[Tempo])
updateGame t (Jogo mapa pista carros nitros historico) acoes stuns
  = ((Jogo mapa pista nCs nNits nHists),nStuns)
  where
  toStun = map not (checkValidPos carros historico (definePercurso mapa))
  lNitros = calcularNitrosAcoes acoes nitros (replicate (length nitros) False)
  (nCs,nNits,nHists,nStuns) = unzip4 (updateCarros t
                                                   (Jogo mapa pista carros nitros historico)
                                                   acoes
                                                   lNitros
                                                   stuns
                                                   toStun)
{-|Faz o mesmo do que a função unzip mas para tuplos com 4 elementos.-}
unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 [] = ([],[],[],[])
unzip4 ((a,b,c,d):ls)
  = (a:as,b:bs,c:cs,d:ds)
  where
  (as,bs,cs,ds) = unzip4 ls

{-|Testa se algum carro ganhou a corrida.-}
checkWonGame :: Mapa -> [[Posicao]] -> Bool
checkWonGame mapa ((pos_atual:pos_ant:pos_ant2:poss):hs)
  | pos_atual == firstPos && pos_ant == lastPos && pos_ant2 == lastPos2 = True
  | otherwise = checkWonGame mapa hs
  where
  percurso = definePercurso mapa
  (firstPos,_,_) = head percurso
  (lastPos ,_,_) = last percurso
  (lastPos2,_,_) = last (init percurso)
checkWonGame mapa (x:xs) = checkWonGame mapa xs
checkWonGame mapa [] = False

{-|Devolve os valores atualizados dos carros, nitros e históricos.-}
updateCarros :: Tempo -> Jogo -> [Acao] -> [Bool] -> [Tempo] -> [Bool] -> [(Carro,Tempo,[Posicao],Tempo)]
updateCarros _ (Jogo _ _ [] _ _) _ _ _ _= []
updateCarros t (Jogo mapa pista (c:cs) (tn:tns) (h:hs)) (a:as) (n:ns) (st:sts) (tst:tsts)
  = (updateCarro t mapa pista c tn h n a st tst): (updateCarros t (Jogo mapa pista cs tns hs) as ns sts tsts)

{-|Devolve os valores atualizados de um carro, nitro e histórico.-}
updateCarro :: Tempo -> Mapa -> Propriedades -> (Carro) -> (Tempo) -> [Posicao] 
               -> (Bool) -> Acao -> Tempo -> Bool -> (Carro, Tempo, [Posicao], Tempo)
updateCarro t mapa pista carro nitro posicoes recebeNitro acao stun toStun
  |toStun && stun == 0 = (Carro resetPos nDirecao (0,0),nTNitro,nListaPos,1)
  |m == Nothing = (Carro resetPos nDirecao (0,0),nTNitro,nListaPos,1)
  |otherwise = ((Carro (nPos) nDirecao nVelT3),nTNitro,nListaPos,nStun)
  where

  Propriedades atrito pneus acel peso k_nitro roda = pista
  m = movimenta tab t carroNVel
  (Just (Carro pos _ nVelT3)) = m
  carroNVel = (Carro (x,y) ang nVel)
  (Carro (x,y) ang vel) = carro
  (Mapa _ tab)= mapa
  nPos        = case m of
                Nothing   -> resetPos
                otherwise -> pos

  nStun |stun>t = stun-t
        |otherwise = 0

  nVel | stun > 0 = (0,0)
       | isNaN nVelx || isNaN nVely = vAdd vel somaForcas2
       | otherwise = (nVelx,nVely)

  (nVelx,nVely)= vAdd vel somaForcas

  somaForcas2 = (vAddList [fAtrito,fAcelaracao,fGravidade,fNitro])
  somaForcas  = (vAddList [fPneus,fAtrito,fAcelaracao,fGravidade,fNitro])
  fAtrito     = calcularAtrito atrito carro t
  fAcelaracao = calcularAceleracao acao carro acel t
  fGravidade  = calcularPeso mapa carro peso t
  fPneus      = calcularFPneus carro pneus t
  fNitro      = calcularFNitro carro recebeNitro k_nitro t
  nDirecao    = vNormalizeDegrees (calcularAngulo acao carro roda t)
  nTNitro     = if nStun == 0 then calculaTempoNitro acao nitro t
                else nitro
  posicao     = (floor x,floor y)
  nListaPos   = addToLista posicao posicoes
  resetPos    = case tipo of
                (Curva Este) -> (fx +0.25, fy+0.75)
                (Curva Oeste)-> (fx +0.75, fy+0.25)
                (Curva Sul) -> (fx +0.25, fy+0.25)
                (Curva Norte) -> (fx+0.75, fy+0.75)
                otherwise -> (fx +0.5,fy+0.5)
                where
                (x,y) |toStun = (nListaPos !! 1)
                      |otherwise = (nListaPos !! 0)
                (fx,fy) = (fromIntegral x, fromIntegral y)
                (Peca tipo _) = ((tab) !! y) !! x

  updatePosicaoCarro :: Maybe Carro -> [Posicao] -> Ponto
  updatePosicaoCarro Nothing ((x,y):_) = (fromIntegral x, fromIntegral y)
  updatePosicaoCarro (Just (Carro pos _ _)) _= pos
