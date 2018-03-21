--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.Assets
Description : Acesso a imagens e outros recursos.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo é destinado ao carregamento e acesso a recursos externos utilizados
pelo jogo, e.g. imagens e tipos de letra.
-}
module Sokoban.Assets (

    Assets,
    load,

    uiBitmap,
    uiFont,

    playerBitmap,

    Theme,
    randomTheme,
    tileBitmap,
    targetBitmap,
    boxBitmap

    ) where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import System.Random
import qualified Data.Map.Strict as Map

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture

import Sokoban.Helper
import qualified Sokoban.UI.Font   as Font
import qualified Sokoban.UI.UIDefs as UIDefs

--------------------------------------------------------------------------------
-- * Constantes

-- $
-- Constantes utilizadas pelo módulo.

-- | Caminho relativo da diretoria onde se encontram os bitmaps relativos à UI.
uiDir :: FilePath
uiDir = "assets/ui/"

-- | Caminho relativo da diretoria onde se encontram os ficheiros relativos ao
-- tipo de letra utilizado pelo jogo.
fontDir :: FilePath
fontDir = "assets/ui/font/"

-- | Caminho relativo da diretoria onde se encontram os bitmaps associados com
-- cada personagem utilizados pelo jogo.
playerDir :: FilePath
playerDir = "assets/players/"

-- | Caminho relativo da diretoria onde se encontram os bitmaps associados com
-- cada tema (aspeto gráfico do tabuleiro e caixas) utilizados pelo jogo.
themeDir :: FilePath
themeDir = "assets/themes/"

--------------------------------------------------------------------------------
-- * Assets

-- $
-- Tipos e funções relacionados com assets em geral.
--
-- Define-se um asset como sendo um recurso externo, e.g. uma imagem.

-- | Tipo /abstrato/ que armazena todos os recursos externos utilizados pelo
-- jogo.
data Assets = Assets {
    assetsUI      :: UIAssets,     -- ^ Assets relacionados com a UI.
    assetsPlayers :: PlayerAssets, -- ^ Assets relacionados com as personagens.
    assetsThemes  :: [ThemeAssets] -- ^ Assets relacionados com as tiles e
                                   -- caixas.
    }

-- | Carrega todos os assets disponíveis, a partir das diretorias predefinidas.
load :: IO Assets
load = do

    ui      <- loadUI
    players <- loadPlayers
    themes  <- loadThemes

    return Assets {
        assetsUI      = ui,
        assetsPlayers = players,
        assetsThemes  = themes
        }

--------------------------------------------------------------------------------
-- ** Interface do Utilizador (UI)

-- $
-- Tipos e funções relacionados com assets utilizados pela UI.

-- | Armazena os bitmaps e o tipo de letra utilizados pela UI.
data UIAssets = UIAssets {
    uiAssetsBitmaps :: BitmapMap, -- ^ Bitmaps utilizados pela UI.
    uiAssetsFont    :: Font.Font  -- ^ Tipo de letra utilizado pela UI.
    }

-- | Obtém um bitmap da UI.
uiBitmap :: String  -- ^ Nome do bitmap.
         -> Assets  -- ^ Assets.
         -> Picture
uiBitmap bitmapName =
    fromMaybe (error bitmapName)
    . Map.lookup bitmapName
    . uiAssetsBitmaps
    . assetsUI

-- | Obtém o tipo de letra da UI.
uiFont :: Assets -> Font.Font
uiFont = uiAssetsFont . assetsUI

--------------------------------------------------------------------------------
-- *** Carregamento

-- $
-- Funções relacionadas com o carregamento de assets da UI.

-- | Carrega os bitmaps e o tipo de letra utilizados pela UI, a partir das
-- diretorias predefinidas.
loadUI :: IO UIAssets
loadUI = do

    bmps <- loadBitmapsFromDir uiDir
    font <- Font.load fontDir UIDefs.supportedTextColors

    return UIAssets {
        uiAssetsBitmaps = bmps,
        uiAssetsFont    = font
        }

--------------------------------------------------------------------------------
-- ** Personagens

-- $
-- Tipos e funções relacionados com assets utilizados pelas personagens.

-- | Armazena os bitmaps utilizados pelas personagens.
type PlayerAssets = Map.Map String BitmapMap

-- | Obtém um bitmap de uma personagem.
playerBitmap :: String     -- ^ Nome da personagem.
             -> String     -- ^ Nome do bitmap.
             -> Assets     -- ^ Assets.
             -> Picture
playerBitmap playerName bitmapName =
    fromMaybe (error $ playerName ++ " " ++ bitmapName)
    . Map.lookup bitmapName
    . fromMaybe (error playerName)
    . Map.lookup playerName
    . assetsPlayers

--------------------------------------------------------------------------------
-- *** Carregamento

-- $
-- Funções relacionadas com o carregamento de assets das personagens.

-- | Carrega os bitmaps utilizados pelas personagens, a partir das diretorias
-- predefinidas.
loadPlayers :: IO PlayerAssets
loadPlayers = do

    dirs    <- getDirsInDir playerDir
    players <- mapM loadPlayer dirs

    return $ Map.fromList players

-- | Carrega os bitmaps utilizados por uma personagem, a partir da diretoria
-- especificada.
loadPlayer :: FilePath               -- ^ Caminho da diretoria. Deve terminar
                                     -- com @\'/\'@.
           -> IO (String, BitmapMap) -- ^ (Nome da personagem, Bitmaps da
                                     -- personagem)
loadPlayer dir = do

    let name = dirName dir
    bmps <- loadBitmapsFromDir dir

    return (name, bmps)

--------------------------------------------------------------------------------
-- ** Tiles e Caixas

-- $
-- Tipos e funções relacionados com assets utilizados pelas tiles e caixas.
--
-- Uma tile é uma das células que constituem o tabuleiro de jogo e a respetiva
-- imagem.

-- | Tipo /abstrato/ que dentifica um tema.
--
-- Os temas afetam o aspeto das tiles e das caixas utilizados para desenhar o
-- tabuleiro de jogo. Cada tema é constituido por uma série de sprites que são
-- utilizados para representar as tiles e as caixas.
newtype Theme = Theme Int

-- | Armazena os sprites utilizados por cada tema.
data ThemeAssets = ThemeAssets {
    themeAssetsTiles  :: SpriteList,    -- ^ Sprites das tiles.
    themeAssetsTarget :: SpriteBitmaps, -- ^ Sprite da tile para locais de
                                        -- arrumação.
    themeAssetsBox    :: SpriteBitmaps  -- ^ Sprite para as caixas.
    }

-- | Obtém um identificador de tema aleatório, referente a um dos temas
-- disponíveis no 'Assets' especificado.
randomTheme :: Assets -> IO Theme -- Random theme identifier.
randomTheme assets = do
    index <- randomRIO (0, (length $ assetsThemes assets) - 1)
    return $ Theme index

-- | Devolve uma lista de todos os sprites de tiles que correspondem ao padrão
-- especificado (cf. 'tileBitmap').
tileSprite :: String     -- ^ Padrão a procurar.
           -> SpriteList -- ^ Lista de sprites a analisar.
           -> SpriteList -- ^ Lista de sprites resultante.
tileSprite pattern []                    = []
tileSprite pattern (sprite@(name,_) : t) =
    if and $ zipWith (\nch pch -> nch == '-' || nch == pch) name pattern
        then sprite : tileSprite pattern t
        else tileSprite pattern t

-- | Obtém um bitmap do sprite da tile correspondente ao padrão especificado.
--
-- O sprite que cada célula do tabuleiro terá é dependente da própria célula e
-- das 8 células que a rodeiam. O padrão especificado deve descrever essas 9
-- células, linha por linha, numa string de 9 carateres. Os carateres permitidos
-- são os seguintes:
--
-- * @\'g\'@ - a célula é vazia ou é um local de arrumação;
-- * @\'w\'@ - a célula é uma parede.
--
-- Exemplo: @"gwgwwgwwg"@ corresponde à seguinte disposição de células:
--
-- @
-- gwg
-- wwg
-- wwg
-- @
--
-- __Nota:__ se a célula central for um local de arrumação, deve ser utilizada a
-- função 'targetBitmap'.
tileBitmap :: String      -- ^ Padrão das células.
           -> Theme       -- ^ Identificador do tema a utilizar.
           -> Assets      -- ^ Assets.
           -> IO Picture
tileBitmap pattern (Theme theme) =
    bitmapFromSprite
    . head
    . tileSprite pattern
    . themeAssetsTiles
    . (!! theme)
    . assetsThemes

-- | Obtém um bitmap do sprite da tile correspondente a um local de arrumação.
targetBitmap :: Theme      -- ^ Identificador do tema a utilizar.
             -> Assets     -- ^ Assets.
             -> IO Picture
targetBitmap (Theme theme) =
    bitmapFromSpriteBitmaps . themeAssetsTarget . (!! theme) . assetsThemes

-- | Obtém um bitmap do sprite correspondente às caixas.
boxBitmap :: Theme      -- ^ Identificador do tema a utilizar.
          -> Assets     -- ^ Assets.
          -> IO Picture
boxBitmap (Theme theme) =
    bitmapFromSpriteBitmaps . themeAssetsBox . (!! theme) . assetsThemes

--------------------------------------------------------------------------------
-- *** Carregamento

-- $
-- Funções relacionadas com o carregamento de assets das tiles e caixas.

-- | Carrega os assets utilizados pelos temas, a partir das diretorias
-- predefinidas.
loadThemes :: IO [ThemeAssets]
loadThemes = do
    dirs <- getDirsInDir themeDir
    mapM loadTheme dirs

-- | Carrega os assets utilizados por um tema, a partir da diretoria
-- especificada.
loadTheme :: FilePath       -- ^ Caminho da diretoria. Deve terminar
                            -- com @\'/\'@.
          -> IO ThemeAssets
loadTheme dir = do

    let themeName = dirName dir
    sprites <- loadSpritesFromDir dir

    let tiles = checkTiles themeName
                . map (\(n,s) -> (drop 5 n, s))
                . filter (\(n,_) -> "tile_" `isPrefixOf` n)
                $ sprites

    let target = fromMaybe
                    (themeError themeName "sprite \"target\" not found")
                    (lookup "target" sprites)

    let box = fromMaybe
                  (themeError themeName "sprite \"box\" not found")
                  (lookup "box" sprites)

    return ThemeAssets {
        themeAssetsTiles  = tiles,
        themeAssetsTarget = target,
        themeAssetsBox    = box
        }

-- | Verifica se uma lista de sprites representa corretamente um conjunto de
-- tiles.
--
-- Se a lista for válida, devolve a mesma lista; em caso contrário, causa um
-- erro.
checkTiles :: String     -- ^ Nome do tema.
           -> SpriteList -- ^ Lista de sprites a verificar.
           -> SpriteList
checkTiles themeName tiles = do
    if null badTiles
        then map (checkTile themeName) tiles
        else themeError themeName $ "Bad tiles: " ++ show badTiles
    where
        patternCombs = replicateM 9 ['g', 'w']
        tileSprites = map (\p -> (length $ tileSprite p tiles, p)) patternCombs
        badTiles = filter ((/= 1) . fst) tileSprites

-- | Verifica se um sprite representa corretamente uma tile.
--
-- Se o sprite for válido, devolve o mesmo sprite; em caso contrário, causa um
-- erro.
checkTile :: String -- ^ Nome do tema.
          -> Sprite -- ^ Sprite a verificar.
          -> Sprite
checkTile themeName sprite@(name, _) = do
    if length name == 9 && all (`elem` "-gw") name
        then sprite
        else themeError themeName $ "bad tile name (\"" ++ name ++ "\")"

-- | Causa um erro relacionado com o tema de nome especificado.
themeError :: String -- ^ Nome do tema.
           -> String -- ^ Mensagem de erro.
           -> a
themeError themeName errorMsg =
    error $ "Error loading theme \"" ++ themeName ++ "\": " ++ errorMsg

--------------------------------------------------------------------------------
-- * Bitmaps

-- $
-- Tipos e funções relacionados com bitmaps em geral.

-- | Mapa que associa um nome a cada 'Picture' que contém um bitmap.
type BitmapMap = Map.Map String Picture

-- | Tuplo que associa um nome a uma 'Picture' que contém um bitmap.
type NamedBitmap = (String, Picture)

--------------------------------------------------------------------------------
-- ** Carregamento

-- $
-- Funções relacionadas com o carregamento de bitmaps.

-- | Carrega todos os bitmaps que se encontram na diretoria especificada.
--
-- São considerados todos os ficheiros de extensão @.bmp@. As subdiretorias
-- /não/ são incluídas.
--
-- É devolvido um mapa que associa um nome a cada bitmap. O nome de um bitmap
-- corresponde ao nome do ficheiro /sem extensão/ de onde foi carregado.
loadBitmapsFromDir :: FilePath     -- ^ Caminho da diretoria. Deve terminar com
                                   -- @\'/\'@.
                   -> IO BitmapMap
loadBitmapsFromDir dir = do

    files <- getFilesInDir dir
    let bmpFiles = filter (".bmp" `isSuffixOf`) files

    images <- mapM loadBitmap bmpFiles

    return $ Map.fromList images

-- | Carrega um bitmap a partir de um ficheiro.
--
-- É devolvido um tuplo que associa um nome ao bitmap. O nome do bitmap
-- corresponde ao nome do ficheiro /sem extensão/.
loadBitmap :: FilePath       -- ^ Caminho do ficheiro a carregar.
           -> IO NamedBitmap
loadBitmap file = do

    bmp <- loadBMP file
    let name = fileNameWithoutExt file

    return (name, bmp)

--------------------------------------------------------------------------------
-- * Sprites

-- $
-- Tipos e funções relacionados com sprites.
--
-- Define-se um sprite como sendo uma correspondência entre um nome e um
-- conjunto de bitmaps, cada um com uma probabilidade associada. A soma das
-- probabilidades de todos os bitmaps num sprite deve ser de @100%@.
--
-- Os sprites destinam-se a oferecer acesso aleatório a um conjunto de bitmaps,
-- dependente da probabilidade de cada bitmap.

-- | Uma lista de sprites.
type SpriteList = [Sprite]

-- | Representa um sprite.
--
-- Associa um nome a uma lista de bitmaps, cada um com uma probabilidade
-- associada.
--
-- A lista de bitmaps corresponde a uma lista de tuplos em que o
-- primeiro elemento representa a probabilidade associada com um bitmap e o
-- segundo elemento representa o bitmap. A probabilidade de cada bitmap deve ter
-- um valor entre @1@ e @100@, e a soma das probabilidades de todos os bitmaps
-- deve ser @100@.
--
-- Os elementos da lista de tuplos devem estar ordenados por ordem crescente da
-- probabilidade.
type Sprite = (String, SpriteBitmaps)

-- | Lista de bitmaps de um sprite (cf. 'Sprite').
type SpriteBitmaps = [(Int, Picture)]

-- | Constitui uma parte de um sprite.
--
-- Associa um nome com uma probabilidade e um bitmap.
--
-- A probabilidade deve ter um valor entre @1@ e @100@.
type SpritePart = (String, (Int, Picture))

-- | Devolve aleatoriamente um dos bitmaps de um sprite.
--
-- É tomada em consideração a probabilidade associada com cada bitmap.
bitmapFromSprite :: Sprite -> IO Picture
bitmapFromSprite (_, bmps) = bitmapFromSpriteBitmaps bmps

-- | Devolve aleatoriamente um dos bitmaps de uma lista de bitmaps de um sprite.
--
-- É tomada em consideração a probabilidade associada com cada bitmap.
bitmapFromSpriteBitmaps :: SpriteBitmaps -> IO Picture
bitmapFromSpriteBitmaps bmps = do

    let aux n ((prob,pic) : t) = (if n <= prob then pic else aux (n-prob) t)

    rand <- randomRIO (1, 100) :: IO Int
    return $ aux rand bmps

--------------------------------------------------------------------------------
-- ** Carregamento

-- $
-- Funções relacionadas com o carregamento de sprites.

-- | Constrói uma lista de sprites a partir de todos os bitmaps que se encontram
-- na diretoria especificada.
--
-- São considerados todos os ficheiros de extensão @.bmp@. As subdiretorias
-- /não/ são incluídas.
--
-- Os nomes e bitmaps dos sprites resultantes são calculados a partir dos nomes
-- dos ficheiros carregados (cf. 'loadSpritePart').
loadSpritesFromDir :: FilePath      -- ^ Caminho da diretoria. Deve terminar com
                                    -- @\'/\'@.
                   -> IO SpriteList
loadSpritesFromDir dir = do

    files <- getFilesInDir dir
    let bmpFiles = filter (".bmp" `isSuffixOf`) files

    loadSprites bmpFiles

-- | Constrói uma lista de sprites a partir de todos os bitmaps especificados.
--
-- Os nomes e bitmaps dos sprites resultantes são calculados a partir dos nomes
-- dos ficheiros carregados (cf. 'loadSpritePart').
loadSprites :: [FilePath]    -- ^ Caminho dos ficheiros a carregar.
            -> IO SpriteList
loadSprites files = do
    parts <- mapM loadSpritePart files
    return $ mergeSpriteParts parts

-- | Contrói uma lista de sprites a partir de uma lista de valores do tipo
-- 'SpritePart'.
--
-- @'SpritePart's@ com o mesmo nome são fundidas para formar um sprite.
mergeSpriteParts :: [SpritePart] -> SpriteList
mergeSpriteParts parts = map checkSprite sprites
    where
        ins [new] = insertBy (compare `on` fst) new
        sprites   = foldr (\(n,p) -> insertWithAL ins n [p]) [] parts

-- | Verifica se a probabilidade total de um sprite é de @100%@.
--
-- Se o sprite for válido, devolve o mesmo sprite; em caso contrário, causa um
-- erro.
checkSprite :: Sprite -> Sprite
checkSprite sprite@(name, bmps) =
    if prob == 100
        then sprite
        else error $ "Sprite \"" ++ name ++ "\": bad prob of " ++ show prob
    where
        prob = sum $ map fst bmps

-- | Carrega um valor do tipo 'SpritePart' a partir do ficheiro especificado.
--
-- O nome e probabilidade associados com a 'SpritePart' são extraídos do nome do
-- ficheiro, o qual deve seguir um formato específico:
--
-- @\<nome\>[.\<n\>_p\<prob\>].bmp@
--
-- * @\<nome\>@ - Nome da 'SpritePart';
-- * @\<n\>@ - Sequência de carateres arbitrária; diferencia entre ficheiros de
-- sprites com o mesmo nome e probabilidade;
-- * @\<prob\>@ - Probabilidade associada com a 'SpritePart'; inteiro entre @1@
-- e @100@.
--
-- Quando a parte opcional é omitida, a 'SpritePart' toma uma probabilidade de
-- @100%@.
--
-- Exemplos:
--
-- * @imagem.bmp@ - 'SpritePart' de nome @"imagem"@ e probabilidade @100@;
-- * @ola.1_p17.bmp@ - 'SpritePart' de nome @"ola"@ e probabilidade @17@.
loadSpritePart :: FilePath      -- ^ Caminho do ficheiro a carregar.
               -> IO SpritePart
loadSpritePart file = do

    bmp <- loadBMP file

    let (name, probStr) = span (/= '.') (fileNameWithoutExt file)
        prob = read (drop 2 . dropWhile (/= '_') $ probStr) :: Int

    if null probStr
        then do
            return (name, (100, bmp))
        else do
            when
                (prob < 1 || prob > 100)
                (fail $ file ++ ": bad prob = " ++ show prob)
            return (name, (prob, bmp))

--------------------------------------------------------------------------------
