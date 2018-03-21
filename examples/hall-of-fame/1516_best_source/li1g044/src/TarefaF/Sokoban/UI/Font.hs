--------------------------------------------------------------------------------

{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : Sokoban.UI.Font
Description : Tipos de letra.
Copyright   : Alberto Faria <albertofaria2@gmail.com>;
              Fábio Fontes <fabio.4797@gmail.com>

Este módulo permite desenhar texto com formatação e tipos de letra arbitrários,
carregados a partir de uma série de ficheiro num formato adequado.
-}
module Sokoban.UI.Font (
    Text (..),
    Alignment (..),

    Font,
    load,
    size, draw
    ) where

import Codec.BMP
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Sokoban.Helper

--------------------------------------------------------------------------------
-- * Tipos

-- | Representa uma secção de texto formatado.
data Text = Text {
    textString      :: String,     -- ^ Texto.
    textHeight      :: Float,      -- ^ Altura de uma linha de texto.
    textAlignment   :: Alignment,  -- ^ Alinhamento do texto.
    textColor       :: Color,      -- ^ Cor do texto.
    textShadowColor :: Maybe Color -- ^ Cor da sombra. 'Nothing' para não
                                   -- desenhar sombra.
    }

-- | Define o alinhamento de uma secção de texto.
data Alignment
    = LeftTop      -- ^ Encostado à esquerda e acima.
    | CenterTop    -- ^ Centrado horizontalmente e encostado acima.
    | RightTop     -- ^ Encostado à direita e acima.
    | LeftCenter   -- ^ Encostado à esquerda e centrado verticalmente.
    | Center       -- ^ Centrado horizontal e verticalmente.
    | RightCenter  -- ^ Encostado à direita e centrado verticalmente.
    | LeftBottom   -- ^ Encostado à esquerda e abaixo.
    | CenterBottom -- ^ Centrado horizontalmente e encostado abaixo.
    | RightBottom  -- ^ Encostado à direita e abaixo.

-- | Tipo utilizado pelo ficheiro de configuração do tipo de letra.
data FontConfig = FontConfig {
    height        :: Float,
    spacing       :: Float,
    globalOffsetY :: Float,
    newlineOffset :: Float,
    shadowOffset  :: Maybe Vector
    }
    deriving (Read)

-- | Representa um tipo de letra.
data Font = Font {
    fontHeight        :: Float,        -- ^ Altura de uma linha de texo.
    fontSpacing       :: Float,        -- ^ Espaço horizontal entre carateres.
    fontYOffset       :: Float,        -- ^ Ajuste vertical global.
    fontNewlineOffset :: Float,        -- ^ Distância entre linhas.
    fontShadowOffset  :: Maybe Vector, -- ^ Diferença de posição da sombra.
    fontChars         :: Map.Map Char FontChar -- ^ Mapa de carateres suportados
                                               -- pelo tipo de letra.
    }

-- | Representa um caratere de um tipo de letra.
data FontChar = FontChar {
    fontCharSize    :: Vector,            -- ^ Dimensões do caratere.
    fontCharYOffset :: Float,             -- ^ Ajuste vertical na posição do
                                          -- caratere.
    fontCharPics    :: [(Color, Picture)] -- ^ Bitmaps do caratere, em várias
                                          -- cores.
    }

--------------------------------------------------------------------------------
-- * Carregamento

-- | Carrega um tipo de letra a partir de uma diretoria. Essa diretoria deve
-- conter um ficheiro de configuração adequado e imagens do tipo BMP que
-- representem os carateres do tipo de letra.
load :: FilePath -- ^ Caminho da diretoria.
     -> [Color]  -- ^ Cores suportadas.
     -> IO Font
load dir colors = do

    config <- loadConfig dir

    chars <- loadChars dir colors

    return Font {
        fontHeight        = height        config,
        fontSpacing       = spacing       config,
        fontYOffset       = globalOffsetY config,
        fontNewlineOffset = newlineOffset config,
        fontShadowOffset  = shadowOffset  config,
        fontChars         = chars
        }

-- | Carrega o ficheiro de configuração de um tipo de letra.
loadConfig :: FilePath      -- ^ Caminho do ficheiro de configuração.
           -> IO FontConfig
loadConfig dir = do
    contents <- readFile (dir ++ "_font.cfg")
    return $ read contents

-- | Carrega os carateres de um tipo de letra, a partir de uma diretoria
-- adequada.
loadChars :: FilePath -- ^ Caminho da diretoria.
          -> [Color]  -- ^ Cores suportadas.
          -> IO (Map.Map Char FontChar)
loadChars dir colors = do

    files <- getFilesInDir dir
    let charFiles = filter (".bmp" `isSuffixOf`) files

    chars <- mapM (loadChar colors) charFiles

    return $ Map.fromListWithKey
        (\k _ _ -> error $ "Duplicate font character: '" ++ [k] ++ "'")
        chars

-- | Carrega um caratere de um tipo de letra, a partir de um ficheiro de imagem
-- do tipo BMP.
loadChar :: [Color]  -- ^ Cores suportadas.
         -> FilePath -- ^ Caminho do ficheiro.
         -> IO (Char, FontChar)
loadChar colors file = do

    bmpFile <- readBMP file
    let bmp = either (error . ((file ++ ": ") ++) . show) id bmpFile

    let (codepointStr, yOffStr) = span (/= '_') (fileNameWithoutExt file)

    let char = chr (read codepointStr)
        size = fromIntegralVector $ bmpDimensions bmp |-| (2,2)
        yOff = if null yOffStr then 0 else read (tail yOffStr) :: Float

    let pics = map (\c -> (c, multBMPToPicture c bmp)) colors

    return (
        char,
        FontChar {
            fontCharSize    = size,
            fontCharYOffset = yOff,
            fontCharPics    = pics
            }
        )

--------------------------------------------------------------------------------
-- * Desenho de texto

--------------------------------------------------------------------------------
-- ** Medição

-- | Calcula as dimensões que uma secção de texto tomará ao ser desenhada.
size :: Font   -- ^ Tipo de letra.
     -> Text   -- ^ Texto a medir.
     -> Vector -- ^ Dimensão do texto ao ser desenhado.
size font text = scale *| sizeString font (textString text)
    where scale = textHeight text / fontHeight font

-- | Calcula as dimensões que uma 'String' de texto tomaria ao ser desenhada no
-- tamanho nativo do tipo de letra.
sizeString :: Font   -- ^ Tipo de letra.
           -> String -- ^ Texto a medir.
           -> Vector -- ^ Dimensão do texto ao ser desenhado.
sizeString _    ""  = (0, 0)
sizeString font str = (width, height)
    where
        lns         = splitWhen (== '\n') str
        numExtraLns = fromIntegral $ length lns - 1

        width = maximum $ map (widthLine font) lns

        hLn     = fontHeight font
        newlOff = fontNewlineOffset font
        height  = hLn + numExtraLns * (hLn + newlOff)

-- | Calcula a largura que uma 'String' de texto /sem newlines/ tomaria ao ser
-- desenhada no tamanho nativo do tipo de letra.
widthLine :: Font   -- ^ Tipo de letra.
          -> String -- ^ Linha de texto a medir.
          -> Float  -- ^ Largura da linha texto ao ser desenhada.
widthLine font str = textWidth
    where
        charMap   = fontChars font
        charWidth = map widthChar str
        textWidth = sum $ intersperse (fontSpacing font) charWidth
        widthChar ch = w
            where
                fch = fromMaybe
                          (error $ "Font doesn't have character " ++ show ch ++ ".")
                          (Map.lookup ch charMap)
                (w,_) = fontCharSize fch

--------------------------------------------------------------------------------
-- ** Transformação em 'Picture'

-- | Desenha uma secção de texto.
draw :: Vector  -- ^ Posição principal do texto. O seu significado varia
                -- consoante o alinhamento do texto.
     -> Font    -- ^ Tipo de letra.
     -> Text    -- ^ Texto a ser desenhado.
     -> Picture -- ^ 'Picture' resultante.
draw pos font text =
    case (fontShadowOffset font, textShadowColor text) of
        (Just shadowOffset, Just shadowColor) -> Pictures [
            drawString align (pos |+| (scale *| shadowOffset)) font str scale shadowColor,
            drawString align pos font str scale color
            ]
        _ -> drawString align pos font str scale color
    where
        str   = textString text
        scale = textHeight text / fontHeight font
        align = textAlignment text
        color = textColor text

-- | Desenha uma 'String'.
drawString :: Alignment -- ^ Alinhamento do texto.
           -> Vector    -- ^ Posição principal do texto.
           -> Font      -- ^ Tipo de letra.
           -> String    -- ^ Texto a ser desenhado.
           -> Float     -- ^ Escala a aplicar ao texto.
           -> Color     -- ^ Cor do texto.
           -> Picture
drawString align (cx,cy) font str scale color =
    Translate cx cy . Scale scale scale $ pic
    where
        (_,h) = sizeString font str
        (f, yInit) = case align of
            LeftTop      -> (drawLineLeft  ,   0)
            CenterTop    -> (drawLineCenter,   0)
            RightTop     -> (drawLineRight ,   0)
            LeftCenter   -> (drawLineLeft  , h/2)
            Center       -> (drawLineCenter, h/2)
            RightCenter  -> (drawLineRight , h/2)
            LeftBottom   -> (drawLineLeft  ,   h)
            CenterBottom -> (drawLineCenter,   h)
            RightBottom  -> (drawLineRight ,   h)
        lnDiff = fontHeight font + fontNewlineOffset font
        lns = zip [yInit,yInit-lnDiff..] (splitWhen (== '\n') str)
        pic = Pictures $ concatMap (\(y,ln) -> f font color y ln) lns

-- | Desenha uma linha de texto alinhada à esquerda.
drawLineLeft :: Font -> Color -> Float -> String -> [Picture]
drawLineLeft font color y str = drawLine font color (0,y) str

-- | Desenha uma linha de texto alinhada à centrada horizontalmente.
drawLineCenter :: Font -> Color -> Float -> String -> [Picture]
drawLineCenter font color y str = drawLine font color (-w/2,y) str
    where w = widthLine font str

-- | Desenha uma linha de texto alinhada à direita.
drawLineRight :: Font -> Color -> Float -> String -> [Picture]
drawLineRight font color y str = drawLine font color (-w,y) str
    where w = widthLine font str

-- | Desenha uma linha de texto.
drawLine :: Font      -- ^ Tipo de letra.
         -> Color     -- ^ Cor do texto.
         -> Vector    -- ^ Canto superior esquerdo da linha.
         -> String    -- ^ Texto a desenhar.
         -> [Picture] -- ^ Imagens de cada caratere.
drawLine _    _     _     ""     = []
drawLine font color (x,y) (c:cs) =
    transPic : drawLine font color (x + w + fsp, y) cs
    where
        fh    = fontHeight font
        fsp   = fontSpacing font
        fch   = (fontChars font) Map.! c
        (w,h) = fontCharSize fch
        yoff  = fontYOffset font + fontCharYOffset fch
        pic   = fromJust $ lookup color (fontCharPics fch)
        transPic = Translate (x + w/2) (y - fh + h/2 + yoff) pic

--------------------------------------------------------------------------------
