-- Adiciona dois vetores bidimensionais componente por componente.
(|+|) :: Vector -> Vector -> Vector
(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)

-- Multiplica dois vetores bidimensionais componente por componente.
(|*|) :: Vector -> Vector -> Vector
(x1, y1) |*| (x2, y2) = (x1*x2, y1*y2)

-- Representa uma matrix quadrada de ordem 3. Deve ter 9 elementos.
type Matrix = [Float]

-- Devolve o elemento de uma matrix quadrada de ordem 3 na posição especificada.
matGet :: Matrix -> (Int, Int) -> Float
matGet m (i,j) = m !! (j + 3*i - 4)

-- Multiplica duas matrizes quadradas de ordem 3.
matMul :: Matrix -> Matrix -> Matrix
matMul a b = map f [(i,j) | i <- [1..3], j <- [1..3]]
    where f (i,j) = sum [(a `matGet` (i,k)) * (b `matGet` (k,j)) | k <- [1..3]]

-- Constrói uma matriz quadrada de ordem 3 que representa uma translação no
-- plano.
translationMatrix :: Vector -> Matrix
translationMatrix (x,y) = [
    1,0,x,
    0,1,y,
    0,0,1
    ]

-- Constrói uma matriz quadrada de ordem 3 que representa uma rotação no plano,
-- em relação à origem.
rotationMatrix :: Float -> Matrix
rotationMatrix a = [
    cos a, -sin a, 0,
    sin a,  cos a, 0,
        0,      0, 1
    ]

-- Constrói uma matriz quadrada de ordem 3 que representa uma transformação de
-- escala no plano, em relação à origem.
scalingMatrix :: Vector -> Matrix
scalingMatrix (x,y) = [
    x,0,0,
    0,y,0,
    0,0,1
    ]

-- Representa um retângulo alinhado com os eixos.
data Rect = Rect { rectLeft, rectBottom, rectRight, rectTop :: Float }
    deriving (Eq, Show)


-- Calcula as dimensões de um retângulo alinhado com os eixos.
rectSize :: Rect -> Vector
rectSize (Rect l b r t) = (r-l, t-b)

-- Calcula o menor retângulo (alinhado com os eixos) que contém todos os
-- retângulos especificados.
joinRects :: [Rect] -> Maybe Rect
joinRects []    = Nothing
joinRects rects = Just $
    Rect
        (minimum $ map rectLeft   rects)
        (minimum $ map rectBottom rects)
        (maximum $ map rectRight  rects)
        (maximum $ map rectTop    rects)
