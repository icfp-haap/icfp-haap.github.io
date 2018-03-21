module VectorLib 
(Vetor,vAdd,vAddList,vMultList,vInvertY,vCrossProduct3by3,vRadToDegree,vMag,vMult,vDist,vNormalizeDegrees,vDot,vNormalize,vNormalizeAngle,vSetMag,vToLinearEq,vLimit,vFromAngle,vHeading,vPerpendicularFromAng,vAngleBetween,vDegreeToRad)
where

type Vetor = (Double,Double)

{-|Adiciona dois vetores.-}
vAdd :: Vetor -> Vetor -> Vetor
vAdd (x,y) (a,b)
  = (x+a, y+b)

{-|Adiciona todos os vetores de uma lista.-}
vAddList :: [Vetor] -> Vetor
vAddList [] = (0,0)
vAddList (x:xs) = vAdd x (vAddList xs)

{-|Multiplica um vetor por um escalar.-}
vMult :: Vetor -> Double -> Vetor
vMult (x,y) a
  = (x*a, y*a)

{-|Multiplicar todos os vetores de uma lista por um escalar.-}
vMultList :: [Vetor] -> Double -> [Vetor]
vMultList [x] a = [vMult x a]
vMultList (x:xs) a = (vMult x a):(vMultList xs a)

{-|Determina a magnitude de um vetor.-}
vMag :: Vetor -> Double
vMag (x,y)
  = sqrt (x^2 + y^2)

{-|Calcula a distância entre dois vetores.-}
vDist :: Vetor -> Vetor -> Double
vDist v1 v2
  = abs (vMag v1 - vMag v2)

{-|Faz o produto escalar entre dois vetores.-}
vDot :: Vetor -> Vetor -> Double
vDot (x,y) (a,b)
  = x*a + y*b

{-|Normaliza um vetor.-}
vNormalize :: Vetor -> Vetor
vNormalize (x,y)
  = (x/len, y/len)
  where
  len = vMag (x,y)

{-|Limita um vetor para que tenha uma magnitude máxmia.-}
vLimit :: Double -> Vetor -> Vetor
vLimit a (x,y)
  |mag > a   = (x*scale, y*scale)
  |otherwise = (x,y)
  where
  mag = vMag (x,y)
  scale = a/mag

{-|Altera a magnitude de um vetor para a magnitude pretendida.-}
vSetMag :: Double -> Vetor -> Vetor
vSetMag a (x,y)
  = (x*scale, y*scale)
  where
  mag = vMag (x,y)
  scale = a/mag

{-|Vetor normalizado com certo angulo relativamente ao eixo x.-}
vFromAngle :: Double -> Vetor
vFromAngle a
  = (cos (a), sin (a))

{-|Angulo para o qual o vetor está virado.-}
vHeading :: Vetor -> Double
vHeading (x,y)
  |x>=0 && y>=0 = ang
  |x<=0 && y>=0 = ang + pi
  |x>=0 && y<=0 = ang
  |x<=0 && y<=0 = ang + pi
  where
  decl = y/x
  ang  = atan decl

{-|Vetor perpendicular a uma linha com certo angulo.-}
vPerpendicularFromAng :: Double -> Vetor
vPerpendicularFromAng ang = vFromAngle (ang + pi/2)

{-|Angulo entre dois angulos.-}
vAngleBetween :: Vetor -> Vetor -> Double
vAngleBetween v1 v2 = acos ((vDot v1 v2)/(vMag v1 * vMag v2))

{-|Graus para radianos.-}
vDegreeToRad :: Double -> Double
vDegreeToRad a = (a*pi)/180

{-|Radianos para graus-}
vRadToDegree :: Double -> Double
vRadToDegree a = (a*180)/pi

{-|Inverte o valor y de um vetor-}
vInvertY :: Vetor -> Vetor
vInvertY (x,y) = (x,-y)

{-|Devolve a equação linear que corresponde à linha em que um vetor se encontra.-}
vToLinearEq :: Double -> Vetor -> (Double,Double)
vToLinearEq angle (x,y)
  = (a,b)
  where
  a = atan angle
  b = y- x*a

{-|Normaliza um ângulo em radianos entre 180 e -180-}
vNormalizeAngle :: Double -> Double
vNormalizeAngle ang
  |sig > 0 = ang - toSub
  |sig < 0 = ang + toSub
  |sig ==0 = 0
  where
  sig = signum ang
  toSub = 2*pi * (fromIntegral $ ceiling $ abs $ ang - (2*pi))

{-|Normaliza um ângulo em graus-}
vNormalizeDegrees :: Double -> Double
vNormalizeDegrees a
  |a>180 = a-360
  |a<(-180) = a+360
  |otherwise = a


vCrossProduct3by3 :: (Float,Float,Float) -> (Float,Float,Float) -> (Float,Float,Float)
vCrossProduct3by3 (ax,ay,az) (bx,by,bz)
  = ((ay*bz)-(az*by),(az*bx)-(ax*bz),(ax*by)-(ay*bx))