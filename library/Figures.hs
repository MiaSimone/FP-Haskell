module Figures where

data Vector3D = Vector3D
  { x :: Float
  , y :: Float
  , z :: Float
  } deriving (Show)

-- Lægge 2 vektorer sammen:
add3D :: Vector3D -> Vector3D -> Vector3D
add3D v1 v2 =
  Vector3D (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)

-- Trække 2 vektorer fra hinanden:
sub3D :: Vector3D -> Vector3D -> Vector3D
sub3D (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  Vector3D (x1 - x2) (y1 - y2) (z1 - z2)

-- Gange en vektor med et tal fx til at forlænge vektoren:
mul3D :: Vector3D -> Float -> Vector3D
mul3D (Vector3D x1 y1 z1) f =
  Vector3D (f*x1) (f*y1) (f*z1)

-- Prikproduktet
dot3D :: Vector3D -> Vector3D -> Float
dot3D (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  x1*x2 + y1*y2 + z1*z2

-- Længden af en vektor:
size3D :: Vector3D -> Float
size3D v = sqrt (dot3D v v)
-- size3D (Vector3D x' y' z') = sqrt (x'**2 + y'*y' + z'**2)

data Line3D = Line3D
  { origo :: Vector3D
  , unit :: Vector3D
  } deriving (Show)

data Sphere3D = Sphere3D
  { center :: Vector3D
  , radius :: Float
  } deriving (Show)

eye :: Vector3D
eye = Vector3D 0 0 0

sphere :: Sphere3D
sphere = Sphere3D (Vector3D 0 30 0) 2

-- Linjen fra spidsen af den første vektor til spidsen af den anden vector
lineFrom :: Vector3D -> Vector3D -> Line3D
lineFrom origin target = Line3D origin unit where
  vectorBetween = sub3D target origin -- find vector from o to t : t - o
  -- unit er længden af v
  unit = mul3D vectorBetween (1.0/size3D vectorBetween)

-- Afstanden fra linjens origin til kuglens kant
-- Fx hvis kuglens radius er 2 og y koordinat er 10, og linjen starter i 0,0 så vil afstanden være 8.
dist3D :: Sphere3D -> Line3D -> Maybe Float
dist3D (Sphere3D c r) (Line3D o u) =
-- f er det inde i kvadratroden i en andengrads ligning -- hvis f er under 0 rammer den ikke, hvis 0
-- rammer den 1 gang og over 0 rammer den 2 gange
  if f < 0 then Nothing
  else Just ((-b - sqrt f)/2.0)
  where
    -- forskellen mellem origo og center
    oc = sub3D o c
    b = 2*(dot3D u oc)
    f = b**2 - 4*(dot3D oc oc - r**2)

linePoint3D :: Line3D -> Float -> Vector3D
linePoint3D (Line3D o u) distance =
  add3D o (mul3D u distance)


reflect3D :: Sphere3D -> Line3D -> Maybe Line3D
reflect3D sphere line = fmap reflected distance where
  distance = dist3D sphere line
  reflected :: Float -> Line3D
  reflected d = Line3D target vector where
    target = linePoint3D line d
    u = unit line
    normalvektor = mul3D (sub3D target (center sphere)) (1.0/(radius sphere))
    vector = add3D u (mul3D normalvektor (-2*(dot3D u normalvektor)))

reflectedLine3D :: Sphere3D -> Line3D -> Line3D
reflectedLine3D sphere line =
  case (reflect3D sphere line) of
    Just reflection -> reflection
    Nothing -> line

data Color3D = SkyColor | GroundColor deriving (Show)

-- xi = hvor langt til siden på jorden
-- yi = hvor langt ude på jorden

reflectedColor3D :: Line3D -> Color3D
reflectedColor3D (Line3D o u)
  | (z u) >= 0 =                                 SkyColor
  | otherwise =                                  GroundColor
  where
    h = (z o) - 2
    f = -h/(z u)
    xi = (x o) + (x u)*f
    yi = (y o) + (y u)*f





