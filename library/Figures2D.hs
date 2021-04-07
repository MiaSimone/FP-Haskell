module Figures2D where

data Vector2D = Vector2D
  { x :: Float
  , y :: Float
  } deriving (Show)

-- Trække 2 vektorer fra hinanden:
sub2D :: Vector2D -> Vector2D -> Vector2D
sub2D (Vector2D x1 y1) (Vector2D x2 y2) =
  Vector2D (x1 - x2) (y1 - y2)

-- Gange en vektor med et tal fx til at forlænge vektoren:
mul2D :: Vector2D -> Float -> Vector2D
mul2D (Vector2D x1 y1) f =
  Vector2D (f*x1) (f*y1)


-- Prikproduktet
dot2D :: Vector2D -> Vector2D -> Float
dot2D (Vector2D x1 y1) (Vector2D x2 y2) =
  x1*x2 + y1*y2

-- Længden af en vektor:
size2D :: Vector2D -> Float
size2D v = sqrt (dot2D v v)

data Line2D = Line2D
  { origo :: Vector2D
  , unit :: Vector2D
  } deriving (Show)

eye :: Vector2D
eye = Vector2D 0 0

lines :: Vector2D -> Line2D
lines origin target = Line2D origo unit where
    vectorBetween = sub2D target origin -- find vector from o to t : t - o
    -- unit er længden af v
    unit = mul2D vectorBetween (1.0/size2D vectorBetween)


data Color2D = BlueColor | YellowColor deriving (Show)

-- xi = hvor langt til siden på jorden
-- yi = hvor langt ude på jorden

lineColor :: Line2D -> Color2D
lineColor (Line2D o u)
  | u >= 0 =                                 BlueColor
  | otherwise =                              YellowColor
