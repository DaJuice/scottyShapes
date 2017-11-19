module Shapes(
  Shape(..),
  Point,
  Vector(..),
  Matrix(..),
  Transform(..),
  Drawing(..),
  Colour(..),
  Style(..)
  )  where

-- Utilities

-- ****************************************************************************
-- *                  {- Defining the Colour functionality -}                 *
-- *       Colour is a new Datatype that takes three int parameters (RGB)     *
-- *                    Each integer is of the range 0 - 255                  *
-- ****************************************************************************
data Colour = Colour Int Int Int
              deriving (Show, Read)

-- ****************************************************************************
-- *           Want to define some styles we can do with the colours          *
-- *        e.g. Stroke width (Width of lines the shape is drawn with),       *
-- *            Stroke colour, and the interior colour of the shape           *
-- ****************************************************************************
data Style = None
           | StrokeWidth Float
           | StrokeCol Colour
           | FillCol Colour
           deriving (Show, Read)

data Vector = Vector Double Double
              deriving (Show, Read)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving (Show, Read)

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
             deriving (Show, Read)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving (Show, Read)

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings
-- Need to alter a Drawing to take some list of Stylings so that they can be applied to the SVG
type Drawing = [(Transform,Shape, [Style])]

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

-- Change the inside1 function to take a list of stylings to be applied
-- e.g. [StrokeWidth 0.5, StrokeColour (255, 0, 0), FillCol (0, 255, 0)]
inside1 :: Point -> (Transform, Shape, [Style]) -> Bool
inside1 p (t,s, sty) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1


distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)
