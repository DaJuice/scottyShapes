module Shapes(
  Shape(..),
  Vector(..),
  Matrix(..),
  Transform(..),
  Drawing,
  Colour(..),
  Style(..)
  )  where

-- ****************************************************************************
-- *                  {- Defining the Colour functionality -}                 *
-- *       Colour is a new Datatype that takes three int parameters (RGB)     *
-- *                    Each integer is of the range 0 - 255                  *
-- ****************************************************************************
data Colour = Colour Int Int Int
    deriving (Read,Show)

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

-- Added in some nice record syntax to allow accessing the x/y coords of the vect
data Vector = Vector
    { x :: Double
    , y :: Double
    }
    deriving (Show, Read)
--vector = Vector
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
    deriving (Show, Read)

-- Shapes
data Shape = Empty 
    | Circle Int Int Float -- in the form x y r 
    | Square Int Int Float Float -- in the form x y h w
    deriving (Show, Read)

-- Transformations
data Transform = Identity
    | Translate Vector
    | Scale Vector
    | Compose Transform Transform
    | Rotate Matrix
    deriving (Show, Read)

(<+>) :: Transform -> Transform -> Transform
t0 <+> t1 = Compose t0 t1

-- Drawings
-- Need to alter a Drawing to take some list of Stylings so that they can be applied to the SVG
type Drawing = [(Transform, Shape, [Style])]
