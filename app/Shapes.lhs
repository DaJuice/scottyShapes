
{- Topics in Functional Programming [Assignment01, Part 1] -}

This file can be thought of as both the implementation, and the documentation for part one "Extending the DSL".
This contents of this file are largely taken from the implementation on myModule, althrough many parts have been
removed or altered such as the Shape constructor.

> module Shapes(
>   Shape(..),
>   Vector(..),
>   Matrix(..),
>   Transform(..),
>   Drawing,
>   Colour(..),
>   Style(..),
>   getX,
>   getY
>   )  where


The definition of the "Colour" constructor is necessary for changing the colour and shading of the drawn vectors.
I went through several iterations of this, (starting off with trying to build hex values using string concatenation),
but eventually settled on this implementation as it's probably the most straightforward.
The values of the Integers are the Red, Green, and Blue channels respectively.

> data Colour = Colour Int Int Int
>     deriving (Read,Show)

I defined the new transformations as a separate datatype called "Style", mainly because these transformations are for
changing the styling of the drawn SVGs, rather than their dimensions. 
So a styling can either be:
1. Nothing
2. A stroke width, defining the width of the lines drawn
3. A stroke colour, defining the colour of the lines that are drawn
4. Or a fill colour, defining the interior colour of the shape

> data Style = None
>     | Fill Colour
>     | Shading Double
>     | StrokeWidth Float
>     | Stroke Colour
>     deriving (Show, Read)

The shapes have also been slightly modified, enabling the specification of the initial values of each shape.
I also changed Square to Rectangle, as the SVG supports the rect svg shape, and you can make prettier pictures
with rectangles.

> data Shape = Empty 
>     | Circle Int Int Float -- in the form x y r 
>     | Rectangle Int Int Float Float -- in the form x y w h
>     deriving (Show, Read)

Finally, I needed to add in the style datatype into the drawing. It made sense to make it a list of styles, so that multiple styles can be
applied to each shape

> type Drawing = [(Transform, Shape, [Style])]

Everything else is what was already defined in the Shapes.hs file from the ShapesExample3

> data Vector = Vector Double Double
>     deriving (Show, Read)

> getX :: Vector -> Double 
> getX (Vector x _) = x

> getY :: Vector -> Double
> getY (Vector _ y) = y
        
> data Matrix = Matrix Vector Vector
>     deriving (Show, Read)

> data Transform = Identity
>     | Translate Vector
>     | Scale Vector
>     | Compose Transform Transform
>     | Rotate Int
>     deriving (Show, Read)

This is the handy infix composition operator

> (<+>) :: Transform -> Transform -> Transform
> t0 <+> t1 = Compose t0 t1

