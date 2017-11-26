
{- Topics in Functional Programming [Assignment01, Part 2] -}

This file can be thought of as both the implementation, and the documentation for part two "SVG Interpretations".

> module SVGInterpreter
>     (
>         createSVG
>     )
>     where

> import Shapes
> import Text.Blaze.Svg11 ((!))
> import Data.Monoid ((<>))
> import qualified Text.Blaze.Svg11 as S
> import qualified Text.Blaze.Svg11.Attributes as A

To start off this section of documentation I'm going to define the basic functions and then build upon them, until the final
"createSVG" function, which is called in the Main.hs

Get colour is needed for when parsing the styles, as the blaze.SVG attributes needs to read a string.

> getColour :: Colour -> String
> getColour (Colour r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

This function is used to take a "Style" datatype, as defined in the Shapes.hs file, and convert it into a suitable value for
blazeSVG.
Because the Attributes are Monoids, it's useful to define the "None" styling as an "mempty". I use stringValue to take the string
value and feed it into the relevant attribute (stringValue takes a string and converts it into an AttributeValue, which is needed to 
create the SVG attributes)

> createSVGStyle :: Style -> S.Attribute
> createSVGStyle None = mempty
> createSVGStyle (Fill c) = A.fill $ S.stringValue $ getColour c
> createSVGStyle (Shading d) = A.fillOpacity $ S.stringValue (show d)
> createSVGStyle (Stroke c) = A.stroke $ S.stringValue $ getColour c
> createSVGStyle (StrokeWidth w) = A.strokeWidth $ S.stringValue (show w)

Next I define a useful helper function, which just maps the Style parsing function over a list of Styles, returning a list
of SVG attributes.

> attrList :: [Style] -> [S.Attribute]
> attrList styles = map createSVGStyle styles

Next I'll look at the functions that deal with parsing transforms

parseTransform does something similar to the Style parsing function, just taking the transform type, and converting it into
the relevant SVG transform. Once again, the use of the monoid is useful here, allowing the use of "<>" (mappend), which allows
the combining of two associative operations (definition according to Hackage).
I'm also pretty sure that the Rotate I'm using here rotates around the corner of the SVG and not the actual shape itself which
isn't ideal, but I didn't have the time to fix it

> parseTransform :: Transform -> S.AttributeValue 
> parseTransform Identity = mempty
> parseTransform (Scale vect) = S.scale (getX vect)(getY vect)
> parseTransform (Translate vect) = S.translate (getX vect)(getY vect)
> parseTransform (Rotate angl) = S.rotate angl
> parseTransform (Compose t1 t2) = parseTransform t1 <> parseTransform t2

I originally had the parseTransform and createSVGTransform together as one function.
{ e.g. parseTransform (Scale vect) = A.transform ! S.scale (getX vect)(gety vect) }
However I didn't like how it looked really, so I just use function composition to chain the two things together 
and return an SVG Attribute

> createSVGTransform :: Transform -> S.Attribute
> createSVGTransform = A.transform . parseTransform

The last of the basic parsing functions is the function to parse a shape and give an Svg.
It works pretty similar to the other functions. Using (!) to force strict evaluation, and stringValue to create AttributeValues,
I build the circle and Rectangle shapes.
This is also where the specified x/y and radius/width/height, are used to place the shape or move it around in the image

> createSVGShape :: Shape -> S.Svg
> createSVGShape Empty = mempty
> createSVGShape (Circle xpos ypos r) = S.circle ! A.cx (S.stringValue (show xpos)) ! A.cy (S.stringValue(show ypos)) ! A.r (S.stringValue(show r))
> createSVGShape (Rectangle xpos ypos w h) = S.rect ! A.x (S.stringValue(show xpos)) ! A.y (S.stringValue(show ypos)) ! A.width (S.stringValue(show w)) ! A.height (S.stringValue(show h))
 
I found my code was getting hard to read in the later functions when trying to apply transformations and styles together with the shapes
in one line, so the next two functions are just to make things easier to read.

applyTrans just applies the "Transform Attribute" to the created SVG shape,

> applyTrans :: Shape -> Transform -> S.Svg
> applyTrans shape trans = (!) (createSVGShape shape) (createSVGTransform trans)

while applyStyles applies all of the stylings in the style list to the transformed shape (which is an SVG type), returning a nicely styled
Svg shape.

> applyStyles :: S.Svg -> [Style] -> S.Svg
> applyStyles svg styles = foldl (!) svg (attrList styles)

A drawing is really just a list of Shapes with transformations and styles applied to them, so svgList takes that drawing, and converts 
it into a list of SVG shapes.
My reasoning behind applying the transformation first and then the styles was that hopefully the styles wouldn't get warped and weird looking 
(like they did when transformations were applied to the styled SVG). However they still look kind of weird if you scale the SVG really far and
apply the style.

> svgList :: Drawing -> [S.Svg]
> svgList drawing = map render drawing
>     where
>         render :: (Transform, Shape, [Style]) -> S.Svg
>         render (trans, shape, styles) = applyStyles (applyTrans shape trans) styles

This nifty little function takes that list of SVG shapes I just created and smooshes (I believe that's the technical term) into a combined SVG image,
ready for viewing. This is done with mconcat (another handy monoid function), which takes a list of monoids "[a]" and returns a single monoid "a"

> combineSVGs :: [S.Svg] -> S.Svg
> combineSVGs svgs = mconcat svgs

Finally, this function just creates the canvas and draws the SVG image created by "combineSVGs", and is the only function that really needs to be
exported.

> createSVG :: Drawing -> S.Svg
> createSVG drawing = S.docTypeSvg ! A.version "1.1" ! A.width "1600" ! A.height "1200" $ do combineSVGs $ svgList drawing