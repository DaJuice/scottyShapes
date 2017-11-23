
module SVGInterpreter
    (
    )
    where

import Shapes
import Text.Blaze.Svg11 ((!))
import Data.Monoid
import Data.Monoid ((<>))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

   {-                           SVG INTERPRETER                              -}
-- ****************************************************************************
-- *    Here I'm going to define a series of SVG interpretation functions.    *
-- *   These functions' purpose is to convert the drawing and tranformation   *
-- *                  data types into types the SVG can use.                  *
-- ****************************************************************************

createSVG :: Drawing -> S.Svg
createSVG drawing = S.docTypeSvg ! A.version "1.1" ! A.width "1400" ! A.height "1400" $ do combineSVGs $ blegh2 drawing

combineSVGs :: [S.Svg] -> S.Svg
combineSVGs svgs = mconcat svgs

blegh2 :: Drawing -> [S.Svg]
blegh2 drawing = map blegh drawing
	where
		blegh :: (Transform, Shape, [Style]) -> S.Svg
		blegh (trans, shape, styles) = applyStyles (applyTrans shape trans) styles

applyTrans :: Shape -> Transform -> S.Svg
applyTrans shape trans = (!) (createSVGShape shape) (createSVGTransform trans)

createSVGTransform :: Transform -> S.Attribute
createSVGTransform = A.transform . parseTransform

parseTransform :: Transform -> S.AttributeValue
parseTransform Identity = mempty
parseTransform (Scale vect) = S.scale (x vect)(y vect)
parseTransform (Translate vect) = S.translate (x vect)(y vect)
parseTransform (Rotate angl) = S.rotate angl
parseTransform (Compose t1 t2) = parseTransform t1 <> parseTransform t2

createSVGShape :: Shape -> S.Svg
createSVGShape Empty = mempty
createSVGShape (Circle xpos ypos r) = S.circle ! A.cx (S.stringValue (show xpos)) ! A.cy (S.stringValue(show ypos)) ! A.r (S.stringValue(show r))
createSVGShape (Square xpos ypos h w) = S.rect ! A.rx (S.stringValue(show xpos)) ! A.ry (S.stringValue(show ypos)) ! A.height (S.stringValue(show h)) ! A.width (S.stringValue(show w))

applyStyles :: S.Svg -> [Style] -> S.Svg
applyStyles svg styles = foldl (!) svg (attrList styles)

attrList :: [Style] -> [S.Attribute]
attrList styles = map createSVGStyle styles

createSVGStyle :: Style -> S.Attribute
createSVGStyle None = mempty
createSVGStyle (FillCol c) = A.fill $ S.stringValue $ getColour c
createSVGStyle (StrokeCol c) = A.stroke $ S.stringValue $ getColour c
createSVGStyle (StrokeWidth w) = A.strokeWidth $ S.stringValue (show w)

getColour :: Colour -> String
getColour (Colour r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"