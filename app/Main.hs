{-# LANGUAGE OverloadedStrings #-}

import Shapes
import Control.Monad
import System.IO
import Data.Maybe
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as BlazeSvg
import qualified Text.Blaze.Svg11.Attributes as SvgAttr
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Web.Scotty
import qualified Data.Text.Lazy as DataLazy

main :: IO ()
main = scotty 3000 $ do
	get "/" $ do 
		file "./index.html"

	post "/result" $ do
		userInput <- param "input"
		let drawing = read userInput :: [(Transform, Shape, [Style])] -- Cast the read to be of the "Drawing" type
		-- renderSvg is a function from the SVG library which produces a native Haskell string
		html $ DataLazy.pack $ renderSvg $ drawingToSVG $ drawing


   {-							SVG INTERPRETER								 -}
-- ****************************************************************************
-- *    Here I'm going to define a series of SVG interpretation functions.    *
-- *   These functions' purpose is to convert the drawing and tranformation   *
-- *                  data types into types the SVG can use.                  *
-- ****************************************************************************
drawingToSVG :: Drawing -> BlazeSvg.Svg
drawingToSVG drawing@((_, shape, _):_)
	-- Strict evaluation is used to actually get some values from Haskell
	-- If the shape isn't defined don't draw it
	| isNothing (shapeToSVG shape) = BlazeSvg.docType ! SvgAttr.version "1.1" ! SvgAttr.width "200" ! SvgAttr.height "200"
	-- otherwise parse the drawings into SVGs
	| otherwise = BlazeSvg.docTypeSvg ! SvgAttr.version "1.1" ! SvgAttr.width "200" ! SvgAttr.height "200" $ do mapSVGs (map drawToSVG drawing)

-- Not really a map, just combines all the SVGs with a ">>" operator
mapSVGs :: [BlazeSvg.Svg] -> BlazeSvg.Svg
mapSVGs svgs = foldl1 (>>) svgs

drawToSVG :: (Transform, Shape, [Style]) -> BlazeSvg.Svg 
drawToSVG (trans, shape, [style])
	-- This bit is kind of a doozy to read, but basically it just covers the cases of getting no transformations and a styling or some transformations
	-- and some stylings, or just some stylings, and then folds a strictness evaluator over the interpreter functions.
	| null (parseTransformation trans) && length (parseStyleList [style]) > 0 = foldl (!) (fromJust (shapeToSVG shape))(parseStyleList [style])
	| length (parseTransformation trans) > 0 && length (parseStyleList [style]) > 0 = foldl (!) (foldl (!) (fromJust $ shapeToSVG shape)(parseTransList trans))(parseStyleList [style])
	| length (parseTransformation trans) > 0 && null (parseStyleList [style]) = foldl (!) (fromJust (shapeToSVG shape)) (parseTransList trans)

-- This function parses a list of transforms and returns a list of "Just" SVG Attributes
-- catMaybes is a super helpful function that returns all the "Justs" from a list of Maybes
parseTransList :: Transform -> [BlazeSvg.Attribute]
parseTransList trans = catMaybes (parseTransformation trans)

-- Need to define a function to parse our definition of a transformation and turn it into
-- a transformation that the SVG can use. Because many different transformations can be applied
-- to a shape (through composition), need to return an array of those tranformations.
parseTransformation :: Transform -> [Maybe BlazeSvg.Attribute]
parseTransformation Identity = [Nothing]
parseTransformation t = [parseTranslate t]
parseTransformation t = [parseScale t]
parseTransformation t = [parseRotate t]
parseTransformation (Compose trans1 trans2) = parseTransformation trans1 ++ parseTransformation trans2
-- .transform is the combinator for the transform attribute
-- parseTransformation (Translate (Vector x y)) = [Just (SvgAttr.transform $ BlazeSvg.translate x y)]
-- parseTransformation (Scale (Vector x y)) = [Just (SvgAttr.transform $ BlazeSvg.scale x y)]
-- parseTransformation (Rotate (Matrix (Vector a b) (Vector d c))) = [Just (SvgAttr.transform $ BlazeSvg.rotate (acos a))]
-- parseTransformation (Compose trans1 trans2) = parseTransformation trans1 ++ parseTransformation trans2

parseTranslate :: Transform -> Maybe BlazeSvg.Attribute
parseTranslate (Translate (Vector x y)) = Just (SvgAttr.transform (BlazeSvg.translate x y))

parseScale :: Transform -> Maybe BlazeSvg.Attribute
parseScale (Scale (Vector x y)) = Just (SvgAttr.transform (BlazeSvg.scale x y))

parseRotate :: Transform -> Maybe BlazeSvg.Attribute
parseRotate (Rotate (Matrix (Vector a b) (Vector d c))) = Just (SvgAttr.transform (BlazeSvg.rotate (acos a)))

-- Need to specify functions that will convert our Shape datatypes into the svg versions
-- Because "Empty" is an option for the Shape datatype, a Maybe is useful as then we can
-- return "Nothing". We also want to strictly evaluate the attributes of the shapes, hence "(!)"
shapeToSVG :: Shape -> Maybe BlazeSvg.Svg
shapeToSVG Empty = Nothing
shapeToSVG Circle = Just (BlazeSvg.circle ! SvgAttr.cx "100" ! SvgAttr.cy "100" ! SvgAttr.r "100")
shapeToSVG Square = Just ( BlazeSvg.rect ! SvgAttr.x "100" ! SvgAttr.y "100" ! SvgAttr.width "100" ! SvgAttr.height "100")

-- This function parses a list of styles and returns a list of "Just" SVG Attributes
-- catMaybes is a super helpful function that returns all the "Justs" from a list of Maybes
parseStyleList :: [Style] -> [BlazeSvg.Attribute]
parseStyleList styles = catMaybes (map styleToSvgAttr styles)

-- If the style is given in the correct format this function will return a "Just" version
-- of the style, otherwise it will return Nothing. This can be thought of as a parser for 
-- the style attributes of the svg image.
styleToSvgAttr :: Style -> Maybe BlazeSvg.Attribute
styleToSvgAttr None = Nothing
-- stringValue creates an attribute value from a String (which is what show returns)
styleToSvgAttr (StrokeWidth float) = Just (SvgAttr.strokeWidth (BlazeSvg.stringValue (show float)))
styleToSvgAttr (StrokeCol (Colour r g b)) = Just (SvgAttr.stroke (BlazeSvg.stringValue ("rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")))
styleToSvgAttr (FillCol (Colour r g b)) = Just (SvgAttr.fill (BlazeSvg.stringValue ("rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")")))