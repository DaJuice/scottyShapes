{-# LANGUAGE OverloadedStrings #-}

import SVGInterpreter
import Control.Monad
import System.IO
import Shapes
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Web.Scotty
import qualified Data.Text.Lazy as DataLazy

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do 
    	middleware $ staticPolicy (noDots >-> addBase "static")
    	file "index.html"


    post "/result" $ do
        userInput <- param "input"
        let drawing = read userInput :: [(Transform, Shape, [Style])] -- Cast the read to be of the "Drawing" type
        -- renderSvg is a function from the SVG library which produces a native Haskell string
        html $ DataLazy.pack $ renderSvg $ drawingToSVG $ drawing