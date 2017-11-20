{-# LANGUAGE OverloadedStrings #-}

import SVGInterpreter
import Shapes
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Web.Scotty
import qualified Data.Text.Lazy as DataLazy

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do 
        file "app/index.html"
        --html "<form action='/' method='POST'>Shape Input:<br><input type='text' name='input' style='width:1400px; font-size:18px' placeholder='DSL input goes here in the form [(Transform, Shape, [Style])]'><br><br><input type='submit' value='Submit'>"

    post "/result" $ do
        userInput <- param "input"
        let drawing = read userInput :: [(Transform, Shape, [Style])] -- Cast the read to be of the "Drawing" type
        -- renderSvg is a function from the SVG library which produces a native Haskell string
        html $ DataLazy.pack $ renderSvg $ drawingToSVG $ drawing