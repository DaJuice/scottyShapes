{-# LANGUAGE OverloadedStrings #-}

import SVGInterpreter
import Shapes
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Data.Text.Lazy as DataLazy

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do 
        file "app/index.html"

    post "/result" $ do
        userInput <- param "input"
        let drawing = read userInput :: Drawing -- Cast the read to be of the "Drawing" type

        -- renderSvg is a function from the SVG library which produces a native Haskell string
        html $ DataLazy.pack $ renderSvg $ createSVG $ drawing