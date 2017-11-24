{-# LANGUAGE OverloadedStrings #-}

import SVGInterpreter
import Shapes
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Web.Scotty
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Data.Text.Lazy as L

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do html $ landingPage
        --file "app/index.html"

    get "/result" $ do
        userInput <- param "input"
        -- renderSvg is a function from the SVG library which produces a native Haskell string
        html $ L.pack $ renderSvg $ createSVG $ (read userInput :: Drawing)

defaultSvg :: H.Html
defaultSvg = "[(Compose (Scale (Vector {x = 2.0, y = 2.0})) (Translate (Vector {x = 2.5, y = 2.5})),Circle 12 13 5.0,[FillCol (Colour 123 123 123),StrokeWidth 1.1,StrokeCol (Colour 50 50 50)])]"    

landingPage :: L.Text
landingPage = do
    R.renderHtml $ do
        H.head $ do
            H.title "Haskell Shapes"
            H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"
        H.body $ do
            H.div ! A.class_ "container" $ do
                H.div ! A.class_ "jumbotron text-center" $ do
                    H.h3 "Scotty Shape Webapp"
                    H.p "Use the form below to submit your shape for drawing!"
                    H.br
                    H.h2 "Example"
                    H.p "[(Compose (Scale (Vector {x = 2.0, y = 2.0})) (Translate (Vector {x = 2.5, y = 2.5})),Circle 12 13 5.0,[FillCol (Colour 123 123 123),StrokeWidth 1.1,StrokeCol (Colour 50 50 50)])]"
                H.form ! A.class_ "form-group" ! A.action "/result" $ do
                    H.label ! A.for "input" $ "Shape input:"
                    H.br
                    H.textarea ! A.id "input" ! A.class_ "form-control" ! A.name "input" ! A.placeholder "DSL input goes here in the form [(Transform, Shape, [Style])]" $ defaultSvg
                    H.br
                    H.input ! A.class_ "btn btn-primary" ! A.type_ "submit" ! A.value "Submit"
