

{- Topics in Functional Programming [Assignment01, Part 3] -}

This file can be thought of as both the implementation, and the documentation for part three "Web App Interface".

> {-# LANGUAGE OverloadedStrings #-}

> import SVGInterpreter
> import Shapes
> import Text.Blaze.Svg.Renderer.String (renderSvg)
> import Web.Scotty
> import Text.Blaze.Html5 ((!))
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import qualified Text.Blaze.Html.Renderer.Text as R
> import qualified Data.Text.Lazy as L

This is the bit where the magic happens. 
The first get just loads the landing page, i.e. the page containing the form where the DSL is entered and submitted
in order to draw the shapes.

> main :: IO ()
> main = scotty 3000 $ do
>     get "/" $ do html $ landingPage

This get then loads the DSL string submitted in the form and draws it on the screen.
The read just makes sure that the userInput is fed into createSVG as a "Drawing".

>     get "/result" $ do
>         userInput <- param "input"
>         html $ L.pack $ renderSvg $ createSVG $ (read userInput :: Drawing)

This is a default image that's preloaded into the form purely for testing!

> defaultSvg :: H.Html
> defaultSvg = "[(Compose(Scale (Vector 5 5))(Rotate 45), Rectangle 70 -50 100 100, [Stroke (Colour 0 0 0), StrokeWidth 3.5, Fill (Colour 255 255 255)]),(Identity, Circle 350 400 100, [Fill (Colour 255 50 50), Stroke (Colour 50 255 255), StrokeWidth 5, Shading 0.6]),(Identity, Circle 500 400 100, [Fill (Colour 50 255 50), Stroke (Colour 255 50 255), StrokeWidth 5, Shading 0.6]),(Identity, Circle 425 500 100, [Fill (Colour 50 50 255), Stroke (Colour 255 255 50), StrokeWidth 5, Shading 0.6])]"

I defined the landingPage function just to make the main easier to read and to compartmentalize the code a bit.
All that's going on here is using Blaze.Html to create the Head and Body of the html and fill it in with the form, textbox, submit button.
and general details of how to use the app.
I didn't like how unpleasant the raw Html looked so I imported Bootstrap to make it a little easier on the eyes, so all the class_ values are
just for prettifying the page really!

> landingPage :: L.Text
> landingPage = do
>     R.renderHtml $ do
>         H.head $ do
>             H.title "Haskell Shapes"
>             H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"
>         H.body $ do
>             H.div ! A.class_ "container" $ do
>                 H.div ! A.class_ "jumbotron" $ do
>                     H.h2 ! A.class_ "text-center" $ "Scotty Shape Webapp"
>                     H.p "Using the form below you can create an SVG image from different shapes, stylings and transforms"
>                     H.br
>                     H.form ! A.class_ "form-group" ! A.action "/result" $ do
>                         H.label ! A.for "input" $ "Shape input:"
>                         H.br
>                         H.textarea ! A.id "input" ! A.rows "5" ! A.class_ "form-control" ! A.name "input" ! A.placeholder "DSL input goes here in the form [(Transform, Shape, [Style])]" $ defaultSvg
>                         H.br
>                         H.input ! A.style "float: right;" ! A.class_ "btn btn-outline-success" ! A.type_ "submit" ! A.value "Submit"
>                     H.br
>                     H.h3 ! A.class_ "text-center" $ "Usage"
>                     H.p "The general format of the input is [(Transform, Shape, [Style])]."
>                     H.p "It's worth noting the syntax, as if it isn't inputted correctly the page will return nothing. As such, bracketing should be kept consistent, and if you don't want an effect, use the 'None' option rather than leaving it blank"
>                     H.p "Transform can be in the form:"
>                     H.ul ! A.class_ "list-group" $ do 
>                        H.li ! A.class_ "list-group-item" $ "Identity (No transform)"
>                        H.li ! A.class_ "list-group-item" $ "Translate (Vector a b)"
>                        H.li ! A.class_ "list-group-item" $ "Scale (Vector a b)"
>                        H.li ! A.class_ "list-group-item" $ "Rotate (Int)"
>                        H.li ! A.class_ "list-group-item" $ "Compose (TransformA TransformB)"
>                     H.br
>                     H.p "Style can be in the form:"
>                     H.ul ! A.class_ "list-group" $ do 
>                        H.li ! A.class_ "list-group-item" $ "None (no style)"
>                        H.li ! A.class_ "list-group-item" $ "Fill (Colour r g b) :: where 0 < r/g/b < 255"
>                        H.li ! A.class_ "list-group-item" $ "Shading (Double d) :: where 0 < d < 1"
>                        H.li ! A.class_ "list-group-item" $ "StrokeWidth (Float)"
>                        H.li ! A.class_ "list-group-item" $ "Stroke (Colour r g b): where 0 < r/g/b < 255"
>                        H.li ! A.class_ "list-group-item" $ "Compose (TransformA TransformB)"
>                     H.br
>                     H.p "Shape can be in the form:"
>                     H.ul ! A.class_ "list-group" $ do 
>                        H.li ! A.class_ "list-group-item" $ "Empty (no shape)"
>                        H.li ! A.class_ "list-group-item" $ "Circle (Int Int Double) :: in the form xpos, ypos, radius"
>                        H.li ! A.class_ "list-group-item" $ "Rectangle (Int Int Double Double) :: in the form xpos, ypos, width, height"
