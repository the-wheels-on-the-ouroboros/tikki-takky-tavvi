module View.Styles where

import Color exposing (Color)
import Text


backgroundColor : Color
backgroundColor = green


boardColor : Color
boardColor = blue


overlayColor : Color
overlayColor = transparentOrange


overlayTextStyle : Text.Style
overlayTextStyle =
    { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
    , height = Just <| 50
    , color = gray
    , bold = True
    , italic = False
    , line = Nothing
    }


spaceColor : Color
spaceColor = gray


spaceMargin : Int
spaceMargin = 20


spaceSize : Int
spaceSize = 100


spaceMarkStyle : Text.Style
spaceMarkStyle =
    { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
    , height = Just <| 50
    , color = orange
    , bold = True
    , italic = False
    , line = Nothing
    }


blue : Color
blue = Color.rgb 96 181 204


gray : Color
gray = Color.rgb 90 99 120


green : Color
green = Color.rgb 127 209 59


orange : Color
orange = Color.rgb 240 173 0

transparentOrange : Color
transparentOrange = Color.rgba 240 173 0 0.75
