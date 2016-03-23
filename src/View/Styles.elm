module View.Styles where

import Color exposing (Color)
import Text


backgroundColor : Color
backgroundColor = blue


boardColor : Color
boardColor = green


buttonColor : Color
buttonColor = green


buttonHeight : Int
buttonHeight = buttonWidth // 3


buttonWidth : Int
buttonWidth = spaceSize + spaceMargin


buttonTextStyle : Text.Style
buttonTextStyle =
    { defaultTextStyle
    | height = (Just 30)
    , color = gray
    }


defaultTextStyle : Text.Style
defaultTextStyle =
    { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
    , height = (Just 50)
    , color = Color.rgb 0 0 0
    , bold = True
    , italic = False
    , line = Nothing
    }


overlayColor : Color
overlayColor = transparentOrange


overlayTextStyle : Text.Style
overlayTextStyle = { defaultTextStyle | color = gray }


spaceColor : Color
spaceColor = gray


spaceMargin : Int
spaceMargin = 20


spaceSize : Int
spaceSize = 100


spaceMarkStyle : Text.Style
spaceMarkStyle = { defaultTextStyle | color = orange }


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
