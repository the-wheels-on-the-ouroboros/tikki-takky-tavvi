module Styles where

import Color exposing (rgb)

boardSpaceMargin = 20
boardSpaceSize = 100
boardSpaceColor = gray
boardBackgroundColor = blue
boardSpaceMarkStyle = {
    typeface = [ "Helvetica Neue", "Arial", "sans-serif" ],
    height = Just <| 50,
    color = orange,
    bold = True,
    italic = False,
    line = Nothing
  }

blue = rgb 96 181 204
gray = rgb 90 99 120
green = rgb 127 209 59
orange = rgb 240 173 0
