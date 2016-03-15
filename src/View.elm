module View where

import Graphics.Collage exposing (collage, square, filled)
import Html
import Color exposing (rgb)

import Model exposing (..)

render : Signal.Address Coordinates -> GameState -> Html.Html
render coordinates gameState =
  Html.fromElement <| collage 100 100 [ filled gray <| square 100 ]

blue = rgb 96 181 204
gray = rgb 90 99 120
green = rgb 127 209 59
orange = rgb 240 173 0
