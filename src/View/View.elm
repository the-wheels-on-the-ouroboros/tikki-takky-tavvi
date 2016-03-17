module View.View where

import Graphics.Element as Element exposing (Element)
import Html exposing (Html)

import GameModel exposing (Coordinates, GameState)
import View.GameBoard as GameBoard
import View.Styles as Styles
import View.Utilities as ViewUtil


render : Signal.Address Coordinates -> GameState -> Html
render moveAddress gameState =
    let
        containerWidth = (ViewUtil.calculateBoardWidth gameState) + Styles.spaceMargin
    in
        Html.fromElement
            <| Element.color Styles.backgroundColor
            <| Element.container containerWidth containerWidth Element.middle
            <| GameBoard.create moveAddress gameState
