module View.View (render) where

import Graphics.Element as Element exposing (Element)
import Html exposing (Html)

import GameLogic.Update exposing (Action)
import GameModel exposing (Coordinates, GameState)
import View.GameBoard as GameBoard
import View.ResetButton as ResetButton
import View.Styles as Styles
import View.Utilities as ViewUtil


render : Signal.Address Action -> GameState -> Html
render address gameState =
    let
        containerWidth = (ViewUtil.calculateBoardWidth gameState) + Styles.spaceMargin
        gameBoard =
            Element.color Styles.backgroundColor
                <| Element.container containerWidth containerWidth Element.middle
                <| GameBoard.create address gameState
        buttonContainerHeight = (Styles.spaceMargin // 2) + Styles.buttonHeight
        resetButton =
            Element.color Styles.backgroundColor
                <| Element.container containerWidth buttonContainerHeight Element.midTop
                <| ResetButton.create address
    in
        Html.fromElement (Element.flow Element.down [ gameBoard, resetButton ])
