module View.View (render) where

import Graphics.Element as Element exposing (Element, Position)
import Html exposing (Html)

import GameLogic.Update exposing (Action)
import GameModel exposing (GameState)
import View.GameBoard as GameBoard
import View.ResetButton as ResetButton
import View.Styles as Styles
import View.Title as Title
import View.Utilities as ViewUtil


render : Signal.Address Action -> GameState -> Html
render address gameState =
    let
        containerWidth = pad (pad (ViewUtil.calculateBoardWidth gameState))
        title = placeInContainer containerWidth Styles.titleHeight Element.midBottom
            <| Title.create
        gameBoard = placeInContainer containerWidth containerWidth Element.middle
            <| GameBoard.create address gameState
        resetButton = placeInContainer containerWidth (pad Styles.buttonHeight) Element.midTop
            <| ResetButton.create address

    in
        Html.fromElement (Element.flow Element.down [ title, gameBoard, resetButton ])


pad : Int -> Int
pad dimension =
    dimension + Styles.spaceMargin


placeInContainer : Int -> Int -> Position -> Element -> Element
placeInContainer width height position element =
    Element.color Styles.backgroundColor (Element.container width height position element)
