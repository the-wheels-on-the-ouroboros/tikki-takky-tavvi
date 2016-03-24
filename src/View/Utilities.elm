module View.Utilities (calculateBoardWidth, coloredSquare, playerToString, toText) where

import Color exposing (Color)
import Graphics.Collage as Collage exposing (Form)
import Graphics.Element as Element exposing (Element)
import Text

import GameModel exposing (GameState, Player (X, O))
import View.Styles as Styles


calculateBoardWidth : GameState -> Int
calculateBoardWidth gameState = gameState.boardSize * (Styles.spaceSize + Styles.spaceMargin)


coloredSquare : Int -> Color -> Form
coloredSquare size color =
    Collage.filled color (Collage.square (toFloat size))


playerToString : Maybe Player -> String
playerToString player =
    case player of
        Just X -> "X"
        Just O -> "O"
        Nothing -> " "


toText : String -> Text.Style -> Element
toText content style =
    Element.centered (Text.style style (Text.fromString content))
