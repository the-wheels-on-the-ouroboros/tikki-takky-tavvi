module View.Utilities where

import Model exposing (GameState, Player (X, O))
import View.Styles as Styles


calculateBoardWidth : GameState -> Int
calculateBoardWidth gameState = gameState.boardSize * (Styles.spaceSize + Styles.spaceMargin)


playerToString : Maybe Player -> String
playerToString player =
    case player of
        Just X -> "X"
        Just O -> "O"
        Nothing -> " "
