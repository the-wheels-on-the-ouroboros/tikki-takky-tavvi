module View.GameBoard (create) where

import Color exposing (Color)
import Graphics.Collage as Collage exposing (Form)
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameLogic.Update exposing (Action (MoveInput))
import GameModel exposing (Coordinates, GameState, Move, Player (X, O))
import View.Overlay as Overlay
import View.Styles as Styles
import View.Utilities as ViewUtil


create : Signal.Address Action -> GameState -> Element
create address gameState =
    Overlay.applyBoardOverlay gameState
        <| Element.flow Element.down
        <| List.map (Element.flow Element.right)
        <| createBoardSpaces address gameState


createBoardSpaces : Signal.Address Action -> GameState -> List (List Element)
createBoardSpaces address gameState =
    let
        boardCoordinates = GameModel.boardCoordinates gameState
        coordinatesForRow = \coordinates row -> List.filter (\c -> c.row == row) coordinates
    in
        List.map
            (List.map (\coordinates -> createBoardSpace coordinates address gameState))
            (List.map (coordinatesForRow boardCoordinates) [0..(gameState.boardSize - 1)])


createBoardSpace : Coordinates -> Signal.Address Action -> GameState -> Element
createBoardSpace coordinates address gameState =
    let
        paddedSize =
            Styles.spaceSize + Styles.spaceMargin
        padding =
            coloredSquare paddedSize Styles.boardColor
        background =
            coloredSquare Styles.spaceSize Styles.spaceColor
        playerMark =
            Collage.toForm (createPlayerMark (GameModel.playerAt coordinates gameState))
    in
        Input.clickable
            (Signal.message address (MoveInput coordinates))
            (Collage.collage paddedSize paddedSize [ padding, background, playerMark ])


createPlayerMark : Maybe Player -> Element
createPlayerMark player =
    Element.centered
        <| Text.style Styles.spaceMarkStyle
        <| Text.fromString
        <| ViewUtil.playerToString player


coloredSquare : Int -> Color -> Form
coloredSquare size color =
    Collage.filled color (Collage.square (toFloat size))
