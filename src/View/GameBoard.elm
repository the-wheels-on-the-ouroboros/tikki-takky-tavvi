module View.GameBoard (create) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text

import GameModel exposing (Coordinates, GameState, Move, Player (X, O), Status (InProgress, Tied, Won))
import View.Overlay as Overlay
import View.Styles as Styles
import View.Utilities as ViewUtil


create : Signal.Address Coordinates -> GameState -> Element
create moveAddress gameState =
    Overlay.applyBoardOverlay gameState
        <| Element.flow Element.down
        <| List.map (Element.flow Element.right)
        <| createBoardSpaces moveAddress gameState


createBoardSpaces : Signal.Address Coordinates -> GameState -> List (List Element)
createBoardSpaces moveAddress gameState =
    let
        boardCoordinates = GameModel.boardCoordinates gameState
        coordinatesForRow = \coordinates row -> List.filter (\c -> c.row == row) coordinates
    in
        List.map
            (List.map (\coordinates -> createBoardSpace coordinates moveAddress gameState))
            (List.map (coordinatesForRow boardCoordinates) [0..(gameState.boardSize - 1)])


createBoardSpace : Coordinates -> Signal.Address Coordinates -> GameState -> Element
createBoardSpace coordinates moveAddress gameState =
    let
        paddedSize = Styles.spaceSize + Styles.spaceMargin
        padding = Collage.filled Styles.boardColor (Collage.square (toFloat paddedSize))
        background = Collage.filled Styles.spaceColor (Collage.square (toFloat Styles.spaceSize))
        playerMark =
            Collage.toForm
                <| createPlayerMark
                <| GameModel.playerAt coordinates gameState
    in
        Input.clickable
            (Signal.message moveAddress coordinates)
            (Collage.collage paddedSize paddedSize [ padding, background, playerMark ])


createPlayerMark : Maybe Player -> Element
createPlayerMark player =
    Element.centered
        <| Text.style Styles.spaceMarkStyle
        <| Text.fromString
        <| ViewUtil.playerToString player
