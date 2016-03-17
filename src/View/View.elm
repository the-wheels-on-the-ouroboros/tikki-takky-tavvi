module View.View where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Html exposing (Html)
import Text

import Model exposing (
        Coordinates,
        GameState,
        Move,
        Player (X, O),
        Status (InProgress, Tied, Won)
    )
import View.Overlay as Overlay
import View.Styles as Styles
import View.Utilities as ViewUtil


render : Signal.Address Coordinates -> GameState -> Html
render moveAddress gameState =
    let
        containerWidth =
            (gameState.boardSize + 1) * Styles.spaceMargin + gameState.boardSize * Styles.spaceSize
    in
        Html.fromElement
            <| Element.color Styles.backgroundColor
            <| Element.container containerWidth containerWidth Element.middle
            <| drawGameBoard moveAddress gameState


drawGameBoard : Signal.Address Coordinates -> GameState -> Element
drawGameBoard moveAddress gameState =
    Overlay.applyGameOverOverlay gameState
        <| Element.flow Element.down
        <| List.map (Element.flow Element.right)
        <| createBoardSpaces moveAddress gameState


createBoardSpaces : Signal.Address Coordinates -> GameState -> List (List Element)
createBoardSpaces moveAddress gameState =
    let
        boardCoordinates = Model.boardCoordinates gameState
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
                <| Model.playerWhoMovedAt coordinates gameState
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
