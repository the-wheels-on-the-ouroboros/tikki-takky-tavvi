module View where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Html exposing (Html)
import Text

import Model exposing (
        Coordinates,
        GameState,
        Move,
        Player (X, O)
    )
import Styles


render : Signal.Address Coordinates -> GameState -> Html
render moveAddress gameState =
    let
        containerWidth = (gameState.boardSize + 1) * (Styles.spaceMargin + Styles.spaceSize)
        gameBoard = drawGameBoard moveAddress gameState
    in
        Html.fromElement (Element.container containerWidth containerWidth Element.middle gameBoard)


drawGameBoard : Signal.Address Coordinates -> GameState -> Element
drawGameBoard moveAddress gameState =
    Element.flow Element.down
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
        playerMark = Collage.toForm
            <| createPlayerMark
            <| Model.playerWhoMovedAt coordinates gameState
    in
        Input.clickable
            (Signal.message moveAddress coordinates)
            (Collage.collage paddedSize paddedSize [ padding, background, playerMark ])


createPlayerMark : Maybe Player -> Element
createPlayerMark player =
    let
        playerString = case player of
            Just X -> "X"
            Just O -> "O"
            Nothing -> " "
    in
        Element.centered (Text.style Styles.spaceMarkStyle (Text.fromString playerString))
