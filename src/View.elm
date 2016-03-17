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
        Player (X, O),
        Status (InProgress, Tied, Won)
    )
import Styles


render : Signal.Address Coordinates -> GameState -> Html
render moveAddress gameState =
    let
        containerWidth = (gameState.boardSize + 1) * (Styles.spaceMargin + Styles.spaceSize)
    in
        Html.fromElement
            <| Element.container containerWidth containerWidth Element.middle
            <| drawGameBoard moveAddress gameState


drawGameBoard : Signal.Address Coordinates -> GameState -> Element
drawGameBoard moveAddress gameState =
    applyGameOverOverlay gameState
        <| Element.flow Element.down
        <| List.map (Element.flow Element.right)
        <| createBoardSpaces moveAddress gameState


applyGameOverOverlay : GameState -> Element -> Element
applyGameOverOverlay gameState gameBoard =
    let
        boardWidth = gameState.boardSize * (Styles.spaceSize + Styles.spaceMargin)
        wonGameOverlay =
            \player -> overlay boardWidth ((playerToString (Just player)) ++ " won!")
        tiedGameOverlay = overlay boardWidth "You tied"
    in
        case gameState.status of
            InProgress ->
                gameBoard
            Tied ->
                Element.flow Element.outward [ gameBoard, tiedGameOverlay ]
            Won player ->
                Element.flow Element.outward [ gameBoard, wonGameOverlay player ]


overlay : Int -> String -> Element
overlay elementSize message =
    Collage.collage
        elementSize
        elementSize
        [ Collage.filled Styles.overlayColor
            <| Collage.square
            <| toFloat elementSize
        , Collage.toForm
            <| Element.centered
            <| Text.style Styles.overlayTextStyle
            <| Text.fromString message
        ]


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
    Element.centered (Text.style Styles.spaceMarkStyle (Text.fromString (playerToString player)))


playerToString : Maybe Player -> String
playerToString player =
    case player of
        Just X -> "X"
        Just O -> "O"
        Nothing -> " "
