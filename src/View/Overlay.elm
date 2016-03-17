module View.Overlay where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Text

import Model exposing (
        GameState,
        Player (X, O),
        Status (InProgress, Tied, Won)
    )
import View.Styles as Styles
import View.Utilities as ViewUtil


applyGameOverOverlay : GameState -> Element -> Element
applyGameOverOverlay gameState gameBoard =
    let
        boardWidth = gameState.boardSize * (Styles.spaceSize + Styles.spaceMargin)
        wonGameOverlay =
            \player -> overlay boardWidth ((ViewUtil.playerToString (Just player)) ++ " won!")
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
