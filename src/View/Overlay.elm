module View.Overlay (applyBoardOverlay) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Text

import GameModel exposing (GameState, Player (X, O), Status (InProgress, Tied, Won))
import View.Styles as Styles
import View.Utilities as ViewUtil


applyBoardOverlay : GameState -> Element -> Element
applyBoardOverlay gameState gameBoard =
    let
        boardOverlay = overlay (ViewUtil.calculateBoardWidth gameState)
    in
        case gameState.status of
            InProgress ->
                gameBoard
            Tied ->
                Element.flow Element.outward [ gameBoard, boardOverlay gameTiedMessage ]
            Won player ->
                Element.flow Element.outward [ gameBoard , boardOverlay (gameWonMessage player) ]


gameWonMessage : Player -> String
gameWonMessage player = (ViewUtil.playerToString (Just player)) ++ " won!"


gameTiedMessage : String
gameTiedMessage = "You tied"


overlay : Int -> String -> Element
overlay size message =
    let
        background = ViewUtil.coloredSquare size Styles.overlayColor
        text = Collage.toForm (ViewUtil.toText message Styles.overlayTextStyle)
    in
        Collage.collage size size [ background, text ]
