module View where

import Styles exposing (
    boardSpaceSize,
    boardSpaceMargin,
    boardSpaceColor,
    boardSpaceMarkStyle,
    boardBackgroundColor
  )
import Graphics.Collage exposing (collage, square, filled, toForm, move)
import Graphics.Element exposing (centered, container, middle, down, flow)
import Html
import Text

import Model exposing (Player (X, O), GameState, Coordinates, Move)

render : Signal.Address Coordinates -> GameState -> Html.Html
render coordinates gameState =
  let
    board = drawGameBoard gameState
    containerWidth = (calculateBoardWidth gameState) + 100
  in
    Html.fromElement <| container containerWidth containerWidth middle board

drawGameBoard : GameState -> Graphics.Element.Element
drawGameBoard gameState =
  let
    boardWidth = calculateBoardWidth gameState
    background = filled boardBackgroundColor <| square <| toFloat <| calculateBoardWidth gameState
  in
    collage boardWidth boardWidth <| background :: (createBoardSpaces gameState)

calculateBoardWidth : GameState -> Int
calculateBoardWidth gameState =
  gameState.boardSize * boardSpaceSize + (gameState.boardSize + 1) * boardSpaceMargin

createBoardSpaces : GameState -> List Graphics.Collage.Form
createBoardSpaces gameState =
  let
    boardCoordinates = Model.boardCoordinates gameState
  in
    List.map (\coordinates -> createBoardSpace coordinates gameState) boardCoordinates

createBoardSpace : Coordinates -> GameState -> Graphics.Collage.Form
createBoardSpace coordinates gameState =
  let
    background = filled boardSpaceColor <| square boardSpaceSize
    playerMark = createPlayerMark <| Model.playerWhoMovedAt coordinates gameState
    moveToPositionInBoard = positionSpaceInBoard gameState.boardSize coordinates
  in
    moveToPositionInBoard <| collage boardSpaceSize boardSpaceSize [ background, playerMark ]

positionSpaceInBoard : Int -> Coordinates -> Graphics.Element.Element -> Graphics.Collage.Form
positionSpaceInBoard boardSize coordinates boardSpace =
  let
    gridCellSize = boardSpaceSize + boardSpaceMargin
    horizontalShift = toFloat <| gridCellSize * (coordinates.row - (boardSize - 1) // 2)
    verticalShift = toFloat <| -1 * gridCellSize * (coordinates.col - (boardSize - 1) // 2)
  in
    move (horizontalShift, verticalShift) <| toForm <| boardSpace

createPlayerMark : Maybe Player -> Graphics.Collage.Form
createPlayerMark player =
  let
    playerString = case player of
      Just X -> "X"
      Just O -> "O"
      Nothing -> " "
  in
    toForm <| centered <| Text.style boardSpaceMarkStyle <| Text.fromString playerString
