module View where

import Styles exposing (
    boardSpaceSize,
    boardSpaceMargin,
    boardSpaceColor,
    boardSpaceMarkStyle,
    boardBackgroundColor
  )
import Graphics.Collage exposing (collage, square, filled, toForm, move)
import Graphics.Element exposing (centered, container, middle, down, right, flow)
import Html
import Text

import Model exposing (Player (X, O), GameState, Coordinates, Move)

render : Signal.Address Coordinates -> GameState -> Html.Html
render coordinates gameState =
  let
    containerWidth = (gameState.boardSize + 1) * (boardSpaceMargin + boardSpaceSize)
  in
    Html.fromElement <| container containerWidth containerWidth middle <| drawGameBoard gameState

drawGameBoard : GameState -> Graphics.Element.Element
drawGameBoard gameState =
  flow down <| List.map (flow right) <| createBoardSpaces gameState

createBoardSpaces : GameState -> List (List Graphics.Element.Element)
createBoardSpaces gameState =
  let
    boardCoordinates = Model.boardCoordinates gameState
    coordinatesForRow = \coordinates row ->
      List.filter (\c -> c.row == row) coordinates
    partitionedCoordinatesByRow =
      List.map (coordinatesForRow boardCoordinates) [0..(gameState.boardSize - 1)]
  in
    List.map
      (List.map (\coordinates -> createBoardSpace coordinates gameState))
      partitionedCoordinatesByRow

createBoardSpace : Coordinates -> GameState -> Graphics.Element.Element
createBoardSpace coordinates gameState =
  let
    paddedSize = boardSpaceSize + boardSpaceMargin
    padding = filled boardBackgroundColor <| square paddedSize
    background = filled boardSpaceColor <| square boardSpaceSize
    playerMark = toForm <| createPlayerMark <| Model.playerWhoMovedAt coordinates gameState
  in
    collage paddedSize paddedSize [ padding, background, playerMark ]

createPlayerMark : Maybe Player -> Graphics.Element.Element
createPlayerMark player =
  let
    playerString = case player of
      Just X -> "X"
      Just O -> "O"
      Nothing -> " "
  in
    centered <| Text.style boardSpaceMarkStyle <| Text.fromString playerString
