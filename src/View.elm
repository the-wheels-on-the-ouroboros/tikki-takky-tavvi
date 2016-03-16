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
import Graphics.Input exposing (clickable)
import Html
import Text

import Model exposing (Player (X, O), GameState, Coordinates, Move)

render : Signal.Address Coordinates -> GameState -> Html.Html
render moveAddress gameState =
  let
    containerWidth = (gameState.boardSize + 1) * (boardSpaceMargin + boardSpaceSize)
    gameBoard = drawGameBoard moveAddress gameState
  in
    Html.fromElement <| container containerWidth containerWidth middle gameBoard

drawGameBoard : Signal.Address Coordinates -> GameState -> Graphics.Element.Element
drawGameBoard moveAddress gameState =
  flow down <| List.map (flow right) <| createBoardSpaces moveAddress gameState

createBoardSpaces : Signal.Address Coordinates -> GameState -> List (List Graphics.Element.Element)
createBoardSpaces moveAddress gameState =
  let
    boardCoordinates = Model.boardCoordinates gameState
    coordinatesForRow = \coordinates row -> List.filter (\c -> c.row == row) coordinates
    partitionedCoordinatesByRow =
      List.map (coordinatesForRow boardCoordinates) [0..(gameState.boardSize - 1)]
  in
    List.map
      (List.map (\coordinates -> createBoardSpace coordinates moveAddress gameState))
      partitionedCoordinatesByRow

createBoardSpace : Coordinates -> Signal.Address Coordinates -> GameState -> Graphics.Element.Element
createBoardSpace coordinates moveAddress gameState =
  let
    paddedSize = boardSpaceSize + boardSpaceMargin
    padding = filled boardBackgroundColor <| square paddedSize
    background = filled boardSpaceColor <| square boardSpaceSize
    playerMark = toForm <| createPlayerMark <| Model.playerWhoMovedAt coordinates gameState
  in
    collage paddedSize paddedSize [ padding, background, playerMark ]
      |> clickable (Signal.message moveAddress coordinates)

createPlayerMark : Maybe Player -> Graphics.Element.Element
createPlayerMark player =
  let
    playerString = case player of
      Just X -> "X"
      Just O -> "O"
      Nothing -> " "
  in
    centered <| Text.style boardSpaceMarkStyle <| Text.fromString playerString
