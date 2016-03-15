module View where

import Styles exposing (
    boardSpaceSize,
    boardSpaceMargin,
    boardSpaceColor,
    boardSpaceMarkStyle,
    boardBackgroundColor
  )
import Text exposing (fromString, style)
import Graphics.Collage exposing (collage, square, filled, toForm, move)
import Graphics.Element exposing (centered, container, middle, down, flow)
import Html

import Model exposing (Player (X, O), GameState, Coordinates, Move)

render : Signal.Address Coordinates -> GameState -> Html.Html
render coordinates gameState =
  Html.fromElement <| container (boardWidth + 100) (boardWidth + 100) middle drawGameBoard

boardSize = 3
boardWidth = boardSize * boardSpaceSize + (boardSize + 1) * boardSpaceMargin

drawGameBoard : Graphics.Element.Element
drawGameBoard =
  let
    background = filled boardBackgroundColor <| square boardWidth
  in
    collage boardWidth boardWidth <| background :: boardSpaces

boardSpaces : List Graphics.Collage.Form
boardSpaces =
  let
    boardCoordinates = [ (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2) ]
  in
    List.map2 positionBoardSpacesInGrid (List.repeat 9 (drawBoardSpace (Just O))) boardCoordinates

positionBoardSpacesInGrid : Graphics.Element.Element -> (Float, Float) -> Graphics.Collage.Form
positionBoardSpacesInGrid space (row, col) =
  let
    horizontalShift = (boardSpaceSize + boardSpaceMargin) * (row - (boardSize - 1) / 2)
    verticalShift = -1 * (boardSpaceSize + boardSpaceMargin) * (col - (boardSize - 1) / 2)
  in
    move (horizontalShift, verticalShift) <| toForm <| space

drawBoardSpace : Maybe Player -> Graphics.Element.Element
drawBoardSpace player =
  let
    background = filled boardSpaceColor <| square boardSpaceSize
  in
    collage boardSpaceSize boardSpaceSize [ background, playerMark player ]

playerMark : Maybe Player -> Graphics.Collage.Form
playerMark player =
  let
    playerString = case player of
      Just X -> "X"
      Just O -> "O"
      Nothing -> " "
  in
    toForm <| centered <| style boardSpaceMarkStyle <| fromString playerString
