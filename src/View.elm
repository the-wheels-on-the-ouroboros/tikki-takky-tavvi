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
  Html.fromElement <| container (boardWidth + 100) (boardWidth + 100) middle <| drawGameBoard gameState

boardSize = 3
boardWidth = boardSize * boardSpaceSize + (boardSize + 1) * boardSpaceMargin

drawGameBoard : GameState -> Graphics.Element.Element
drawGameBoard gameState =
  let
    background = filled boardBackgroundColor <| square boardWidth
  in
    collage boardWidth boardWidth <| background :: (createBoardSpaces gameState)

createBoardSpaces : GameState -> List Graphics.Collage.Form
createBoardSpaces gameState =
  let
    boardCoordinates = Model.boardCoordinates gameState
  in
    List.map2
      positionBoardSpacesInGrid
      (List.map (\spaceCoordinates -> drawBoardSpace spaceCoordinates gameState) boardCoordinates)
      boardCoordinates

positionBoardSpacesInGrid : Graphics.Element.Element -> Coordinates -> Graphics.Collage.Form
positionBoardSpacesInGrid space coordinates =
  let
    gridCellSize = boardSpaceSize + boardSpaceMargin
    horizontalShift = gridCellSize * ((toFloat coordinates.row) - (boardSize - 1) / 2)
    verticalShift = -1 * gridCellSize * ((toFloat coordinates.col) - (boardSize - 1) / 2)
  in
    move (horizontalShift, verticalShift) <| toForm <| space

drawBoardSpace : Coordinates -> GameState -> Graphics.Element.Element
drawBoardSpace coordinates gameState =
  let
    background = filled boardSpaceColor <| square boardSpaceSize
    playerMark = createPlayerMark <| Model.playerWhoMovedAt coordinates gameState
  in
    collage boardSpaceSize boardSpaceSize [ background, playerMark ]

createPlayerMark : Maybe Player -> Graphics.Collage.Form
createPlayerMark player =
  let
    playerString = case player of
      Just X -> "X"
      Just O -> "O"
      Nothing -> " "
  in
    toForm <| centered <| Text.style boardSpaceMarkStyle <| Text.fromString playerString
