module Update where

import Model exposing (GameState, Coordinates, Move, other, playerWhoMovedAt)

makeMove : Coordinates -> GameState -> GameState
makeMove coordinates gameState =
  case playerWhoMovedAt coordinates gameState of
    Just _ -> gameState
    Nothing -> {
        gameState |
          currentPlayer = other gameState.currentPlayer,
          movesSoFar = Move coordinates gameState.currentPlayer :: gameState.movesSoFar
      }

isGameOver : GameState -> Bool
isGameOver gameState =
  List.any (isWinningLine gameState.boardSize) (getLines gameState)
    || (gameState.boardSize^2) == (List.length gameState.movesSoFar)

isWinningLine : Int -> List Move -> Bool
isWinningLine lineLength line =
  case line of
    firstMove::_ ->
      (List.length line) == lineLength && List.all (\move -> move.player == firstMove.player) line
    _ ->
      False

getLines : GameState -> List (List Move)
getLines gameState =
  let
    maxIndex = gameState.boardSize - 1
    moves = gameState.movesSoFar
    indices = [0..maxIndex]
  in
    List.concat [
      List.map (\row-> List.filter (\move -> move.coordinates.row == row) moves) indices,
      List.map (\col-> List.filter (\move -> move.coordinates.col == col) moves) indices,
      [ List.filter (\move -> move.coordinates.row == move.coordinates.col) moves ],
      [ List.filter (\move -> move.coordinates.row + move.coordinates.col == maxIndex) moves ]
    ]
