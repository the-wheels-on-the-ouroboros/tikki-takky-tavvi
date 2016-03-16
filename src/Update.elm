module Update where

import Model exposing (
    GameState,
    Coordinates,
    Move,
    Player(X, O),
    Status(InProgress, Tied, Won)
  )

makeMove : Coordinates -> GameState -> GameState
makeMove coordinates gameState =
  let
    newGameState =
      { gameState |
        currentPlayer = otherPlayer gameState.currentPlayer,
        movesSoFar = Move coordinates gameState.currentPlayer :: gameState.movesSoFar
      }
  in
    case Model.playerWhoMovedAt coordinates gameState of
      Just _ ->
        gameState
      Nothing ->
        updateGameStatus newGameState

updateGameStatus : GameState -> GameState
updateGameStatus gameState =
  case winningPlayer gameState of
    Just player ->
      { gameState | status = Won player }
    Nothing ->
      if isGameOver gameState
        then { gameState | status = Tied }
        else { gameState | status = InProgress }

otherPlayer : Player -> Player
otherPlayer player =
  case player of
    X -> O
    O -> X

winningPlayer : GameState -> Maybe Player
winningPlayer gameState =
  let
    winningLines = List.filter (isWinningLine gameState.boardSize) (getLines gameState)
  in
    case winningLines of
      line::_ ->
        case line of
          move::_ -> Just move.player
          [] -> Nothing
      [] ->
        Nothing

isGameOver : GameState -> Bool
isGameOver gameState =
  let
    isGameWon = List.any (isWinningLine gameState.boardSize) (getLines gameState)
    isGameTied = (gameState.boardSize^2) == (List.length gameState.movesSoFar)
  in
    isGameWon || isGameTied

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
  in
    List.concat
      [ List.map (\row-> List.filter (\move -> move.coordinates.row == row) moves) [0..maxIndex]
      , List.map (\col-> List.filter (\move -> move.coordinates.col == col) moves) [0..maxIndex]
      , [ List.filter (\move -> move.coordinates.row == move.coordinates.col) moves ]
      , [ List.filter (\move -> move.coordinates.row + move.coordinates.col == maxIndex) moves ]
      ]
