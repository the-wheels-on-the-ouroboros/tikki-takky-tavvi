module Model where

type Player = X | O

type Status = InProgress | Tied | Won Player

type alias Coordinates = {
  row : Int,
  col : Int
}

type alias Move = {
  coordinates : Coordinates,
  player : Player
}

type alias GameState = {
  boardSize : Int,
  currentPlayer : Player,
  movesSoFar : List Move,
  status : Status
}

initialGameState : GameState
initialGameState = GameState 3 X [] InProgress

boardCoordinates : GameState -> List Coordinates
boardCoordinates gameState =
  let
    indices = [0..(gameState.boardSize - 1)]
  in
    List.concatMap (\row -> List.map (\col -> Coordinates row col) indices) indices

playerWhoMovedAt : Coordinates -> GameState -> Maybe Player
playerWhoMovedAt coordinates gameState =
  case List.filter (\move -> move.coordinates == coordinates) gameState.movesSoFar of
    move::_ -> Just move.player
    _ -> Nothing
