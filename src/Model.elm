module Model where

type Player = X | O

type alias Coordinates = {
  row : Int,
  col : Int
}

type alias Move = {
  coordinates : Coordinates,
  player : Player
}

type alias GameState = {
  currentPlayer : Player,
  movesSoFar : List Move
}

other : Player -> Player
other player =
  case player of
    X -> O
    O -> X
