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
