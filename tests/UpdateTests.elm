module UpdateTests where

import ElmTest exposing (..)

import Model exposing (GameState, Coordinates, Move, Player (X, O))
import Update exposing (..)

all : Test
all =
  suite "Updating the game model" <|
    [
      suite "Making a move" <|
        let
          player = X
          gameState = GameState 3 player []
          coordinates = Coordinates 0 0
        in
          [
            test "Records the move made in a new game state" <|
              assertEqual
                [ Move coordinates player ]
                (makeMove coordinates gameState).movesSoFar,

            test "Returns a game state in which the other player is now the current player" <|
              assertEqual
                (Model.other player)
                (makeMove coordinates gameState).currentPlayer
          ]
    ]
