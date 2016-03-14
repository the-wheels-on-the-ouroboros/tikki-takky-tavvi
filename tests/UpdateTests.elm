module UpdateTests where

import ElmTest exposing (..)

import Model exposing (..)
import Update exposing (..)

all : Test
all =
  suite "Updating the game model" <|
    [
      suite "Making a move" <|
        let
          player = X
          initialState = GameState player []
          coordinates = Coordinates 0 0
        in
          [
            test "Records the move made in a new game state" <|
              assertEqual
                [ Move coordinates player ]
                (makeMove coordinates initialState).movesSoFar,

            test "Returns a game state in which the other player is now the current player" <|
              assertEqual
                (other player)
                (makeMove coordinates initialState).currentPlayer
          ]
    ]
