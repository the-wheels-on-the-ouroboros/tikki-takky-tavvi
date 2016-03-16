module UpdateTests where

import ElmTest exposing (..)

import Model exposing (
    GameState,
    Coordinates,
    Move,
    Player (X, O),
    Status (InProgress, Tied, Won)
  )
import Update exposing (makeMove, isGameOver, otherPlayer)

all : Test
all =
  suite "Updating the game model" <|
    [
      suite "Making a move" <|
        let
          player = X
          targetCoordinates = (Coordinates 0 0)
        in
          [
            suite "When a move has not been made at target coordinates" <|
              let
                gameState = GameState 3 player [] InProgress
              in
                [
                  test "Records the move made in a new game state" <|
                    assertEqual
                      [ Move targetCoordinates player ]
                      (makeMove targetCoordinates gameState).movesSoFar,

                  test "Returns a game state in which the other player is now the current player" <|
                    assertEqual
                      (otherPlayer player)
                      (makeMove targetCoordinates gameState).currentPlayer
                ],
            suite "When a move has already been made at target coordinates" <|
              let
                preexistingMove = Move targetCoordinates player
                gameState = GameState 3 (otherPlayer player) [ preexistingMove ] InProgress
              in
                [
                  test "Returns an unchanged game state" <|
                    assertEqual gameState <| makeMove targetCoordinates gameState
                ]
          ],
      suite "Getting the other player" <|

        [ test "Given X, the other player is O" <|
            assertEqual X <| otherPlayer O

        , test "Given O, the other player is X" <|
            assertEqual O <| otherPlayer X
        ],
      suite "Checking if the game is over" <|

        [ test "Returns true if a player has won" <|
          let
            moves = [
              Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) X,
              Move (Coordinates 1 0) O, Move (Coordinates 1 1) O
            ]
            gameState = GameState 3 O moves InProgress
          in
            assertEqual True <| isGameOver gameState

        , test "Returns true if the game ended in a tie" <|
          let
            moves = [
              Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) O,
              Move (Coordinates 1 0) O, Move (Coordinates 1 1) O, Move (Coordinates 1 2) X,
              Move (Coordinates 2 0) X, Move (Coordinates 2 1) O, Move (Coordinates 2 2) X
            ]
            gameState = GameState 3 O moves InProgress
          in
            assertEqual True <| isGameOver gameState

        , test "Returns false if no moves have been made" <|
            assertEqual False <| isGameOver <| GameState 3 X [] InProgress

        , test "Returns false if the game has not been won or tied" <|
          let
            moves = [
              Move (Coordinates 0 0) X, Move (Coordinates 0 1) O, Move (Coordinates 0 2) X,
              Move (Coordinates 1 0) O, Move (Coordinates 1 1) X
            ]
            gameState = GameState 3 O moves InProgress
          in
            assertEqual False <| isGameOver gameState
        ]
    ]
