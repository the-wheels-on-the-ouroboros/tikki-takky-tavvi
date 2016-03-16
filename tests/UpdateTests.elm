module UpdateTests where

import ElmTest exposing (..)

import Model exposing (
    GameState,
    Coordinates,
    Move,
    Player (X, O),
    Status (InProgress, Tied, Won)
  )
import Update exposing (
    makeMove,
    isGameOver,
    otherPlayer,
    winningPlayer
  )

all : Test
all =
  suite "Updating the game model"

    [ suite "Making a move"

      [ test "Records the move made in a new game state" <|
        assertEqual
          [ Move (Coordinates 0 0) X ]
          (makeMove (Coordinates 0 0) (GameState 3 X [] InProgress)).movesSoFar

      , test "Returns a game state in which the other player is now the current player" <|
        assertEqual
          (otherPlayer X)
          (makeMove (Coordinates 0 0) (GameState 3 X [] InProgress)).currentPlayer

      , test "Returns unchanged game state when a move has already been made at coordinates" <|
        let
          gameState = GameState 3 (otherPlayer X) [ Move (Coordinates 0 0) X ] InProgress
        in
          assertEqual gameState (makeMove (Coordinates 0 0) gameState)

      , test "Returns a game state with status InProgress if game is not over after move is made" <|
        assertEqual InProgress (makeMove (Coordinates 0 0) (GameState 3 X [] InProgress)).status

      , test "Returns a game state with status Tied if game is tied after move is made" <|
        let
          moves =
            [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) O
            , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O, Move (Coordinates 1 2) X
            , Move (Coordinates 2 0) X, Move (Coordinates 2 1) O
            ]
        in
          assertEqual Tied (makeMove (Coordinates 2 2) (GameState 3 X moves InProgress)).status

      , test "Returns a game state with status Won X if player X won the game" <|
        let
          moves =
            [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X
            , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O
            ]
        in
          assertEqual (Won X) (makeMove (Coordinates 0 2) (GameState 3 X moves InProgress)).status

      , test "Returns a game state with status Won O if player O won the game" <|
        let
          moves =
            [ Move (Coordinates 0 0) O, Move (Coordinates 0 1) O
            , Move (Coordinates 1 0) X, Move (Coordinates 1 1) X
            ]
        in
          assertEqual (Won O) (makeMove (Coordinates 0 2) (GameState 3 O moves InProgress)).status
      ]
      , suite "Getting the other player"

        [ test "Given X, the other player is O" <|
          assertEqual X (otherPlayer O)

        , test "Given O, the other player is X" <|
          assertEqual O (otherPlayer X)
        ]
      , suite "Checking if the game is over"

        [ test "Returns true if a player has won" <|
          let
            moves =
              [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) X
              , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O
              ]
            gameState = GameState 3 O moves InProgress
          in
            assertEqual True (isGameOver gameState)

        , test "Returns true if the game ended in a tie" <|
          let
            moves =
              [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) O
              , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O, Move (Coordinates 1 2) X
              , Move (Coordinates 2 0) X, Move (Coordinates 2 1) O, Move (Coordinates 2 2) X
              ]
          in
            assertEqual True (isGameOver (GameState 3 O moves InProgress))

        , test "Returns false if no moves have been made" <|
          assertEqual False (isGameOver (GameState 3 X [] InProgress))

        , test "Returns false if the game has not been won or tied" <|
          let
            moves =
              [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) O, Move (Coordinates 0 2) X
              , Move (Coordinates 1 0) O, Move (Coordinates 1 1) X
              ]
            gameState = GameState 3 O moves InProgress
          in
            assertEqual False (isGameOver (GameState 3 O moves InProgress))
        ]
      , suite "Getting the winning player"

        [ test "Returns nothing if the game is not over" <|
          assertEqual Nothing (winningPlayer (GameState 3 X [] InProgress))

        , test "Returns nothing if the game is tied" <|
          let
            moves =
              [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) O
              , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O, Move (Coordinates 1 2) X
              , Move (Coordinates 2 0) X, Move (Coordinates 2 1) O, Move (Coordinates 2 2) X
              ]
          in
            assertEqual Nothing (winningPlayer (GameState 3 X moves InProgress))

        , test "Returns player X if they won the game" <|
          let
            moves =
              [ Move (Coordinates 0 0) X, Move (Coordinates 0 1) X, Move (Coordinates 0 2) X
              , Move (Coordinates 1 0) O, Move (Coordinates 1 1) O
              ]
          in
            assertEqual (Just X) (winningPlayer (GameState 3 O moves InProgress))

        , test "Returns player O if they won the game" <|
          let
            moves =
              [ Move (Coordinates 0 0) O, Move (Coordinates 0 1) O, Move (Coordinates 0 2) O
              , Move (Coordinates 1 0) X, Move (Coordinates 1 1) X
              ]
          in
            assertEqual (Just O) (winningPlayer (GameState 3 X moves InProgress))
        ]
    ]
