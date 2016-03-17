module GameLogic.UpdateStateTests where

import ElmTest exposing (Test, assertEqual, suite, test)

import GameModel exposing (Coordinates, GameState, Move, Player (X, O), Status (InProgress, Tied, Won))
import TestHelpers exposing (x, o)

import GameLogic.UpdateState exposing (makeMove, otherPlayer)


all : Test
all =
    suite "Updating the game state"

        [ suite "Making a move"

            [ test "Records the move made in a new game state" <|
                assertEqual [ Move (Coordinates 0 0) X ]
                    <| .movesSoFar
                    <| makeMove (Coordinates 0 0)
                    <| GameState 3 X [] InProgress

            , test "Returns a game state in which the other player is now the current player" <|
                assertEqual (otherPlayer X)
                    <| .currentPlayer
                    <| makeMove (Coordinates 0 0)
                    <| GameState 3 X [] InProgress

            , test "Returns an unchanged game state when a move already exists at coordinates" <|
                let
                    gameState = GameState 3 (otherPlayer X) [ Move (Coordinates 0 0) X ] InProgress
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , test "Returns an unchanged game state when given game state status is Tied" <|
                let
                    gameState = GameState 3 X [] Tied
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , test "Returns an unchanged game state when given game state status is Won" <|
                let
                    gameState = GameState 3 X [] (Won X)
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , suite "Updating the game status"

                [ test "Game state has status InProgress if game is not over after move is made" <|
                    assertEqual InProgress
                        <| .status
                        <| makeMove (Coordinates 0 0)
                        <| GameState 3 X [] InProgress

                , test "Game state has status Tied if game is tied after move is made" <|
                    let
                        moves =
                            [ x 0 0, x 0 1, o 0 2
                            , o 1 0, o 1 1, x 1 2
                            , x 2 0, o 2 1
                            ]
                    in
                        assertEqual Tied
                            <| .status
                            <| makeMove (Coordinates 2 2)
                            <| GameState 3 X moves InProgress

                , test "Returns a game state with status Won X if player X won the game" <|
                    let
                        moves =
                            [ x 0 0, x 0 1
                            , o 1 0, o 1 1
                            ]
                    in
                        assertEqual (Won X)
                            <| .status
                            <| makeMove (Coordinates 0 2)
                            <| GameState 3 X moves InProgress

                , test "Returns a game state with status Won O if player O won the game" <|
                    let
                        moves =
                            [ o 0 0, o 0 1
                            , x 1 0, x 1 1
                            ]
                    in
                        assertEqual (Won O)
                            <| .status
                            <| makeMove (Coordinates 0 2)
                            <| GameState 3 O moves InProgress
                ]
            ]
        , suite "Getting the other player"

            [ test "Given X, the other player is O" <|
                assertEqual X (otherPlayer O)

            , test "Given O, the other player is X" <|
                assertEqual O (otherPlayer X)
            ]
        ]
