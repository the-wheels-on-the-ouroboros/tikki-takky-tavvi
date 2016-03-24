module GameLogic.HandleTurnTests (all) where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import GameModel exposing
    ( Coordinates
    , GameState
    , Move
    , Player (X, O)
    , Status (InProgress, Tied, Won)
    )
import TestHelpers exposing (x, o)

import GameLogic.HandleTurn exposing (makeMove, nextPlayer)


all : Test
all =
    suite "Handling a turn"

        [ suite "Making a move"

            [ test "Returns a game state in which a move has been made at given coordinates" <|
                assertEqual [ Move (Coordinates 0 0) X ]
                    <| .movesSoFar
                    <| makeMove (Coordinates 0 0)
                    <| GameState 3 False X [] InProgress

            , test "Returns a game state in which the next player is now the current player" <|
                assertEqual (nextPlayer X)
                    <| .currentPlayer
                    <| makeMove (Coordinates 0 0)
                    <| GameState 3 False X [] InProgress

            , test "Does not make move if a move already exists at coordinates" <|
                let
                    gameState = GameState 3 False O [ Move (Coordinates 0 0) X ] InProgress
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , test "Does not make move if given game state status is Tied" <|
                let
                    gameState = GameState 3 False X [] Tied
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , test "Does not make move if given game state status is Won" <|
                let
                    gameState = GameState 3 False X [] (Won X)
                in
                    assertEqual gameState (makeMove (Coordinates 0 0) gameState)

            , suite "Updating the game status"

                [ test "Game has status InProgress if game is not over after move is made" <|
                    assertEqual InProgress
                        <| .status
                        <| makeMove (Coordinates 0 0)
                        <| GameState 3 False X [] InProgress

                , test "Game has status Tied if game is tied after move is made" <|
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
                            <| GameState 3 False X moves InProgress

                , test "Game has status Won X if player X won the game" <|
                    let
                        moves =
                            [ x 0 0, x 0 1
                            , o 1 0, o 1 1
                            ]
                    in
                        assertEqual (Won X)
                            <| .status
                            <| makeMove (Coordinates 0 2)
                            <| GameState 3 False X moves InProgress

                , test "Game has status Won O if player O won the game" <|
                    let
                        moves =
                            [ o 0 0, o 0 1
                            , x 1 0, x 1 1
                            ]
                    in
                        assertEqual (Won O)
                            <| .status
                            <| makeMove (Coordinates 0 2)
                            <| GameState 3 False O moves InProgress
                ]
            , suite "Getting the next player"

                [ test "Given X, the next player is O" <|
                    assertEqual X (nextPlayer O)

                , test "Given O, the next player is X" <|
                    assertEqual O (nextPlayer X)
                ]
            ]
        ]
