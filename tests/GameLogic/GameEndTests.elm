module GameLogic.GameEndTests (all) where

import ElmTest exposing (Test, assert, assertEqual, suite, test)

import GameModel exposing (GameState, Move, Player (X, O), Status (InProgress, Tied, Won))
import TestHelpers exposing (x, o)

import GameLogic.GameEnd exposing (isGameOver, winningPlayer)


all : Test
all =
    suite "Handling the end of the game"

        [ suite "Checking if the game is over"

            [ test "Returns true if a player has won" <|
                let
                    moves =
                        [ x 0 0, x 0 1, x 0 2
                        , o 1 0, o 1 1
                        ]
                    gameState = GameState 3 O moves (Won X)
                in
                    assertEqual True (isGameOver gameState)

            , test "Returns true if the game ended in a tie" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0, o 2 1, x 2 2
                        ]
                in
                    assertEqual True (isGameOver (GameState 3 O moves Tied))

            , test "Returns false if no moves have been made" <|
                assertEqual False (isGameOver (GameState 3 X [] InProgress))

            , test "Returns false if the game has not been won or tied" <|
                let
                    moves =
                        [ x 0 0, o 0 1, x 0 2
                        , o 1 0, x 1 1
                        ]
                    gameState = GameState 3 O moves InProgress
                in
                    assertEqual False (isGameOver (GameState 3 O moves InProgress))

            , test "Result does not depend on game status" <|
                let
                    moves =
                        [ x 0 0, o 0 1, x 0 2
                        , o 1 0, x 1 1
                        ]
                    gameStates =
                        List.map (GameState 3 O moves) [ InProgress, Tied, (Won X), (Won O) ]
                in
                    assert (List.all (not << isGameOver) gameStates)
            ]
        , suite "Getting the winning player"

            [ test "Returns nothing if the game is not over" <|
                assertEqual Nothing (winningPlayer (GameState 3 X [] InProgress))

            , test "Returns nothing if the game is tied" <|
                let
                    moves =
                        [ x 0 0, x 0 1, o 0 2
                        , o 1 0, o 1 1, x 1 2
                        , x 2 0, o 2 1, x 2 2
                        ]
                in
                    assertEqual Nothing (winningPlayer (GameState 3 X moves Tied))

            , test "Returns player X if they won the game" <|
                let
                    moves =
                        [ x 0 0, x 0 1, x 0 2
                        , o 1 0, o 1 1
                        ]
                in
                    assertEqual (Just X) (winningPlayer (GameState 3 O moves (Won X)))

            , test "Returns player O if they won the game" <|
                let
                    moves =
                        [ o 0 0, o 0 1, o 0 2
                        , x 1 0, x 1 1
                        ]
                in
                assertEqual (Just O) (winningPlayer (GameState 3 X moves (Won O)))

            , test "Result does not depend on game status" <|
                let
                    moves =
                        [ x 0 0, o 0 1, x 0 2
                        , o 1 0, x 1 1
                        ]
                    gameStates =
                        List.map (GameState 3 O moves) [ InProgress, Tied, (Won X), (Won O) ]
                in
                    assert (List.all (\state -> (winningPlayer state) == Nothing) gameStates)

            ]
        ]
